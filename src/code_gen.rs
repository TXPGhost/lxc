use std::collections::HashMap;

use crate::ast::{Ident, IdentKind};
use crate::ssa::lookup::LookupError;
use crate::ssa::type_checking::{Type, TypeLookupError};
use crate::ssa::{Func, Global, Prog, Stmt};

use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{AbiParam, Function, InstBuilder, Signature, UserFuncName};
use cranelift_codegen::ir::{Value, types::*};
use cranelift_codegen::isa::CallConv;
use cranelift_codegen::settings;
use cranelift_codegen::verifier::verify_function;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_module::{Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use thiserror::Error;

// TODO
// 1. compiling decls (using the type isn't quite right)
// 2. dead code elimination
// 3. compiling entire programs
// 4. some way to run the code
// 5. standard library and/or C ffi calls
// 6. objects

/// Code generation context.
pub struct CodeGenCtxt<'a> {
    /// The program currently being generated.
    pub prog: &'a Prog,

    /// Code generation settings,
    pub settings: CodeGenSettings,

    /// Mapping from identifiers to cranelift values.
    pub values: HashMap<Ident, Value>,
}

/// Code generation settings.
pub struct CodeGenSettings {
    /// Whether to emit a native binary,
    pub emit_bin: bool,
}

impl<'a> CodeGenCtxt<'a> {
    /// Starts a new code generation context from the given program.
    pub fn new(prog: &'a Prog, settings: CodeGenSettings) -> Self {
        assert!(prog.types.is_some());
        Self {
            prog,
            settings,
            values: HashMap::new(),
        }
    }

    /// Generates native code, consuming the context.
    pub fn generate(mut self, path: &str) -> Result<(), CodeGenError> {
        let flags = settings::Flags::new(settings::builder());
        let isa = cranelift_native::builder()
            .unwrap()
            .finish(cranelift_codegen::settings::Flags::new(
                cranelift_codegen::settings::builder(),
            ))
            .unwrap();

        // Build an ObjectModule to produce an object file
        let builder = ObjectBuilder::new(
            isa,
            "my_module".to_string(),
            cranelift_module::default_libcall_names(),
        )
        .unwrap();

        let mut module = ObjectModule::new(builder);
        let mut ctx = module.make_context();

        let base = self.prog.base();
        for (ident, field) in &base.fields {
            if let Global::Func(func) = self.prog.lookup(field).unwrap() {
                let func = func.code_gen(&mut self, &mut module)?;
                verify_function(&func, &flags).unwrap();

                ctx.func = func;

                let func_id = module
                    .declare_function(&ident.name, Linkage::Export, &ctx.func.signature)
                    .unwrap();
                module.define_function(func_id, &mut ctx).unwrap();
            }
        }

        let obj = module.finish();
        std::fs::write(path, obj.emit().unwrap()).unwrap();
        Ok(())
    }
}

/// An error that can occur during native code generation.
#[derive(Error, Debug)]
pub enum CodeGenError {
    /// Failed to look up some identifier.
    #[error("failed to look up identifier")]
    LookupError(#[from] LookupError),

    /// An illegal type was passed as a function argument.
    #[error("illegal argument type")]
    IllegalArgumentType(Type),

    /// An illegal constant type was provided.
    #[error("illegal constant")]
    IllegalConstant(Type),

    /// A type lookup failed.
    #[error("failed to lookup type")]
    TypeLookupError(#[from] TypeLookupError),
}

/// Used to generate code from SSA types.
pub trait CodeGen {
    /// The generated output type.
    type Output;

    /// Generates IR code from the given type with the provided context.
    fn code_gen(
        &self,
        ctxt: &mut CodeGenCtxt,
        module: &mut dyn Module,
    ) -> Result<Self::Output, CodeGenError>;
}

impl CodeGen for Func {
    type Output = Function;

    fn code_gen(
        &self,
        ctxt: &mut CodeGenCtxt,
        module: &mut dyn Module,
    ) -> Result<Self::Output, CodeGenError> {
        let mut sig = Signature::new(CallConv::SystemV);
        let params: Result<Vec<(&Ident, AbiParam)>, CodeGenError> = self
            .params
            .iter()
            .map(|ident| {
                ctxt.prog
                    .types
                    .as_ref()
                    .unwrap()
                    .lookup(ident)
                    .map_err(CodeGenError::TypeLookupError)
                    .and_then(|ty| match compile_type_to_abi_param(ty) {
                        Ok(ty) => Ok((ident, ty)),
                        Err(e) => Err(e),
                    })
            })
            .collect();
        let params = params?;
        for (_, abi_param) in &params {
            sig.params.push(*abi_param);
        }
        if let Some(ret_id) = &self.ret {
            let ret_ty = ctxt.prog.types.as_ref().unwrap().lookup(ret_id)?;
            sig.returns.push(compile_type_to_abi_param(ret_ty)?);
        }
        let mut fn_builder_ctx = FunctionBuilderContext::new();
        let mut func = Function::with_name_signature(UserFuncName::user(0, 0), sig.clone()); // todo: change name?
        {
            let mut builder = FunctionBuilder::new(&mut func, &mut fn_builder_ctx);

            let block = builder.create_block();
            builder.append_block_params_for_function_params(block);

            builder.switch_to_block(block);
            builder.seal_block(block);

            // Create function parameter value associations in the entry block.
            #[allow(clippy::needless_range_loop)]
            for i in 0..params.len() {
                let value = builder.block_params(block)[i];
                ctxt.values.insert(params[i].0.clone(), value);
            }

            // Compile each statement.
            for ident in &self.stmts {
                let Global::Stmt(stmt) = ctxt.prog.lookup(ident)? else {
                    unreachable!("should always be a stmt");
                };
                match stmt {
                    Stmt::Decl(id) => {
                        let ty = ctxt.prog.types.as_ref().unwrap().lookup(id)?;
                        let res = compile_decl(ctxt, &mut builder, id, ty)?;
                        ctxt.values.insert(ident.clone(), res);
                    }
                    Stmt::Call(call) => match call.func.kind {
                        IdentKind::BuiltinValue => match call.func.name.as_str() {
                            "add" | "sub" | "mul" | "div" | "eq" | "ne" => {
                                let lhs = &call.args[0];
                                let rhs = &call.args[1];
                                let res = compile_binop(
                                    &mut builder,
                                    call.func.name.as_str(),
                                    lhs,
                                    rhs,
                                    ctxt,
                                );
                                ctxt.values.insert(ident.clone(), res);
                            }
                            _ => unimplemented!(),
                        },
                        IdentKind::Value => {
                            let args: Vec<Value> = call
                                .args
                                .iter()
                                .map(|arg| {
                                    *ctxt.values.get(arg).unwrap_or_else(|| {
                                        panic!("value lookup failed: {}", arg.name)
                                    })
                                })
                                .collect();

                            let callee = module
                                .declare_function(&call.func.name, Linkage::Import, &sig)
                                .expect("problem declaring function");
                            let callee = module.declare_func_in_func(callee, builder.func);

                            builder.ins().call(callee, &args);
                        }
                        _ => todo!("type functions"),
                    },
                }
            }

            if let Some(ret_id) = &self.ret {
                let ret_val = match ctxt.values.get(ret_id) {
                    Some(value) => *value,
                    None => compile_decl(
                        ctxt,
                        &mut builder,
                        ret_id,
                        ctxt.prog.types.as_ref().unwrap().lookup(ret_id)?,
                    )?,
                };
                builder.ins().return_(&[ret_val]);
            } else {
                builder.ins().return_(&[]);
            }

            builder.finalize();
        }

        Ok(func)
    }
}

fn compile_type_to_abi_param(ty: Type) -> Result<AbiParam, CodeGenError> {
    match ty {
        Type::I64 => Ok(AbiParam::new(I64)),
        Type::F64 => Ok(AbiParam::new(F64)),
        Type::ConstI64(_) => Ok(AbiParam::new(I64)), // TEMPORARY
        Type::ConstF64(_) => Ok(AbiParam::new(F64)), // TEMPORARY
        Type::Bool => Ok(AbiParam::new(I8)),
        Type::ConstBool(_) => Ok(AbiParam::new(I8)), // TEMPORARY
        ty => Err(CodeGenError::IllegalArgumentType(ty)),
    }
}

fn compile_decl(
    ctxt: &CodeGenCtxt,
    builder: &mut FunctionBuilder,
    id: &Ident,
    ty: Type,
) -> Result<Value, CodeGenError> {
    Ok(match ty {
        Type::ConstI64(i) => builder.ins().iconst(I64, i),
        Type::ConstF64(f) => builder.ins().f64const(f),
        Type::ConstBool(b) => builder.ins().iconst(I8, b as i64),
        _ => *ctxt.values.get(id).expect("unresolved value"),
    })
}

fn compile_binop(
    builder: &mut FunctionBuilder,
    op: &str,
    lhs: &Ident,
    rhs: &Ident,
    ctxt: &mut CodeGenCtxt,
) -> Value {
    let lhs = *ctxt
        .values
        .get(lhs)
        .unwrap_or_else(|| panic!("unable to find lhs {} of binop", lhs.name));
    let rhs = *ctxt
        .values
        .get(rhs)
        .unwrap_or_else(|| panic!("unable to find rhs {} of binop", rhs.name));
    match op {
        "add" => builder.ins().iadd(lhs, rhs),
        "sub" => builder.ins().isub(lhs, rhs),
        "mul" => builder.ins().imul(lhs, rhs),
        "div" => builder.ins().sdiv(lhs, rhs),
        "eq" => builder.ins().icmp(IntCC::Equal, lhs, rhs),
        "ne" => builder.ins().icmp(IntCC::NotEqual, lhs, rhs),
        _ => unimplemented!(),
    }
}
