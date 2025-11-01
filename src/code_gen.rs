use crate::ptree::*;

use inkwell::OptimizationLevel;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::values::{AnyValue, AnyValueEnum, IntValue};

/// Context that allows us to build up LLVM types.
pub struct LLvmCtxt<'ctx> {
    /// The LLVM context.
    pub context: &'ctx Context,

    /// The LLVM module currently being worked on.
    pub module: Module<'ctx>,

    /// The LLVM builder.
    pub builder: Builder<'ctx>,

    /// The LLVM execution engine.
    pub execution_engine: ExecutionEngine<'ctx>,
}

/// Trait that indicates a type can be used to generate LLVM code.
pub trait CodeGen {
    /// The output type representing the LLVM code.
    type Output<'ctx>
    where
        Self: 'ctx;

    /// Generates LLVM code for this type.
    fn code_gen<'ctx>(&'ctx self, ctxt: &'ctx LLvmCtxt<'ctx>) -> Self::Output<'ctx>;
}

impl CodeGen for Expr {
    type Output<'ctx> = AnyValueEnum<'ctx>;
    fn code_gen<'ctx>(&'ctx self, ctxt: &'ctx LLvmCtxt<'ctx>) -> Self::Output<'ctx> {
        match self {
            // Expr::Ident(_) => todo!(),
            // Expr::Integer(v) => ctxt
            //     .context
            //     .i64_type()
            //     .const_int(*v as u64, false)
            //     .as_any_value_enum(),
            // Expr::Infix(infix) => {
            //     let lhs = infix.lhs.code_gen(ctxt).into_int_value();
            //     let rhs = infix.rhs.code_gen(ctxt).into_int_value();
            //
            //     match infix.kind {
            //         InfixKind::Add => ctxt
            //             .builder
            //             .build_int_add(lhs, rhs, "add_result")
            //             .unwrap()
            //             .as_any_value_enum(),
            //         InfixKind::Sub => ctxt
            //             .builder
            //             .build_int_sub(lhs, rhs, "sub_result")
            //             .unwrap()
            //             .as_any_value_enum(),
            //         InfixKind::Mul => ctxt
            //             .builder
            //             .build_int_mul(lhs, rhs, "mul_result")
            //             .unwrap()
            //             .as_any_value_enum(),
            //         InfixKind::Div => ctxt
            //             .builder
            //             .build_int_signed_div(lhs, rhs, "div_result")
            //             .unwrap()
            //             .as_any_value_enum(),
            //     }
            // }
            // Expr::Constructor(constructor) => todo!(),
            _ => todo!(),
        }
    }
}
