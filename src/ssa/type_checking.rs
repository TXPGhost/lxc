use std::{
    collections::HashMap,
    num::{ParseFloatError, ParseIntError},
};

use thiserror::Error;

use crate::{ast::IdentKind, ssa::lookup::LookupError};

use super::*;

/// A type-checked type.
#[derive(Debug, Clone)]
pub enum Type {
    /// 64-bit signed integer
    I64,

    /// 64-bit floating point
    F64,

    /// 64-bit signed integer constant
    ConstI64(i64),

    /// 64-bit floating point constant (never NaN or infinity)
    ConstF64(f64),

    /// Boolean
    Bool,

    /// Cosntant boolean
    ConstBool(bool),

    /// Object type
    Object(IndexMap<Ident, Type>),

    /// Function type (argument type, return type)
    Func(Box<Type>, Box<Type>),

    /// Void type
    Void,
}

impl Type {
    /// Returns [true] if this type is a subtype of [other].
    pub fn subtype_of(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::I64, Type::I64) => true,
            (Type::F64, Type::F64) => true,
            (Type::ConstI64(_), Type::I64) => true,
            (Type::ConstF64(_), Type::F64) => true,
            (Type::ConstI64(lhs), Type::ConstI64(rhs)) => lhs == rhs,
            (Type::ConstF64(lhs), Type::ConstF64(rhs)) => lhs == rhs,
            (Type::Object(lhs), Type::Object(rhs)) => {
                for (id, lhs_ty) in lhs {
                    match rhs.get(id) {
                        Some(rhs_ty) => {
                            if !lhs_ty.subtype_of(rhs_ty) {
                                return false;
                            }
                        }
                        None => return false,
                    }
                }
                true
            }
            (Type::Func(lhs_arg, lhs_ret), Type::Func(rhs_arg, rhs_ret)) => {
                // contravariance
                lhs_ret.subtype_of(rhs_ret) && lhs_arg.supertype_of(rhs_arg)
            }
            _ => false,
        }
    }

    /// Returns [true] if this type is a subtype of [other].
    pub fn supertype_of(&self, other: &Self) -> bool {
        other.subtype_of(self)
    }
}

/// Mapping from globally-unique identifiers to types.
#[derive(Debug)]
pub struct Types {
    /// The identifier-type mapping.
    pub types: HashMap<Ident, Type>,
}

/// Type lookup failure.
#[derive(Debug, Error)]
#[error("failed to resolve type")]
pub struct TypeLookupError(Ident);

impl Types {
    /// Constructs a new, empty type mapping.
    pub fn new() -> Self {
        Self {
            types: HashMap::new(),
        }
    }

    /// Assigns a type mapping and also returns the assigned type for convenience.
    pub fn assign(&mut self, ident: &Ident, ty: Type) -> Type {
        self.types.insert(ident.clone(), ty.clone());
        ty
    }

    /// Looks up the type of an identifier.
    pub fn lookup(&self, ident: &Ident) -> Result<Type, TypeLookupError> {
        self.types
            .get(ident)
            .cloned()
            .ok_or_else(|| TypeLookupError(ident.clone()))
    }
}

impl Default for Types {
    fn default() -> Self {
        Self::new()
    }
}

/// An error that can occur during type checking.
#[derive(Debug, Error)]
pub enum TypeCheckError {
    /// An error that occurs parsing an integer.
    #[error("failed to parse integer")]
    ParseIntError(#[from] ParseIntError),

    /// An error that occurs parsing a float.
    #[error("failed to parse float")]
    ParseFloatError(#[from] ParseFloatError),

    /// An illegal float value was provided.
    #[error("illegal float (NaN or infinity)")]
    IllegalF64(f64),

    /// An error that occurs during identifier lookup.
    #[error("identifier lookup failed")]
    LookupError(#[from] LookupError),

    /// An error that occurs during type lookup.
    #[error("type lookup failed")]
    TypeLookupError(#[from] TypeLookupError),

    /// Too many arguments provided to a function.
    #[error("too many function arguments")]
    TooManyFunctionArguments,

    /// Too few arguments provided to a function.
    #[error("too few function arguments")]
    TooFewFunctionArguments,

    /// A function type was expected.
    #[error("expected a function type")]
    ExpectedFunctionType {
        /// The found type.
        found: Box<Type>,
    },

    /// An illegal function argument type was provided.
    #[error("illegal argument type")]
    IllegalArgumentType {
        /// The expected type.
        expected: Box<Type>,

        /// The found type.
        found: Box<Type>,
    },
}

impl Prog {
    /// Performs type checking on the given value, also adding types to the global mapping.
    pub fn type_check(&self, ident: &Ident, types: &mut Types) -> Result<Type, TypeCheckError> {
        let global = self.lookup(ident)?;
        let ty = match global {
            Global::Lit(lit) => typecheck_lit(lit)?,
            Global::Func(func) => {
                let mut tys = IndexMap::new();
                for param in &func.params {
                    let ty = self.type_check(param, types)?;
                    tys.insert(param.clone(), ty);
                }
                let arg_ty = Type::Object(tys);
                for stmt in &func.stmts {
                    self.type_check(stmt, types)?;
                }

                let ret_ty = match &func.ret {
                    Some(ret) => self.type_check(ret, types)?,
                    None => Type::Void,
                };

                Type::Func(Box::new(arg_ty), Box::new(ret_ty))
            }
            Global::Object(object) => {
                let mut tys = IndexMap::new();
                for (fid, id) in &object.fields {
                    let ty = self.type_check(id, types)?;
                    tys.insert(fid.clone(), types.assign(fid, ty));
                }
                types.assign(ident, Type::Object(tys))
            }
            Global::Stmt(stmt) => {
                let ty = self.typecheck_stmt(stmt, types)?;
                types.assign(ident, ty)
            }
            Global::Param(param) => {
                let ty = self.type_check(param, types)?;
                types.assign(param, ty)
            }
        };
        Ok(types.assign(ident, ty))
    }

    fn typecheck_stmt(&self, stmt: &Stmt, types: &mut Types) -> Result<Type, TypeCheckError> {
        match stmt {
            Stmt::Call(c) => {
                if c.func.kind == IdentKind::BuiltinValue {
                    return self.typecheck_builtin_func(c, types);
                }
                let func_ty = self.type_check(&c.func, types)?;
                let Type::Func(arg_ty, ret_ty) = func_ty else {
                    return Err(TypeCheckError::ExpectedFunctionType {
                        found: Box::new(func_ty),
                    });
                };
                let Type::Object(arg_ty) = *arg_ty else {
                    unreachable!("function argument type is always an object type")
                };
                for i in 0..c.args.len() {
                    let arg = &c.args[i];
                    let ty = self.type_check(arg, types)?;
                    if i >= arg_ty.len() {
                        return Err(TypeCheckError::TooManyFunctionArguments);
                    }
                    if !ty.subtype_of(&arg_ty[i]) {
                        return Err(TypeCheckError::IllegalArgumentType {
                            expected: Box::new(arg_ty[i].clone()),
                            found: Box::new(ty),
                        });
                    }
                }
                if c.args.len() < arg_ty.len() {
                    return Err(TypeCheckError::TooFewFunctionArguments);
                }
                Ok(*ret_ty)
            }
            Stmt::Decl(decl) => Ok(self.type_check(decl, types)?),
        }
    }

    fn typecheck_builtin_func(
        &self,
        call: &Call,
        types: &mut Types,
    ) -> Result<Type, TypeCheckError> {
        let func = call.func.name.as_str();
        match func {
            "add" | "sub" | "mul" | "div" | "eq" | "ne" => {
                if call.args.len() != 2 {
                    unreachable!("binop should always have two arguments");
                }
                let lhs = &call.args[0];
                let rhs = &call.args[1];

                self.typecheck_binop(func, lhs, rhs, types)
            }
            _ => unimplemented!(),
        }
    }

    fn typecheck_binop(
        &self,
        op: &str,
        lhs: &Ident,
        rhs: &Ident,
        types: &mut Types,
    ) -> Result<Type, TypeCheckError> {
        let lhs = self.type_check(lhs, types)?;
        let rhs = self.type_check(rhs, types)?;
        match (lhs, rhs) {
            // Constant propagation
            (Type::ConstI64(lhs), Type::ConstI64(rhs)) => match op {
                "add" => Ok(Type::ConstI64(lhs + rhs)),
                "sub" => Ok(Type::ConstI64(lhs - rhs)),
                "mul" => Ok(Type::ConstI64(lhs * rhs)),
                "div" => Ok(Type::ConstI64(lhs / rhs)),
                "eq" => Ok(Type::ConstBool(lhs == rhs)),
                "ne" => Ok(Type::ConstBool(lhs != rhs)),
                _ => unimplemented!(),
            },
            (Type::ConstF64(lhs), Type::ConstF64(rhs)) => match op {
                "add" => Ok(Type::ConstF64(lhs + rhs)),
                "sub" => Ok(Type::ConstF64(lhs - rhs)),
                "mul" => Ok(Type::ConstF64(lhs * rhs)),
                "div" => Ok(Type::ConstF64(lhs / rhs)),
                "eq" => Ok(Type::ConstBool(lhs == rhs)),
                "ne" => Ok(Type::ConstBool(lhs != rhs)),
                _ => unimplemented!(),
            },
            (Type::ConstBool(lhs), Type::ConstBool(rhs)) => match op {
                "eq" => Ok(Type::ConstBool(lhs == rhs)),
                "ne" => Ok(Type::ConstBool(lhs != rhs)),
                _ => unimplemented!(),
            },

            // Builtin types where at least one value is unknown
            (Type::I64 | Type::ConstI64(_), Type::I64 | Type::ConstI64(_)) => match op {
                "add" | "sub" | "mul" | "div" => Ok(Type::I64),
                "eq" | "ne" => Ok(Type::Bool),
                _ => unimplemented!(),
            },
            (Type::F64 | Type::ConstF64(_), Type::F64 | Type::ConstF64(_)) => match op {
                "add" | "sub" | "mul" | "div" => Ok(Type::F64),
                "eq" | "ne" => Ok(Type::Bool),
                _ => unimplemented!(),
            },
            (Type::Bool | Type::ConstBool(_), Type::Bool | Type::ConstBool(_)) => Ok(Type::Bool),
            _ => unimplemented!(),
        }
    }
}

fn typecheck_lit(lit: &Lit) -> Result<Type, TypeCheckError> {
    match lit {
        Lit::String(_) => todo!("implement strings"),
        Lit::Integer(i) => match i.parse::<i64>() {
            Ok(i) => Ok(Type::ConstI64(i)),
            Err(e) => Err(TypeCheckError::ParseIntError(e)),
        },
        Lit::Float(f) => match f.parse::<f64>() {
            Ok(f) if f.is_finite() => Ok(Type::ConstF64(f)),
            Ok(f) => Err(TypeCheckError::IllegalF64(f)),
            Err(e) => Err(TypeCheckError::ParseFloatError(e)),
        },
        Lit::True => Ok(Type::ConstBool(true)),
        Lit::False => Ok(Type::ConstBool(false)),
        Lit::I64T => Ok(Type::I64),
        Lit::F64T => Ok(Type::F64),
        Lit::BoolT => Ok(Type::Bool),
    }
}
