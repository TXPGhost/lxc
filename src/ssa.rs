//! Single static assignment form. At this stage we reduce the AST to a flat structure of global
//! declarations. Identifiers get unique names which correspond to the path taken to access them
//! (e.g. `x@main` for a local variable within the `main` function). We also introduce names like
//! `_v42@main` for local variables, or `_T6` for type declarations. When resolving identifiers we
//! will first look for an exact match, then remove suffixes until we find something that works. In
//! this way, locally-scoped values shadow their global counterparts.

use indexmap::IndexMap;

pub use crate::ast::{Ident, Lit};

/// Lowering the AST into SSA form.
pub mod lowering;

/// Pretty printing of SSA form.
pub mod pretty_print;

/// A program in its entirety.
#[derive(Debug)]
pub struct Prog {
    /// A series of global definitions.
    pub globals: IndexMap<Ident, Global>,
}

/// A function definition.
#[derive(Debug)]
pub struct Func {
    /// The list of function parameters.
    pub params: Vec<Param>,

    /// The list of function body statements.
    pub stmts: Vec<Stmt>,

    /// The optional function return identifier.
    pub ret: Option<Ident>,
}

/// A function parameter.
#[derive(Debug)]
pub struct Param {
    /// The parameter name.
    pub ident: Ident,

    /// The parameter type.
    pub ty: Ident,
}

/// A statement within a function body.
#[derive(Debug)]
pub enum Stmt {
    /// A statement of the form `ident = func(args...)`, calling some function.
    Call(Call),

    /// A statement of the form `ident = ident`, assigning an identified value.
    Decl(Decl),

    /// A statement of the form `ident = LIT`, assigning a constant value.
    Const(Const),

    /// A return statement of the form `ident`.
    Return(Ident),
}

/// A function call statement.
#[derive(Debug)]
pub struct Call {
    /// The resulting value identifier.
    pub ident: Ident,

    /// The function to call's identifier.
    pub func: Ident,

    /// The list of argument identifiers.
    pub args: Vec<Ident>,
}

/// A declaration statement.
#[derive(Debug)]
pub struct Decl {
    /// The resulting identifier.
    pub lhs: Ident,

    /// The identifier being assigned to.
    pub rhs: Ident,
}

/// A constant value statement.
#[derive(Debug)]
pub struct Const {
    /// The resulting value identifier.
    pub ident: Ident,

    /// The constant value literal.
    pub lit: Lit,
}

/// A global definition.
#[derive(Debug)]
pub enum Global {
    /// A literal type (e.g. `4`).
    Lit(Lit),

    /// A function definition.
    Func(Func),

    /// An object definition.
    Object(Object),
}

/// An object definition.
#[derive(Debug)]
pub struct Object {
    /// The list of object fields (also global identifiers).
    pub fields: Vec<(Ident, Ident)>,
}
