//! Single static assignment form. At this stage we reduce the AST to a flat structure of global
//! declarations. Identifiers get unique names which correspond to the path taken to access them
//! (e.g. `x@main` for a local variable within the `main` function). We also introduce names like
//! `_v42@main` for local variables, or `_T6` for type declarations. When resolving identifiers we
//! will first look for an exact match, then remove suffixes until we find something that works. In
//! this way, locally-scoped values shadow their global counterparts.

use indexmap::IndexMap;

pub use crate::ast::{Ident, Lit};
use crate::ssa::type_checking::Types;

/// Lowering the AST into SSA form.
pub mod lowering;

/// Pretty printing of SSA form.
pub mod pretty_print;

/// Semantic identifier lookup.
pub mod lookup;

/// Type checking pass.
pub mod type_checking;

/// A program in its entirety.
#[derive(Debug)]
pub struct Prog {
    /// A series of global definitions.
    pub globals: IndexMap<Ident, Global>,

    /// Cache of statement identifiers to functions and statement indexes.
    pub cache: IndexMap<Ident, (Ident, usize)>,

    /// Type associations,
    pub types: Option<Types>,
}

/// A function definition.
#[derive(Debug)]
pub struct Func {
    /// The list of function parameters.
    pub params: Vec<Param>,

    /// The list of function body statements (points into global definitions).
    pub stmts: Vec<Ident>,

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
    Decl(Ident),
}

/// A function call statement.
#[derive(Debug)]
pub struct Call {
    /// The function to call's identifier.
    pub func: Ident,

    /// The list of argument identifiers.
    pub args: Vec<Ident>,
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

    /// A statement.
    Stmt(Stmt),
}

/// An object definition.
#[derive(Debug)]
pub struct Object {
    /// The list of object fields (also global identifiers).
    pub fields: Vec<(Ident, Ident)>,
}
