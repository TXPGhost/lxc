#![allow(missing_docs)]

pub mod from_ptree;

pub use crate::ptree::{Ident, IdentKind, Param, Visibility};

#[derive(Debug)]
pub enum Expr {
    Ident(Ident),
    Lit(Lit),
    Call(Call),
    Constructor(Constructor),
    Proj(Proj),
    Object(Object),
    Array(Array),
    Vector(Vector),
}

#[derive(Debug)]
pub enum Lit {
    String(String),
    I64(i64),
}

#[derive(Debug)]
pub struct Array {
    pub exprs: Vec<Expr>,
}

#[derive(Debug)]
pub struct Vector {
    pub count: Option<Box<Expr>>,
    pub expr: Box<Expr>,
}

#[derive(Debug)]
pub struct Proj {
    pub object: Box<Expr>,
    pub ident: Ident,
}

#[derive(Debug)]
pub struct Func {
    pub params: Vec<Param>,
    pub body: Block,
}

#[derive(Debug)]
pub struct Call {
    pub func: Box<Expr>,
    pub args: Vec<Arg>,
}

#[derive(Debug)]
pub struct Method {
    pub is_mut: bool,
    pub func: Func,
}

#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug)]
pub enum Stmt {
    Decl(Decl),
    Assn(Assn),
    Return(Expr),
}

#[derive(Debug)]
pub struct Decl {
    pub ident: Ident,
    pub expr: Expr,
}

#[derive(Debug)]
pub struct Assn {
    pub ident: Ident,
    pub expr: Expr,
}

#[derive(Debug)]
pub struct Object {
    pub functions: Vec<Func>,
    pub fields: Vec<Field>,
    pub methods: Vec<Method>,
}

#[derive(Debug)]
pub struct Constructor {
    pub ty: Box<Expr>,
    pub args: Vec<Expr>,
}

#[derive(Debug)]
pub struct Field {
    pub visibility: Visibility,
    pub ident: Ident,
    pub ty: Expr,
}

#[derive(Debug)]
pub struct Arg {
    pub ident: Option<Ident>,
    pub is_mut: bool,
    pub expr: Expr,
}
