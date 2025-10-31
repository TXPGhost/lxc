/// Pretty prints the AST in a human-readable format.
pub mod pretty_print;

/// An expression.
#[derive(Debug)]
pub enum Expr {
    /// An identifier, e.g. `x` or `y`.
    Ident(Ident),

    /// A string literal, e.g. `"abcdef"`
    String(String),

    /// A constant 64-bit signed integer value, e.g. `42`.
    I64(i64),

    /// A binary operator expression, e.g. `x + y`.
    Infix(Infix),

    /// A function call expression, e.g. `fn(arg0, arg1, arg2)`.
    Call(Call),

    /// An object constructor, e.g. `Vector3{1.0, 2.0, 3.0}`.
    Constructor(Constructor),

    /// A field projection expression, e.g. `object.field`.
    Proj(Proj),

    /// An object definition expression.
    Object(Object),

    /// An array literal expression, e.g. `[1, 2, 3, 4, 5]`.
    Array(Array),

    /// A vector expression, e.g. `[5]I32` or `[]I32`.
    Vector(Vector),
}

/// An identifier, e.g. `value` or `Type` or `_builtin`.
#[derive(Debug)]
pub struct Ident {
    /// The identifier name.
    pub name: String,

    /// The identifier kind.
    pub kind: IdentKind,
}

impl Ident {
    /// Generates an instance of the void identifier.
    pub fn void() -> Self {
        Self {
            name: "_".to_owned(),
            kind: IdentKind::Void,
        }
    }
}

/// An identifier kind, based on capitalization of the first letter and/or a leading underscore.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IdentKind {
    /// A userland value identifier.
    Value,

    /// A userland type identifier.
    Type,

    /// A builtin value identifier, prefixed by `_` (e.g. `_add`).
    BuiltinValue,

    /// A builtin type identifier, prefixed by `_` (e.g. `_I32`).
    BuiltinType,

    /// The void identifier (`_`), which binds to nothing
    Void,
}

/// An array literal expression.
#[derive(Debug)]
pub struct Array {
    /// The list of expressions.
    pub exprs: Vec<Expr>,
}

/// An vector expression.
#[derive(Debug)]
pub struct Vector {
    /// The repeat count.
    pub count: Option<Box<Expr>>,

    /// The repeated expression/type.
    pub expr: Box<Expr>,
}

/// A field access expression.
#[derive(Debug)]
pub struct Proj {
    /// The object.
    pub object: Box<Expr>,

    /// The field identifier.
    pub ident: Ident,
}

/// An infix operator expression.
#[derive(Debug)]
pub struct Infix {
    /// The kind of binary operator.
    pub kind: InfixKind,

    /// The left-hand-side argument.
    pub lhs: Box<Expr>,

    /// The right-hand-side argument.
    pub rhs: Box<Expr>,
}

/// A kind of infix operator.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum InfixKind {
    /// Addition (e.g. `lhs + rhs`).
    Add,

    /// Subtraction (e.g. `lhs - rhs`).
    Sub,

    /// Multiplication (e.g. `lhs * rhs`).
    Mul,

    /// Division (e.g. `lhs / rhs`).
    Div,
}

/// A function, with parameters and a body.
#[derive(Debug)]
pub struct Func {
    /// The function's parameter list.
    pub params: Vec<Param>,

    /// The function body's basic block.
    pub body: Block,
}

/// A function call, with a function expression and a list of arguments.
#[derive(Debug)]
pub struct Call {
    /// The function to be called.
    pub func: Box<Expr>,

    /// The function's argument list.
    pub args: Vec<Arg>,
}

/// A method, a function that takes an implicit self receiver.
#[derive(Debug)]
pub struct Method {
    /// Indicates whether this method takes a mutable self receiver, marked with the `&`
    /// decorator and granting mutable access to instance variables.
    pub is_mut: bool,

    /// The associated function
    pub func: Func,
}

/// A basic block of code (e.g. a function body, if condition body, etc.).
#[derive(Debug)]
pub struct Block {
    /// The list of statements comprising this block.
    pub stmts: Vec<Stmt>,
}

/// A statement of code that can exist within a basic block.
#[derive(Debug)]
pub enum Stmt {
    /// A local variable declaration, e.g. `x = 10`.
    Decl(Ident, Expr),

    /// A local variable assignment, e.g. `x := 10`.
    Assn(Ident, Expr),

    /// A return statement, e.g. `<- 42`.
    Return(Expr),

    /// An expression statement, e.g. `obj.method()`
    Expr(Expr),
}

/// An object/struct with a list of named fields and associated methods.
#[derive(Debug)]
pub struct Object {
    /// A list of associated functions for this object. Often used for constructors, and don't take
    /// an implicit self receiver.
    pub functions: Vec<Func>,

    /// A list of fields for the object.
    pub fields: Vec<Field>,

    /// A list of associated methods for this object, which are functions that take a self
    /// receiver.
    pub methods: Vec<Method>,
}

/// An object constructor, coexpr: parse_expr(lexer)? nsisting of an object type and an initializer list.
#[derive(Debug)]
pub struct Constructor {
    /// The the object type being constructed, e.g. `Vector3`.
    pub ty: Box<Expr>,

    /// The initializer list for the constructor, e.g. `{1.0, 2.0, 3.0}`.
    pub args: Vec<Expr>,
}

/// A visibility modifier that can be applied to object fields.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Visibility {
    /// The default visibility modifier, which grants readonly access to the outside world while
    /// granting read/write access to the object internally.
    Default,

    /// The public visibility modifier, which grants read/write access to everyone, annoted with
    /// the `&` decorator (indicates that the field can be _mutated_ externally).
    Public,

    /// The private visibility modifier, which grants read/write access internally and hides the
    /// field from the outside world.
    Private,
}

/// An object field.
#[derive(Debug)]
pub struct Field {
    /// The field's visibility modifier.
    pub visibility: Visibility,

    /// The field's name, must be unique within the struct.
    pub ident: Ident,

    /// The field's type.
    pub ty: Expr,
}

/// An argument to a function.
#[derive(Debug)]
pub struct Arg {
    /// Optional argument name.
    pub ident: Option<Ident>,

    /// Indicates whether this argument is mutable. Mutable parameters are marked with the `&`
    /// decorator, indicating that the value may change from the caller's perspective as a result
    /// of calling this function.
    pub is_mut: bool,

    /// The argument expression.
    pub expr: Expr,
}

/// A paramter to a function.
#[derive(Debug)]
pub struct Param {
    /// Indicates whether this parameter is mutable. Mutable parameters are marked with the `&`
    /// decorator, indicating that the value may change from the caller's perspective as a result
    /// of calling this function.
    pub is_mut: bool,

    /// The parameter name identifier. Must be unique within the parameter list.
    pub ident: Ident,
}

impl Expr {
    /// Returns `true` if this expr/type should be passed by value (as opposed to being passed by
    /// reference). This is the case iff:
    ///
    /// 1. The type's computed size exceeds some constant amount
    /// 2. The type is recursive (necessitates some level of indirection)
    ///
    /// The language semantics don't change based on whether we pass by value or by reference. If
    /// we pass by value mutably, the new value is reflected in the function's return value and we
    /// generate code to assign the old value to the new value after the call site.
    pub fn pass_by_value(&self) -> bool {
        // NOT YET IMPLEMENTED (for now we pass everything by reference)
        false
    }
}
