use super::*;
use crate::ptree;

impl From<ptree::Expr> for Expr {
    fn from(expr: ptree::Expr) -> Self {
        match expr {
            ptree::Expr::Ident(i) => Expr::Ident(i),
            ptree::Expr::String(s) => Expr::Lit(Lit::String(s)),
            ptree::Expr::Integer(i) => Expr::Lit(Lit::Integer(i)),
            ptree::Expr::Float(f) => Expr::Lit(Lit::Float(f)),
            ptree::Expr::Infix(i) => Expr::Call(Call::from(i)),
            ptree::Expr::Call(c) => Expr::Call(Call::from(c)),
            ptree::Expr::Func(_) => todo!("from_ptree func expr"),
            ptree::Expr::Block(_) => todo!("from_ptree block expr"),
            ptree::Expr::Proj(p) => Expr::Proj(Proj::from(p)),
            ptree::Expr::Object(o) => Expr::Object(Object::from(o)),
            ptree::Expr::Array(a) => Expr::Array(Array::from(a)),
            ptree::Expr::Vector(v) => Expr::Vector(Vector::from(v)),
            ptree::Expr::Paren(e) => Expr::from(*e.expr),
            ptree::Expr::Return(_) => todo!("from_ptree return expr"),
            ptree::Expr::If(_) => todo!("from_ptree if expr"),
        }
    }
}

impl From<ptree::Vector> for Vector {
    fn from(vector: ptree::Vector) -> Vector {
        Vector {
            count: vector.count.map(|count| Box::new(Expr::from(*count))),
            expr: Box::new(Expr::from(*vector.expr)),
        }
    }
}

impl From<ptree::Array> for Array {
    fn from(array: ptree::Array) -> Array {
        Array {
            exprs: array.exprs.into_iter().map(Expr::from).collect(),
        }
    }
}

impl From<ptree::Method> for Method {
    fn from(method: ptree::Method) -> Method {
        Method {
            is_mut: method.is_mut,
            func: Func::from(method.func),
        }
    }
}

impl From<ptree::Field> for Field {
    fn from(field: ptree::Field) -> Field {
        Field {
            visibility: field.visibility,
            ident: field.ident,
            ty: Expr::from(field.ty),
        }
    }
}

impl From<ptree::Stmt> for Stmt {
    fn from(stmt: ptree::Stmt) -> Stmt {
        match stmt {
            ptree::Stmt::Decl(ident, expr) => Stmt::Decl(Decl {
                ident,
                expr: Expr::from(expr),
            }),
            ptree::Stmt::Assn(ident, expr) => Stmt::Assn(Assn {
                ident,
                expr: Expr::from(expr),
            }),
            ptree::Stmt::Expr(expr) => Stmt::Assn(Assn {
                ident: Ident::void(),
                expr: Expr::from(expr),
            }),
        }
    }
}

impl From<ptree::Block> for Block {
    fn from(block: ptree::Block) -> Block {
        Block {
            stmts: block.stmts.into_iter().map(Stmt::from).collect(),
        }
    }
}

impl From<ptree::Func> for Func {
    fn from(func: ptree::Func) -> Func {
        Func {
            params: Object::from(func.params),
            ty: func.ty.map(|ty| Box::new(Expr::from(*ty))),
            body: func.body.map(Block::from),
        }
    }
}

impl From<ptree::Object> for Object {
    fn from(object: ptree::Object) -> Object {
        Object {
            functions: object.functions.into_iter().map(Func::from).collect(),
            fields: object.fields.into_iter().map(Field::from).collect(),
            methods: object.methods.into_iter().map(Method::from).collect(),
        }
    }
}

impl From<ptree::Proj> for Proj {
    fn from(proj: ptree::Proj) -> Proj {
        Proj {
            object: Box::new(Expr::from(*proj.object)),
            ident: proj.ident,
        }
    }
}

impl From<ptree::Infix> for Call {
    fn from(infix: ptree::Infix) -> Self {
        let name = match infix.kind {
            ptree::InfixKind::Add => "_add".to_owned(),
            ptree::InfixKind::Sub => "_sub".to_owned(),
            ptree::InfixKind::Mul => "_mul".to_owned(),
            ptree::InfixKind::Div => "_div".to_owned(),
            ptree::InfixKind::Eq => "_eq".to_owned(),
            ptree::InfixKind::Ne => "_ne".to_owned(),
        };
        Call {
            func: Box::new(Expr::Ident(Ident {
                name,
                kind: IdentKind::BuiltinValue,
            })),
            args: vec![
                Arg {
                    ident: None,
                    is_mut: false,
                    expr: Expr::from(*infix.lhs),
                },
                Arg {
                    ident: None,
                    is_mut: false,
                    expr: Expr::from(*infix.rhs),
                },
            ],
        }
    }
}

impl From<ptree::Call> for Call {
    fn from(call: ptree::Call) -> Self {
        Call {
            func: Box::new(Expr::from(*call.func)),
            args: call.args.into_iter().map(Arg::from).collect(),
        }
    }
}

impl From<ptree::Arg> for Arg {
    fn from(arg: ptree::Arg) -> Self {
        Arg {
            ident: arg.ident,
            is_mut: arg.is_mut,
            expr: Expr::from(arg.expr),
        }
    }
}

impl From<ptree::Constructor> for Constructor {
    fn from(constructor: ptree::Constructor) -> Self {
        Constructor {
            ty: Box::new(Expr::from(*constructor.ty)),
            args: constructor.args.into_iter().map(Expr::from).collect(),
        }
    }
}
