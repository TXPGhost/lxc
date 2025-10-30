use std::fmt::Display;

use super::*;

fn comma_join(lhs: String, rhs: String) -> String {
    lhs + ", " + &rhs
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::LIdent(l) => write!(f, "{l}"),
            Expr::UIdent(u) => write!(f, "{u}"),
            Expr::String(s) => write!(f, "\"{s}\""),
            Expr::I64(i) => write!(f, "{i}"),
            Expr::Infix(i) => write!(f, "{i}"),
            Expr::Call(call) => write!(f, "{call}"),
            Expr::Constructor(c) => write!(f, "{c}"),
            Expr::Proj(p) => write!(f, "{p}"),
            Expr::Object(o) => write!(f, "{o}"),
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{{{}}}",
            std::iter::empty()
                .chain(self.functions.iter().map(Func::to_string))
                .chain(self.fields.iter().map(Field::to_string))
                .chain(self.methods.iter().map(Method::to_string))
                .reduce(comma_join)
                .unwrap_or_default()
        )
    }
}

impl Display for Func {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({}) -> {}",
            self.params
                .iter()
                .map(Param::to_string)
                .reduce(comma_join)
                .unwrap_or_default(),
            self.body
        )
    }
}

impl Display for Field {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}: {}", self.visibility, self.ident, self.ty)
    }
}

impl Display for Visibility {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Visibility::Default => write!(f, ""),
            Visibility::Public => write!(f, "&"),
            Visibility::Private => write!(f, "-"),
        }
    }
}

impl Display for Method {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", if self.is_mut { "&" } else { "" }, self.func)
    }
}

impl Display for Param {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", if self.is_mut { "&" } else { "" }, self.name)
    }
}

impl Display for Proj {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}", self.object, self.field)
    }
}

impl Display for Infix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {} {})", self.lhs, self.kind, self.rhs)
    }
}

impl Display for InfixKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InfixKind::Add => write!(f, "+"),
            InfixKind::Sub => write!(f, "-"),
            InfixKind::Mul => write!(f, "*"),
            InfixKind::Div => write!(f, "/"),
        }
    }
}

impl Display for Constructor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{{{}}}",
            self.ty,
            self.args
                .iter()
                .map(Expr::to_string)
                .reduce(comma_join)
                .unwrap_or_default()
        )
    }
}

impl Display for Call {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}({})",
            self.func,
            self.args
                .iter()
                .map(Arg::to_string)
                .reduce(comma_join)
                .unwrap_or_default()
        )
    }
}

impl Display for Arg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}{}",
            self.ident
                .as_ref()
                .map(|i| format!("{i}: "))
                .unwrap_or_default(),
            if self.is_mut { "&" } else { "" },
            self.expr
        )
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // TODO
        write!(f, "TODO")
    }
}
