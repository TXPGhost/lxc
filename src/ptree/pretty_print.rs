use std::fmt::Display;

use super::*;

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::LIdent(l) => write!(f, "{l}"),
            Expr::UIdent(u) => write!(f, "{u}"),
            Expr::String(s) => write!(f, "\"{s}\""),
            Expr::I64(i) => write!(f, "{i}"),
            Expr::Add(lhs, rhs) => write!(f, "Add({lhs}, {rhs})"),
            Expr::Call(call) => write!(f, "{call}"),
            Expr::Constructor(c) => write!(f, "{c}"),
        }
    }
}

impl Display for Constructor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{{{}}}",
            self.ident,
            self.args
                .iter()
                .map(Expr::to_string)
                .reduce(|acc, s| acc + ", " + &s)
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
                .reduce(|acc, s| acc + ", " + &s)
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
