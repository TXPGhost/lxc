use std::fmt::Display;

use super::*;
use crate::style::*;

impl Display for Prog {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (ident, global) in &self.globals {
            writeln!(f, "{ident} ==> {global}")?;
        }
        Ok(())
    }
}

impl Display for Global {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Global::Lit(lit) => write!(f, "{lit}"),
            Global::Func(func) => write!(f, "{func}"),
            Global::Object(object) => write!(f, "{object}"),
        }
    }
}

impl Display for Lit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Lit::String(s) => write!(f, "{s}"),
            Lit::Integer(i) => write!(f, "{i}"),
            Lit::Float(n) => write!(f, "{n}"),
        }
    }
}

impl Display for Func {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        write!(
            f,
            "{}",
            self.params
                .iter()
                .map(Param::to_string)
                .reduce(comma_join)
                .unwrap_or_default()
        )?;
        writeln!(f, ") -> {{")?;
        write!(
            f,
            "{}",
            self.stmts
                .iter()
                .map(Stmt::to_string)
                .reduce(newline_join)
                .unwrap_or_default()
        )?;
        write!(f, "\n}}")?;
        Ok(())
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        write!(
            f,
            "{}",
            self.fields
                .iter()
                .map(Ident::to_string)
                .reduce(comma_join)
                .unwrap_or_default()
        )?;
        write!(f, ")")?;
        Ok(())
    }
}

impl Display for Param {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.ident, self.ty)
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "    ")?;
        match self {
            Stmt::Call(c) => write!(
                f,
                "{} = {}({})",
                c.ident,
                c.func,
                c.args
                    .iter()
                    .map(Ident::to_string)
                    .reduce(comma_join)
                    .unwrap_or_default()
            )?,
            Stmt::Decl(d) => write!(f, "{} = {}", d.lhs, d.rhs)?,
            Stmt::Const(c) => write!(f, "{} = {}", c.ident, c.lit)?,
        }
        Ok(())
    }
}
