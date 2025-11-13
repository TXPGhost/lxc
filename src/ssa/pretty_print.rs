use std::fmt::Display;

use colored::Colorize;

use super::*;
use crate::style::*;

impl Display for Prog {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (ident, global) in &self.globals {
            writeln!(
                f,
                "{ident}{} {}{global}",
                ":".color(PNC),
                " ".repeat((15 - ident.name.len()).max(0))
            )?;
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
        write!(f, "{} {}", "FUN".color(KWD), "(".color(OPR))?;
        write!(
            f,
            "{}",
            self.params
                .iter()
                .map(Param::to_string)
                .reduce(comma_join)
                .unwrap_or_default()
        )?;
        writeln!(f, "{}", ")".color(OPR))?;
        write!(
            f,
            "{}",
            self.stmts
                .iter()
                .map(Stmt::to_string)
                .reduce(newline_join)
                .unwrap_or_default()
        )?;
        Ok(())
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", "OBJ".color(KWD), "(".color(PNC))?;
        write!(
            f,
            "{}",
            self.fields
                .iter()
                .map(|(fid, tid)| format!("{fid}: {tid}"))
                .reduce(comma_join)
                .unwrap_or_default()
        )?;
        write!(f, "{}", ")".color(PNC))?;
        Ok(())
    }
}

impl Display for Param {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{} {}", self.ident, ":".color(PNC), self.ty)
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "    ")?;
        match self {
            Stmt::Call(c) => write!(
                f,
                "{} = {}{}{}{}",
                c.ident,
                c.func,
                "(".color(OPR),
                c.args
                    .iter()
                    .map(Ident::to_string)
                    .reduce(comma_join)
                    .unwrap_or_default(),
                ")".color(OPR),
            )?,
            Stmt::Decl(d) => write!(f, "{} = {}", d.lhs, d.rhs)?,
            Stmt::Const(c) => write!(f, "{} = {}", c.ident, c.lit)?,
        }
        Ok(())
    }
}
