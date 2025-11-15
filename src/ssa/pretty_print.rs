use colored::Colorize;
use std::fmt::Display;

use super::*;
use crate::style::*;

const PADDING: usize = 24;
const INDENT: usize = 4;

impl Display for Prog {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (ident, global) in &self.globals {
            match global {
                Global::Lit(_) => write!(f, "{} ", "LIT".color(KWD))?,
                Global::Func(_) => write!(f, "\n{} ", "FUN".color(KWD))?,
                Global::Object(_) => write!(f, "{} ", "OBJ".color(KWD))?,
            };
            writeln!(
                f,
                "{ident}{} {}{global}",
                ":".color(PNC),
                " ".repeat((PADDING as isize - 6 - ident.name.len() as isize).max(0) as usize)
            )?;
            if matches!(global, Global::Func(_)) {
                writeln!(f)?;
            }
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
            Lit::String(s) => write!(f, "{}", format!("\"{s}\"").color(LIT)),
            Lit::Integer(i) => write!(f, "{}", i.color(LIT)),
            Lit::Float(n) => write!(f, "{}", n.color(LIT)),
        }
    }
}

impl Display for Func {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", "(".color(OPR))?;
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
        write!(f, "{}", "(".color(PNC))?;
        write!(
            f,
            "{}",
            self.fields
                .iter()
                .map(|(fid, tid)| format!("{fid}{} {tid}", ":".color(PNC)))
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
        write!(f, "{}", " ".repeat(PADDING + INDENT))?;
        match self {
            Stmt::Call(c) => write!(
                f,
                "{} {} {}{}{}{}",
                c.ident,
                "=".color(PNC),
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
            Stmt::Return(e) => write!(f, "{e}")?,
        }
        Ok(())
    }
}
