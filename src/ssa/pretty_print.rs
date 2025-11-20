use colored::Colorize;
use std::fmt::{self, Display};

use super::*;
use crate::style::*;

const PADDING: usize = 24;
const INDENT: usize = 4;

/// Custom trait for pretty printing with program context.
pub trait PrettyPrintSsa {
    /// Pretty prints the given SSA type with program context.
    fn pretty_print(&self, f: &mut fmt::Formatter<'_>, prog: &Prog) -> fmt::Result;

    /// Wraps this SSA type into a printable struct with program context (via [Display]).
    fn printable_ssa<'a>(&'a self, prog: &'a Prog) -> PrintableSsa<'a, Self>
    where
        Self: Sized,
    {
        PrintableSsa { value: self, prog }
    }
}

/// Makes an SSA node printable (via [Display]) by attaching it to program context.
pub struct PrintableSsa<'a, T> {
    value: &'a T,
    prog: &'a Prog,
}

impl<'a, T: PrettyPrintSsa> Display for PrintableSsa<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.value.pretty_print(f, self.prog)
    }
}

impl PrettyPrintSsa for Prog {
    fn pretty_print(&self, f: &mut std::fmt::Formatter<'_>, prog: &Prog) -> std::fmt::Result {
        for (ident, global) in &self.globals {
            match global {
                Global::Lit(_) => write!(f, "{} ", "LIT".color(KWD))?,
                Global::Func(_) => write!(f, "\n{} ", "FUN".color(KWD))?,
                Global::Object(_) => write!(f, "{} ", "OBJ".color(KWD))?,
                Global::Stmt(_) => continue, // will be printed within the body
            };
            writeln!(
                f,
                "{}{} {}{}",
                ident.printable_ssa(prog),
                ":".color(PNC),
                " ".repeat((PADDING as isize - 6 - ident.name.len() as isize).max(0) as usize),
                global.printable_ssa(prog)
            )?;
            if matches!(global, Global::Func(_)) {
                writeln!(f)?;
            }
        }
        Ok(())
    }
}

impl PrettyPrintSsa for Global {
    fn pretty_print(&self, f: &mut std::fmt::Formatter<'_>, prog: &Prog) -> std::fmt::Result {
        match self {
            Global::Lit(lit) => lit.pretty_print(f, prog),
            Global::Func(func) => func.pretty_print(f, prog),
            Global::Object(object) => object.pretty_print(f, prog),
            Global::Stmt(stmt) => stmt.pretty_print(f, prog),
        }
    }
}

impl PrettyPrintSsa for Lit {
    fn pretty_print(&self, f: &mut std::fmt::Formatter<'_>, _prog: &Prog) -> std::fmt::Result {
        match self {
            Lit::String(s) => write!(f, "{}", format!("\"{s}\"").color(LIT)),
            Lit::Integer(i) => write!(f, "{}", i.color(LIT)),
            Lit::Float(n) => write!(f, "{}", n.color(LIT)),
        }
    }
}

impl PrettyPrintSsa for Func {
    fn pretty_print(&self, f: &mut std::fmt::Formatter<'_>, prog: &Prog) -> std::fmt::Result {
        write!(f, "{}", "(".color(OPR))?;
        write!(
            f,
            "{}",
            self.params
                .iter()
                .map(|param| param.printable_ssa(prog).to_string())
                .reduce(comma_join)
                .unwrap_or_default()
        )?;
        writeln!(f, "{}", ")".color(OPR))?;
        write!(
            f,
            "{}",
            self.stmts
                .iter()
                .map(|ident| prog
                    .globals
                    .get(ident)
                    .map(|stmt| format!(
                        "{}{} {} {}",
                        " ".repeat(PADDING + INDENT),
                        ident.printable_ssa(prog),
                        "=".color(PNC),
                        stmt.printable_ssa(prog)
                    ))
                    .unwrap_or_else(|| "<???>".to_owned()))
                .reduce(newline_join)
                .unwrap_or_default()
        )?;
        if let Some(ret) = &self.ret {
            write!(
                f,
                "\n{}{}",
                " ".repeat(PADDING + INDENT),
                ret.printable_ssa(prog)
            )?;
        }
        Ok(())
    }
}

impl PrettyPrintSsa for Object {
    fn pretty_print(&self, f: &mut std::fmt::Formatter<'_>, prog: &Prog) -> std::fmt::Result {
        write!(f, "{}", "(".color(PNC))?;
        write!(
            f,
            "{}",
            self.fields
                .iter()
                .map(|(fid, tid)| format!(
                    "{}{} {}",
                    fid.printable_ssa(prog),
                    ":".color(PNC),
                    tid.printable_ssa(prog),
                ))
                .reduce(comma_join)
                .unwrap_or_default()
        )?;
        write!(f, "{}", ")".color(PNC))?;
        Ok(())
    }
}

impl PrettyPrintSsa for Param {
    fn pretty_print(&self, f: &mut std::fmt::Formatter<'_>, prog: &Prog) -> std::fmt::Result {
        write!(
            f,
            "{}{} {}",
            self.ident.printable_ssa(prog),
            ":".color(PNC),
            self.ty.printable_ssa(prog),
        )
    }
}

impl PrettyPrintSsa for Ident {
    fn pretty_print(&self, f: &mut std::fmt::Formatter<'_>, _prog: &Prog) -> std::fmt::Result {
        match self.kind {
            crate::ptree::IdentKind::BuiltinValue => {
                write!(f, "{}", self.name.color(KWD))
            }
            crate::ptree::IdentKind::BuiltinType => {
                write!(f, "{}", self.name.color(KWD).bold())
            }
            crate::ptree::IdentKind::Value => write!(f, "{}", self.name.color(IDT)),
            crate::ptree::IdentKind::Type => write!(f, "{}", self.name.color(TYP).bold()),
            crate::ptree::IdentKind::Void => write!(f, "{}", self.name.color(PNC)),
        }
    }
}

impl PrettyPrintSsa for Stmt {
    fn pretty_print(&self, f: &mut std::fmt::Formatter<'_>, prog: &Prog) -> std::fmt::Result {
        match self {
            Stmt::Call(c) => write!(
                f,
                "{}{}{}{}",
                c.func.printable_ssa(prog),
                "(".color(OPR),
                c.args
                    .iter()
                    .map(|ident| ident.printable_ssa(prog).to_string())
                    .reduce(comma_join)
                    .unwrap_or_default(),
                ")".color(OPR),
            )?,
            Stmt::Decl(d) => write!(f, "{}", d.printable_ssa(prog))?,
        }
        Ok(())
    }
}
