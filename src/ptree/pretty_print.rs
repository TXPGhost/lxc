use std::fmt::{self, Display};

use colored::Colorize;

use super::*;
use crate::style::*;

/// Context for pretty printing (e.g. indent level, etc.).
#[derive(Clone, Copy, Debug, Default)]
pub struct PrettyPrintCtxt {}

/// Custom trait for pretty printing with context.
pub trait PrettyPrint {
    /// Pretty prints the given ptree type with some context.
    fn pretty_print(&self, f: &mut fmt::Formatter<'_>, ppc: PrettyPrintCtxt) -> fmt::Result;

    /// Wraps this ptree type into a printable struct with pretty print context (via [Display]).
    fn printable(&self, ctxt: PrettyPrintCtxt) -> Printable<Self>
    where
        Self: Sized,
    {
        Printable { value: self, ctxt }
    }
}

/// Makes a parse tree node printable (via [Display]) by attaching it to pretty print context.
pub struct Printable<'a, T> {
    value: &'a T,
    ctxt: PrettyPrintCtxt,
}

impl<'a, T: PrettyPrint> Display for Printable<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.value.pretty_print(f, self.ctxt)
    }
}

impl PrettyPrint for Expr {
    fn pretty_print(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        ppc: PrettyPrintCtxt,
    ) -> std::fmt::Result {
        match self {
            Expr::Ident(i) => i.pretty_print(f, ppc),
            Expr::String(s) => write!(f, "{}", format!("\"{s}\"").color(LIT)),
            Expr::Integer(n) | Expr::Float(n) => write!(f, "{}", n.color(LIT)),
            Expr::Infix(i) => i.pretty_print(f, ppc),
            Expr::Call(c) => c.pretty_print(f, ppc),
            Expr::Func(u) => u.pretty_print(f, ppc),
            Expr::Block(b) => b.pretty_print(f, ppc),
            Expr::Proj(p) => p.pretty_print(f, ppc),
            Expr::Object(o) => <Object as PrettyPrint>::pretty_print(o, f, ppc),
            Expr::Array(a) => a.pretty_print(f, ppc),
            Expr::Vector(v) => v.pretty_print(f, ppc),
            Expr::Paren(p) => p.pretty_print(f, ppc),
            Expr::Return(r) => r.pretty_print(f, ppc),
            Expr::If(i) => i.pretty_print(f, ppc),
        }
    }
}

impl PrettyPrint for Array {
    fn pretty_print(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        ppc: PrettyPrintCtxt,
    ) -> std::fmt::Result {
        write!(
            f,
            "{}{}{}",
            "[".color(OPR),
            self.exprs
                .iter()
                .map(|expr| expr.printable(ppc).to_string())
                .reduce(comma_join)
                .unwrap_or_default(),
            "]".color(OPR),
        )
    }
}

impl PrettyPrint for Vector {
    fn pretty_print(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        ppc: PrettyPrintCtxt,
    ) -> std::fmt::Result {
        match &self.count {
            Some(count) => write!(
                f,
                "{}{}{}{}",
                "[".color(OPR),
                count.as_ref().printable(ppc),
                "]".color(OPR),
                self.expr.printable(ppc)
            ),
            None => write!(f, "{}{}", "[]".color(OPR), self.expr.printable(ppc)),
        }
    }
}

impl PrettyPrint for Object {
    fn pretty_print(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        ppc: PrettyPrintCtxt,
    ) -> std::fmt::Result {
        write!(
            f,
            "{}{}{}",
            "(".color(OPR),
            self.fields
                .iter()
                .map(|field| field.printable(ppc).to_string())
                .reduce(comma_join)
                .unwrap_or_default(),
            ")".color(OPR),
        )
    }
}

impl PrettyPrint for Func {
    fn pretty_print(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        ppc: PrettyPrintCtxt,
    ) -> std::fmt::Result {
        match (&self.ty, &self.body) {
            (Some(ty), None) => write!(
                f,
                "{} {} {}",
                self.params.printable(ppc),
                "->".color(PNC),
                ty.printable(ppc)
            ),
            (None, Some(body)) => {
                write!(f, "{} {}", self.params.printable(ppc), body.printable(ppc))
            }
            (Some(ty), Some(body)) => {
                write!(
                    f,
                    "{} {} {} {}",
                    self.params.printable(ppc),
                    "->".color(PNC),
                    ty.printable(ppc),
                    body.printable(ppc)
                )
            }
            (None, None) => unreachable!("parser shouldn't allow this"),
        }
    }
}

impl PrettyPrint for Field {
    fn pretty_print(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        ppc: PrettyPrintCtxt,
    ) -> std::fmt::Result {
        write!(
            f,
            "{}{}{}{}",
            self.decorator.printable(ppc),
            match (&self.ident.kind, &self.ty) {
                (IdentKind::Value, Expr::Func(_)) => format!("{}", self.ident.name.color(FUN)),
                _ => self.ident.printable(ppc).to_string(),
            },
            match self.ty {
                Expr::Func(_) => "",
                Expr::Object(_) => " ",
                _ => ": ",
            }
            .color(PNC),
            &self.ty.printable(ppc)
        )?;
        if let Some(default_value) = &self.default_value {
            write!(f, " {} {}", "=".color(PNC), default_value.printable(ppc))?;
        }
        Ok(())
    }
}

impl PrettyPrint for Decorator {
    fn pretty_print(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        _ppc: PrettyPrintCtxt,
    ) -> std::fmt::Result {
        match self {
            Decorator::Default => write!(f, ""),
            Decorator::Mutable => write!(f, "{}", "*".color(PNC)),
        }
    }
}

impl PrettyPrint for Method {
    fn pretty_print(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        ppc: PrettyPrintCtxt,
    ) -> std::fmt::Result {
        write!(
            f,
            "{}{}",
            if self.is_mut { "*" } else { "" }.color(PNC),
            &self.func.printable(ppc)
        )
    }
}

impl PrettyPrint for Param {
    fn pretty_print(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        ppc: PrettyPrintCtxt,
    ) -> std::fmt::Result {
        write!(
            f,
            "{}{}",
            if self.is_mut { "*" } else { "" }.color(PNC),
            self.ident.printable(ppc)
        )
    }
}

impl PrettyPrint for Proj {
    fn pretty_print(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        ppc: PrettyPrintCtxt,
    ) -> std::fmt::Result {
        if self.ident.kind == IdentKind::Value {
            write!(
                f,
                "{}.{}",
                self.object.printable(ppc),
                self.ident.name.color(MBR)
            )
        } else {
            write!(
                f,
                "{}.{}",
                self.object.printable(ppc),
                self.ident.printable(ppc)
            )
        }
    }
}

impl PrettyPrint for Infix {
    fn pretty_print(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        ppc: PrettyPrintCtxt,
    ) -> std::fmt::Result {
        let lhs_str = if let Expr::Infix(infix) = &*self.lhs {
            format!(
                "{}{}{}",
                "(".color(PNC),
                infix.printable(ppc),
                ")".color(PNC)
            )
        } else {
            self.lhs.printable(ppc).to_string()
        };
        let rhs_str = if let Expr::Infix(infix) = &*self.rhs {
            format!(
                "{}{}{}",
                "(".color(PNC),
                infix.printable(ppc),
                ")".color(PNC)
            )
        } else {
            self.rhs.printable(ppc).to_string()
        };
        write!(f, "{} {} {}", lhs_str, &self.kind.printable(ppc), rhs_str)
    }
}

impl PrettyPrint for InfixKind {
    fn pretty_print(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        _ppc: PrettyPrintCtxt,
    ) -> std::fmt::Result {
        match self {
            InfixKind::Add => write!(f, "{}", "+".color(OPR)),
            InfixKind::Sub => write!(f, "{}", "-".color(OPR)),
            InfixKind::Mul => write!(f, "{}", "*".color(OPR)),
            InfixKind::Div => write!(f, "{}", "/".color(OPR)),
            InfixKind::Eq => write!(f, "{}", "==".color(OPR)),
            InfixKind::Ne => write!(f, "{}", "!=".color(OPR)),
        }
    }
}

impl PrettyPrint for Constructor {
    fn pretty_print(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        ppc: PrettyPrintCtxt,
    ) -> std::fmt::Result {
        write!(
            f,
            "{}{}{}{}",
            self.ty.printable(ppc),
            "(".color(OPR),
            self.args
                .iter()
                .map(|arg| arg.printable(ppc).to_string())
                .reduce(comma_join)
                .unwrap_or_default(),
            ")".color(OPR),
        )
    }
}

impl PrettyPrint for Call {
    fn pretty_print(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        ppc: PrettyPrintCtxt,
    ) -> std::fmt::Result {
        let func_str = if let Expr::Ident(Ident {
            name,
            kind: IdentKind::Value,
        }) = &*self.func
        {
            format!("{}", name.color(FUN))
        } else if let Expr::Proj(Proj {
            object,
            ident:
                Ident {
                    name,
                    kind: IdentKind::Value,
                },
        }) = &*self.func
        {
            format!("{}.{}", object.printable(ppc), name.color(FUN))
        } else {
            self.func.printable(ppc).to_string()
        };
        write!(
            f,
            "{}{}{}{}",
            func_str,
            "(".color(OPR),
            self.args
                .iter()
                .map(|arg| arg.printable(ppc).to_string())
                .reduce(comma_join)
                .unwrap_or_default(),
            ")".color(OPR)
        )
    }
}

impl PrettyPrint for Arg {
    fn pretty_print(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        ppc: PrettyPrintCtxt,
    ) -> std::fmt::Result {
        write!(
            f,
            "{}{}{}",
            self.ident
                .as_ref()
                .map(|i| format!("{}: ", i.printable(ppc)))
                .unwrap_or_default(),
            if self.is_mut { "*" } else { "" }.color(PNC),
            &self.expr.printable(ppc)
        )
    }
}

impl PrettyPrint for Block {
    fn pretty_print(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        ppc: PrettyPrintCtxt,
    ) -> std::fmt::Result {
        // TODO
        write!(
            f,
            "{}{}{}",
            "{".color(OPR),
            self.stmts
                .iter()
                .map(|stmt| stmt.printable(ppc).to_string())
                .reduce(semicolon_join)
                .unwrap_or_default(),
            "}".color(OPR)
        )
    }
}

impl PrettyPrint for Stmt {
    fn pretty_print(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        ppc: PrettyPrintCtxt,
    ) -> std::fmt::Result {
        match self {
            Stmt::Decl(ident, expr) => write!(
                f,
                "{} {} {}",
                ident.printable(ppc),
                "=".color(PNC),
                expr.printable(ppc)
            ),
            Stmt::Assn(ident, expr) => write!(
                f,
                "{} {} {}",
                ident.printable(ppc),
                ":=".color(OPR),
                expr.printable(ppc)
            ),
            Stmt::Expr(expr) => write!(f, "{}", expr.printable(ppc)),
        }
    }
}

impl PrettyPrint for Ident {
    fn pretty_print(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        _ppc: PrettyPrintCtxt,
    ) -> std::fmt::Result {
        match self.kind {
            IdentKind::BuiltinValue => {
                write!(f, "{}", self.name.color(KWD))
            }
            IdentKind::BuiltinType => {
                write!(f, "{}", self.name.color(KWD).bold())
            }
            IdentKind::Value => write!(f, "{}", self.name.color(IDT)),
            IdentKind::Type => write!(f, "{}", self.name.color(TYP).bold()),
            IdentKind::Void => write!(f, "{}", self.name.color(PNC)),
        }
    }
}

impl PrettyPrint for Paren {
    fn pretty_print(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        ppc: PrettyPrintCtxt,
    ) -> std::fmt::Result {
        write!(
            f,
            "{}{}{}",
            "(".color(PNC),
            self.expr.printable(ppc),
            ")".color(PNC)
        )
    }
}

impl PrettyPrint for Return {
    fn pretty_print(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        ppc: PrettyPrintCtxt,
    ) -> std::fmt::Result {
        write!(f, "{} {}", "$".color(KWD).bold(), self.expr.printable(ppc))
    }
}

impl PrettyPrint for If {
    fn pretty_print(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        ppc: PrettyPrintCtxt,
    ) -> std::fmt::Result {
        match &self.else_body {
            Some(else_body) => write!(
                f,
                "{} {}{}{} {} {} {}",
                "?".color(KWD).bold(),
                "(".color(PNC),
                self.cond.printable(ppc),
                ")".color(PNC),
                self.if_body.printable(ppc),
                ":".color(KWD).bold(),
                else_body.as_ref().printable(ppc),
            ),
            None => write!(
                f,
                "{} {}{}{} {}",
                "?".color(KWD).bold(),
                "(".color(PNC),
                self.cond.printable(ppc),
                ")".color(PNC),
                self.if_body.printable(ppc)
            ),
        }
    }
}
