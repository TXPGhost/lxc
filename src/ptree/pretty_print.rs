use std::fmt::Display;

use colored::{Color, Colorize};

use super::*;

const fn color(r: u8, g: u8, b: u8) -> Color {
    Color::TrueColor { r, g, b }
}

// Pretty Printing Colors
static IDT: Color = color(209, 209, 209);
static OPR: Color = color(182, 182, 181);
static PNC: Color = color(172, 172, 172);
static LIT: Color = color(134, 175, 154);
static KWD: Color = color(149, 179, 209);
static FUN: Color = color(248, 198, 153);
static TYP: Color = color(238, 168, 107);
static MBR: Color = color(179, 194, 209);

/// Joins two strings with a comma and a space.
fn comma_join(lhs: String, rhs: String) -> String {
    format!("{}{}{}", lhs, ", ".color(PNC), rhs)
}

/// Joins two strings with a semicolon and a space.
fn semicolon_join(lhs: String, rhs: String) -> String {
    format!("{}{}{}", lhs, "; ".color(PNC), rhs)
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Ident(i) => write!(f, "{i}"),
            Expr::String(s) => write!(f, "{}", format!("\"{s}\"").color(LIT)),
            Expr::Integer(n) | Expr::Float(n) => write!(f, "{}", n.color(LIT)),
            Expr::Infix(i) => write!(f, "{i}"),
            Expr::Call(c) => write!(f, "{c}"),
            Expr::Func(u) => write!(f, "{u}"),
            Expr::Block(b) => write!(f, "{b}"),
            Expr::Proj(p) => write!(f, "{p}"),
            Expr::Object(o) => write!(f, "{o}"),
            Expr::Array(a) => write!(f, "{a}"),
            Expr::Vector(v) => write!(f, "{v}"),
            Expr::Paren(p) => write!(f, "{p}"),
            Expr::Return(r) => write!(f, "{r}"),
            Expr::If(i) => write!(f, "{i}"),
        }
    }
}

impl Display for Array {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}{}",
            "[".color(OPR),
            self.exprs
                .iter()
                .map(Expr::to_string)
                .reduce(comma_join)
                .unwrap_or_default(),
            "]".color(OPR),
        )
    }
}

impl Display for Vector {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.count {
            Some(count) => write!(
                f,
                "{}{}{}{}",
                "[".color(OPR),
                count,
                "]".color(OPR),
                self.expr
            ),
            None => write!(f, "{}{}", "[]".color(OPR), self.expr),
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}{}",
            "(".color(OPR),
            self.fields
                .iter()
                .map(Field::to_string)
                .reduce(comma_join)
                .unwrap_or_default(),
            ")".color(OPR),
        )
    }
}

impl Display for Func {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match (&self.ty, &self.body) {
            (Some(ty), None) => write!(f, "{} -> {}", self.params, ty),
            (None, Some(body)) => write!(f, "{} {}", self.params, body),
            (Some(ty), Some(body)) => write!(f, "{} -> {} {}", self.params, ty, body),
            (None, None) => unreachable!(),
        }
    }
}

impl Display for Field {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}{}{}",
            self.decorator,
            match (&self.ident.kind, &self.ty) {
                (IdentKind::Value, Expr::Func(_)) => format!("{}", self.ident.name.color(FUN)),
                _ => format!("{}", self.ident),
            },
            match self.ty {
                Expr::Func(_) => "",
                Expr::Object(_) => " ",
                _ => ": ",
            }
            .color(PNC),
            self.ty
        )?;
        if let Some(default_value) = &self.default_value {
            write!(f, " {} {}", "=".color(PNC), default_value)?;
        }
        Ok(())
    }
}

impl Display for Decorator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Decorator::Default => write!(f, ""),
            Decorator::Mutable => write!(f, "{}", "*".color(PNC)),
        }
    }
}

impl Display for Method {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}",
            if self.is_mut { "*" } else { "" }.color(PNC),
            self.func
        )
    }
}

impl Display for Param {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}",
            if self.is_mut { "*" } else { "" }.color(PNC),
            self.ident
        )
    }
}

impl Display for Proj {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.ident.kind == IdentKind::Value {
            write!(f, "{}.{}", self.object, self.ident.name.color(MBR))
        } else {
            write!(f, "{}.{}", self.object, self.ident)
        }
    }
}

impl Display for Infix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let lhs_str = if let Expr::Infix(infix) = &*self.lhs {
            format!("{}{}{}", "(".color(PNC), infix, ")".color(PNC))
        } else {
            self.lhs.to_string()
        };
        let rhs_str = if let Expr::Infix(infix) = &*self.rhs {
            format!("{}{}{}", "(".color(PNC), infix, ")".color(PNC))
        } else {
            self.rhs.to_string()
        };
        write!(f, "{} {} {}", lhs_str, self.kind, rhs_str)
    }
}

impl Display for InfixKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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

impl Display for Constructor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}{}{}",
            self.ty,
            "(".color(OPR),
            self.args
                .iter()
                .map(Expr::to_string)
                .reduce(comma_join)
                .unwrap_or_default(),
            ")".color(OPR),
        )
    }
}

impl Display for Call {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
            format!("{}.{}", object, name.color(FUN))
        } else {
            self.func.to_string()
        };
        write!(
            f,
            "{}{}{}{}",
            func_str,
            "(".color(OPR),
            self.args
                .iter()
                .map(Arg::to_string)
                .reduce(comma_join)
                .unwrap_or_default(),
            ")".color(OPR)
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
            if self.is_mut { "*" } else { "" }.color(PNC),
            self.expr
        )
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // TODO
        write!(
            f,
            "{}{}{}",
            "{".color(OPR),
            self.stmts
                .iter()
                .map(Stmt::to_string)
                .reduce(semicolon_join)
                .unwrap_or_default(),
            "}".color(OPR)
        )
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Decl(ident, expr) => write!(f, "{} {} {}", ident, "=".color(PNC), expr),
            Stmt::Assn(ident, expr) => write!(f, "{} {} {}", ident, ":=".color(OPR), expr),
            Stmt::Expr(expr) => write!(f, "{expr}"),
        }
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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

impl Display for Paren {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}{}", "(".color(PNC), self.expr, ")".color(PNC))
    }
}

impl Display for Return {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", "$".color(KWD).bold(), self.expr,)
    }
}

impl Display for If {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.else_body {
            Some(else_body) => write!(
                f,
                "{} {}{}{} {} {} {}",
                "?".color(KWD).bold(),
                "(".color(PNC),
                self.cond,
                ")".color(PNC),
                self.if_body,
                ":".color(KWD).bold(),
                else_body,
            ),
            None => write!(
                f,
                "{} {}{}{} {}",
                "?".color(KWD).bold(),
                "(".color(PNC),
                self.cond,
                ")".color(PNC),
                self.if_body
            ),
        }
    }
}
