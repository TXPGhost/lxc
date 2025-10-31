use std::{fmt::Display, sync::LazyLock};

use colored::{Color, Colorize};

use super::*;

const fn color(r: u8, g: u8, b: u8) -> Color {
    Color::TrueColor { r, g, b }
}

static IDT: Color = color(209, 209, 209);
static OPR: Color = color(182, 182, 181);
static PNC: Color = color(154, 154, 154);
static LIT: Color = color(134, 175, 154);
static KWD: Color = color(149, 179, 209);
static FUN: Color = color(248, 198, 153);
static TYP: Color = color(238, 168, 107);
static MBR: Color = color(179, 194, 209);

fn comma_join(lhs: String, rhs: String) -> String {
    format!("{}{}{}", lhs, ", ".color(OPR), rhs)
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Ident(i) => write!(f, "{i}"),
            Expr::String(s) => write!(f, "{}", format!("\"{s}\"").color(LIT)),
            Expr::I64(i) => write!(f, "{}", i.to_string().color(LIT)),
            Expr::Infix(i) => write!(f, "{i}"),
            Expr::Call(call) => write!(f, "{call}"),
            Expr::Constructor(c) => write!(f, "{c}"),
            Expr::Proj(p) => write!(f, "{p}"),
            Expr::Object(o) => write!(f, "{o}"),
            Expr::Array(a) => write!(f, "{a}"),
            Expr::Vector(v) => write!(f, "{v}"),
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
            "{".color(OPR),
            std::iter::empty()
                .chain(self.functions.iter().map(Func::to_string))
                .chain(self.fields.iter().map(Field::to_string))
                .chain(self.methods.iter().map(Method::to_string))
                .reduce(comma_join)
                .unwrap_or_default(),
            "}".color(OPR),
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
        write!(f, "{}.{}", self.object, self.ident)
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
            "{}{}{}{}",
            self.ty,
            "{".color(OPR),
            self.args
                .iter()
                .map(Expr::to_string)
                .reduce(comma_join)
                .unwrap_or_default(),
            "}".color(OPR),
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
        write!(f, "TODO")
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
        }
    }
}
