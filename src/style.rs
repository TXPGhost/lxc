use colored::{Color, Colorize};

const fn color(r: u8, g: u8, b: u8) -> Color {
    Color::TrueColor { r, g, b }
}

/// The color for identifiers.
pub static IDT: Color = color(209, 209, 209);

/// The color for operators.
pub static OPR: Color = color(182, 182, 181);

/// The color for punctuation.
pub static PNC: Color = color(172, 172, 172);

/// The color for literals.
pub static LIT: Color = color(134, 175, 154);

/// The color for keyword operators.
pub static KWD: Color = color(149, 179, 209);

/// The color for functions.
pub static FUN: Color = color(248, 198, 153);

/// The color for types.
pub static TYP: Color = color(238, 168, 107);

/// The color for members.
pub static MBR: Color = color(179, 194, 209);

/// Joins two strings with a comma and a space.
pub fn comma_join(lhs: String, rhs: String) -> String {
    format!("{}{}{}", lhs, ", ".color(PNC), rhs)
}

/// Joins two strings with a semicolon and a space.
pub fn semicolon_join(lhs: String, rhs: String) -> String {
    format!("{}{}{}", lhs, "; ".color(PNC), rhs)
}

/// Joins two strings with a newline.
pub fn newline_join(lhs: String, rhs: String) -> String {
    format!("{lhs}\n{rhs}")
}
