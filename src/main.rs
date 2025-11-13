#![warn(missing_docs)]

//! The Lx Programming Language Compiler

/// The lexer based on the [logos] crate.
pub mod lexer;

/// A hand-written pratt parser.
pub mod parser;

/// The parse tree emitted from the parser.
pub mod ptree;

/// The abstract syntax tree, a simplified form of the parse tree.
pub mod ast;

/// The program represented in SSA form, useful for analysis.
pub mod ssa;

/// Native code generation.
pub mod code_gen;

/// The command-line interface.
pub mod cli;

/// Pretty printing styles.
pub mod style;

fn main() {
    cli::cli();
}
