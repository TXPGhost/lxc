#![warn(missing_docs)]

//! The Lx Programming Language Compiler

/// The lexer.
pub mod lexer;

/// The parser.
pub mod parser;

/// Code generation.
pub mod code_gen;

/// The parse tree.
pub mod ptree;

/// The command-line interface.
pub mod cli;

fn main() {
    cli::cli();
}
