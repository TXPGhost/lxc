#![warn(missing_docs)]

//! The Lx Programming Language Compiler

/// The lexer based on the [logos] crate.
pub mod lexer;

/// A hand-written pratt parser.
pub mod parser;

/// LLVM code generation.
pub mod code_gen;

/// The parse tree emitted from the parser.
pub mod ptree;

/// The command-line interface.
pub mod cli;

fn main() {
    cli::cli();
}
