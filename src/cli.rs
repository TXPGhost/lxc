use std::{fs::read_to_string, io::Write, path::PathBuf};

use clap::{Parser, Subcommand};
use colored::Colorize;

use crate::{
    ast,
    lexer::Lexer,
    parser::{self, ParseError},
    ssa::lowering::{Ctxt, Lower},
    style::*,
};

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    Repl,
    Run { file: PathBuf },
}

/// Runs the command line interface.
pub fn cli() {
    let args = Args::parse();
    match args.command {
        Command::Repl => repl(),
        Command::Run { file } => run(file),
    }
}

/// Program evaluation result.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EvalResult {
    /// Program evaluated successfully.
    Success,

    /// Ran out of tokens, program is incomplete.
    Incomplete,

    /// Fatal error occured during program evaluation.
    Failure,
}

/// Evaluates user input.
pub fn eval(user_input: &str, ctxt: &mut Ctxt) -> EvalResult {
    // Lexer step
    let mut lexer = match Lexer::new(user_input) {
        Ok(lexer) => lexer,
        Err(e) => {
            println!("{} lexer error: {e:?}", "==>".color(KWD));
            return EvalResult::Failure;
        }
    };

    // Parser step
    let program = match parser::parse_program(&mut lexer) {
        Ok(object) => {
            println!("{} {object}", "==>".color(KWD));
            object
        }
        Err(ParseError::OutOfTokens) => {
            return EvalResult::Incomplete;
        }
        Err(e) => {
            println!("{} parse error: {e:?}", "==>".color(KWD));
            return EvalResult::Failure;
        }
    };

    // AST lowering
    let ast = ast::Object::from(program);

    // SSA lowering
    let ssa = ast.lower(ctxt).unwrap();
    println!("\n{} {}", "ENTRY".color(KWD), ssa);
    println!("{}", ctxt.prog());

    EvalResult::Success
}

/// Runs the interactive shell.
pub fn repl() {
    let mut user_input = String::new();
    let stdin = std::io::stdin();
    let mut stdout = std::io::stdout();
    loop {
        // Gather user input
        let nbyte = stdin
            .read_line(&mut user_input)
            .expect("couldn't read stdin");
        if nbyte == 0 {
            break;
        }
        let trimmed_input = user_input.trim();
        if trimmed_input.is_empty() || trimmed_input.ends_with(';') {
            continue;
        };

        let mut ctxt = Ctxt::new();
        match eval(&user_input, &mut ctxt) {
            EvalResult::Success | EvalResult::Failure => user_input.clear(),
            EvalResult::Incomplete => {
                print!("  ");
                stdout.flush().unwrap();
            }
        }
    }
}

/// Runs the file at the given path.
pub fn run(file: PathBuf) {
    let mut ctxt = Ctxt::new();
    eval(
        &read_to_string(file).expect("unable to read file"),
        &mut ctxt,
    );
}
