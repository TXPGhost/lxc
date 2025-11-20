use std::{fs::read_to_string, io::Write, path::PathBuf};

use clap::{Parser, Subcommand};
use colored::Colorize;

use crate::{
    ast::{self, Ident, IdentKind},
    lexer::Lexer,
    parser::{self, ParseError},
    ptree::pretty_print::{PrettyPrint, PrettyPrintCtxt},
    ssa::{
        Global,
        lowering::{Ctxt, Lower},
        pretty_print::PrettyPrintSsa,
        type_checking::Types,
    },
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
            println!("{} lexer error: {e:?}", "ERR".color(KWD));
            return EvalResult::Failure;
        }
    };

    // Parser step
    let program = match parser::parse_program(&mut lexer) {
        Ok(object) => {
            println!("\n{}", "-- Abstract Syntax Tree".color(PNC));
            println!(
                "{} {}",
                "AST".color(KWD),
                object.printable(PrettyPrintCtxt::default())
            );
            object
        }
        Err(ParseError::OutOfTokens) => {
            return EvalResult::Incomplete;
        }
        Err(e) => {
            println!("{} parse error: {e:?}", "ERR".color(KWD));
            return EvalResult::Failure;
        }
    };

    // AST lowering
    let ast = ast::Object::from(program);

    // SSA lowering
    let ssa = ast.lower(ctxt).unwrap();
    println!("\n{}", "-- Single Static Assignment".color(PNC));
    println!(
        "{} {}\n",
        "ENTRY".color(KWD),
        ssa.printable(PrettyPrintCtxt::default())
    );
    println!("{}", ctxt.prog().printable_ssa(ctxt.prog()));

    // Main function resolution
    let entry_id = Ident {
        name: "_T1".to_owned(),
        kind: IdentKind::Type,
    };
    let entry = ctxt.prog().globals.get(&entry_id).unwrap();
    let Global::Object(entry) = entry else {
        unreachable!();
    };
    let mut main = None;
    for (ident, field) in &entry.fields {
        if ident.name == "main" {
            main = Some(field);
            break;
        }
    }

    match main {
        Some(main) => println!(
            "{} {}",
            "-- Found main function at".color(PNC),
            main.name.color(PNC)
        ),
        None => println!("{}", "-- Did not find main function".color(PNC)),
    }

    // Type checking
    let mut types = Types::new();
    match ctxt.prog().type_check(&entry_id, &mut types) {
        Ok(_) => {}
        Err(e) => eprintln!("{} type error: {e:?}", "ERR".color(KWD)),
    }
    dbg!(&types);

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
