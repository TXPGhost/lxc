use std::{io::Write, path::PathBuf};

use clap::{Parser, Subcommand};

use crate::{
    lexer::Lexer,
    parser::{self, ParseError},
    ptree::Expr,
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

/// Runs the interactive shell.
pub fn repl() {
    let mut user_input = String::new();
    let stdin = std::io::stdin();
    loop {
        let nbyte = stdin
            .read_line(&mut user_input)
            .expect("couldn't read stdin");
        if nbyte == 0 {
            break;
        }

        if user_input.trim().is_empty() {
            continue;
        };
        let mut lexer = match Lexer::new(&user_input) {
            Ok(lexer) => lexer,
            Err(e) => {
                println!("lexer error: {e:?}",);
                continue;
            }
        };
        match parser::parse_program(&mut lexer) {
            Ok(ptree) => println!(
                "[{}]",
                ptree
                    .iter()
                    .map(Expr::to_string)
                    .reduce(|acc, s| acc + ", " + &s)
                    .unwrap_or_default()
            ),
            Err(ParseError::OutOfTokens) => {
                continue;
            }
            Err(e) => println!("parse error: {e:?}"),
        }

        user_input.clear();
    }
}

/// Runs the file at the given path.
pub fn run(_: PathBuf) {
    todo!("haven't implemented run yet")
}
