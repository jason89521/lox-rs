use std::fs;
use std::path::PathBuf;
use std::process::exit;

use clap::{Parser, Subcommand};
use codecrafters_interpreter::*;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Tokenize the source code
    Tokenize {
        /// The lox file
        filename: PathBuf,
    },

    /// Parse the source code
    Parse {
        // The lox file
        filename: PathBuf,
    },

    Evaluate {
        filename: PathBuf,
    },

    Run {
        filename: PathBuf,
    },
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    match &cli.command {
        Commands::Tokenize { filename } => {
            let file_contents = read_file(filename);
            let mut lexer = Lexer::new(&file_contents);
            for token in &mut lexer {
                match token {
                    Ok(token) => println!("{}", token),
                    Err(e) => {
                        eprintln!("[line {}] Error: {}", e.line(), e)
                    }
                }
            }

            if lexer.has_error() {
                exit(65)
            }
        }
        Commands::Parse { filename } => {
            let file_contents = read_file(filename);
            let mut parser = codecrafters_interpreter::Parser::new(&file_contents);
            match parser.parse_expr(0) {
                Ok(tree) => {
                    println!("{tree}");
                }
                Err(e) => {
                    eprintln!("{e}");
                    exit(65)
                }
            }
        }
        Commands::Evaluate { filename } => {
            let file_contents = read_file(filename);
            let mut interpreter = Interpreter::new(&file_contents);
            match interpreter.evaluate() {
                Ok(_) => {
                    //
                }
                Err(e) => {
                    eprintln!("{e}");
                    if let Some(_) = e.downcast_ref::<RuntimeError>() {
                        exit(70)
                    } else {
                        exit(65)
                    }
                }
            }
        }
        Commands::Run { filename } => {
            let file_contents = read_file(filename);
            let mut parser = codecrafters_interpreter::Parser::new(&file_contents);
            match parser.parse() {
                Ok(ast) => {
                    Interpreter::eval(ast)?;
                }
                Err(e) => {
                    eprintln!("{e}");
                    if let Some(_) = e.downcast_ref::<RuntimeError>() {
                        exit(70)
                    } else {
                        exit(65)
                    }
                }
            }
        }
    }

    Ok(())
}

fn read_file(filename: &PathBuf) -> String {
    fs::read_to_string(filename).unwrap_or_else(|_| {
        eprintln!("Failed to read file {:?}", filename);
        String::new()
    })
}
