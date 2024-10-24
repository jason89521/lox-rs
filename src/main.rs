use std::fs;
use std::path::PathBuf;
use std::process::exit;

use clap::{Parser, Subcommand};

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
}

fn main() {
    let cli = Cli::parse();
    match &cli.command {
        Commands::Tokenize { filename } => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {:?}", filename);
                String::new()
            });

            let mut lexer = codecrafters_interpreter::Lexer::new(&file_contents);
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
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {:?}", filename);
                String::new()
            });

            let mut parse = codecrafters_interpreter::Parser::new(&file_contents);
            if let Ok(tree) = parse.parse_expr(0) {
                println!("{tree}");
            }
        }
    }
}
