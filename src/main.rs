use std::env;
use std::fs;

use miette::IntoDiagnostic;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 3 {
        eprintln!("Usage: {} tokenize <filename>", args[0]);
        return;
    }

    let command = &args[1];
    let filename = &args[2];

    match command.as_str() {
        "tokenize" => {
            let file_contents = fs::read_to_string(filename)
                .into_diagnostic()
                .unwrap_or_else(|_| {
                    eprintln!("Failed to read file {}", filename);
                    String::new()
                });

            let lexer = codecrafters_interpreter::Lexer::new(&file_contents);
            for token in lexer {
                println!("{}", token);
            }
        }
        _ => {
            eprintln!("Unknown command: {}", command);
            return;
        }
    }
}
