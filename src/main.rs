use std::env;
use std::fs;
use std::process::exit;

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
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename);
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
        _ => {
            eprintln!("Unknown command: {}", command);
            return;
        }
    }
}
