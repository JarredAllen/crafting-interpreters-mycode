use rlox1 as rlox;

use std::env;
use std::fs::File;

fn main() {
    let args = env::args().collect::<Vec<String>>();
    match args.len() {
        2 => {
            let _ = File::open(args[1].clone()).map_or_else(
                |err| println!("Error opening file: {}", err),
                |file| {
                    let _ =
                        rlox::run_file(file).map_err(|e| println!("Error executing file: {}", e));
                },
            );
        }
        1 => {
            let _ = rlox::run_prompt().map_err(|e| println!("Error running repl: {}", e));
        }
        _ => println!("Usage: rlox1 [lox source file]"),
    }
}
