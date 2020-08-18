mod interpreter;
mod lexer;
mod parser;
mod visitor;

use std::error::Error;
use std::fmt::{self, Debug, Display};
use std::fs::File;
use std::io::{self, Read, Write};

pub use interpreter::{Interpreter, InterpreterError, NativeInterpreter};
pub use lexer::{Lexer, NativeLexer, Token, TokenKind};
pub use parser::{AstExpr, AstExprKind, BinOpKind, NativeParser, Parser, UnOpKind};

pub type RunError = Box<dyn 'static + Error>;

/// Read the given file and execute it
pub fn run_file(mut file: File) -> Result<(), RunError> {
    let mut source = String::new();
    file.read_to_string(&mut source)?;
    let _ = NativeInterpreter::<NativeLexer, NativeParser>::new()
        .run_code(&source)
        .map_err(|errs| {
            println!("Interpretation failed due to errors:");
            errs.iter().for_each(|e| println!("{}", e));
        });
    Ok(())
}

/// Create a REPL prompt and execute the code the user inputs
pub fn run_prompt() -> Result<(), RunError> {
    let mut interpreter: NativeInterpreter<NativeLexer, NativeParser> = Default::default();
    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        let mut line = String::new();
        if io::stdin().read_line(&mut line)? == 0 {
            break;
        }
        // TODO enable reading multiple lines before running
        let _ = interpreter.run_code(&line).map_err(|errs| {
            println!(
                "Interpretation failed due to errors:\n{}",
                errs.iter()
                    .map(|e| format!("{}", e))
                    .collect::<Vec<String>>()
                    .join("\n")
            );
        });
    }
    println!();
    Ok(())
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// A location for an error, with as much info as can be provided
pub enum CodeLocation {
    NoInfo,
    EndOfFile,
    Line {
        line: String,
    },
    LineNum {
        line_num: usize,
    },
    LineNumAndLine {
        line_num: usize,
        line: String,
    },
    LineNumAndPosition {
        line_num: usize,
        col: usize,
    },
    LineNumLineAndPosition {
        line_num: usize,
        line: String,
        col: usize,
    },
}
impl CodeLocation {
    /// Adds the given line to the Code Location, if it doesn't have it already
    fn add_line(&self, line: String) -> Self {
        match self {
            CodeLocation::NoInfo => CodeLocation::Line { line },
            CodeLocation::EndOfFile => CodeLocation::Line { line },
            CodeLocation::LineNum { line_num } => CodeLocation::LineNumAndLine {
                line_num: *line_num,
                line,
            },
            CodeLocation::LineNumAndPosition { line_num, col } => {
                CodeLocation::LineNumLineAndPosition {
                    line_num: *line_num,
                    line,
                    col: *col,
                }
            }
            cl => cl.clone(),
        }
    }

    fn add_line_from_slice(&self, slice: &[String]) -> Self {
        match self {
            CodeLocation::EndOfFile => CodeLocation::LineNumLineAndPosition {
                line_num: slice.len(),
                line: slice[slice.len() - 1].clone(),
                col: slice[slice.len() - 1].len(),
            },
            _ => self.get_line_num().map_or_else(
                || self.clone(),
                |line| self.add_line(slice[line - 1].clone()),
            ),
        }
    }

    pub fn get_line_num(&self) -> Option<usize> {
        match self {
            CodeLocation::LineNum { line_num } => Some(*line_num),
            CodeLocation::LineNumAndLine { line_num, .. } => Some(*line_num),
            CodeLocation::LineNumAndPosition { line_num, .. } => Some(*line_num),
            CodeLocation::LineNumLineAndPosition { line_num, .. } => Some(*line_num),
            _ => None,
        }
    }
}
impl Display for CodeLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                CodeLocation::NoInfo => String::from("No location info"),
                CodeLocation::EndOfFile => String::from("End of file"),
                CodeLocation::Line { line } => format!("? | {}", line),
                CodeLocation::LineNum { line_num } => format!("at line {}", line_num),
                CodeLocation::LineNumAndLine { line_num, line } =>
                    format!("{} | {}", line_num, line),
                CodeLocation::LineNumAndPosition { line_num, col } =>
                    format!("at line {}, col {}", line_num, col),
                CodeLocation::LineNumLineAndPosition {
                    line_num,
                    line,
                    col,
                } => {
                    let line1 = format!("{} | {}", line_num, line);
                    let line2 = format!(
                        "{:spaces$}^{:-<dashes$} Here",
                        "",
                        "",
                        spaces = line1.find('|').unwrap() + 1 + col,
                        dashes = (line.len() as isize + 2 - *col as isize).max(2) as usize,
                    );
                    format!("{}\n{}", line1, line2)
                }
            }
        )
    }
}
