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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_code_location() {
        // Test get_line_num
        assert_eq!(CodeLocation::NoInfo.get_line_num(), None);
        assert_eq!(CodeLocation::EndOfFile.get_line_num(), None);
        assert_eq!(CodeLocation::LineNum { line_num: 5 }.get_line_num(), Some(5));
        assert_eq!(CodeLocation::LineNumAndPosition { line_num: 5, col: 3 }.get_line_num(), Some(5));
        // Test add_line
        assert_eq!(CodeLocation::Line { line: "object.method()".to_string() }, CodeLocation::NoInfo.add_line("object.method()".to_string()));
        assert_eq!(CodeLocation::Line { line: "object.method()".to_string() }, CodeLocation::EndOfFile.add_line("object.method()".to_string()));
        assert_eq!(CodeLocation::LineNumAndLine { line: "object.method()".to_string(), line_num: 5}, CodeLocation::LineNum { line_num: 5 }.add_line("object.method()".to_string()));
        assert_eq!(CodeLocation::LineNumLineAndPosition { line: "object.method()".to_string(), line_num: 5, col: 3 }, CodeLocation::LineNumAndPosition { line_num: 5, col: 3 }.add_line("object.method()".to_string()));
        // Test add_line_from_slice
        let code = ["var object = object();", "object.method();", "print(\"We did it, reddit\");"].iter().cloned().map(String::from).collect::<Vec<String>>();
        assert_eq!(CodeLocation::NoInfo, CodeLocation::NoInfo.add_line_from_slice(&code));
        assert_eq!(CodeLocation::LineNumAndLine { line_num: 1, line: "var object = object();".to_string() }, CodeLocation::LineNum { line_num: 1 }.add_line_from_slice(&code));
        // Test display impls
        assert_eq!("No location info".to_string(), format!("{}", CodeLocation::NoInfo));
        assert_eq!("End of file".to_string(), format!("{}", CodeLocation::EndOfFile));
        assert_eq!("? | print(42);".to_string(), format!("{}", CodeLocation::Line { line: "print(42);".to_string() }));
    }
}
