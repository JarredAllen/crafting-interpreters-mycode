use std::collections::HashMap;

use super::CodeLocation;

pub trait Lexer {
    /// Create a new lexer from the given text
    fn new(source: &str) -> Self;

    /// Returns a slice of all the tokens from this lexer
    fn tokens(&self) -> &[Token];

    /// Returns true iff there was an error lexing the text
    fn found_error(&self) -> bool;
}

/// A complete lexer for Lox
pub struct NativeLexer {
    tokens: Vec<Token>,
    keywords: HashMap<&'static str, TokenKind>,
    found_error: bool,
}

impl NativeLexer {
    fn scan_tokens(&mut self, source: &str) {
        let mut index = source.chars().peekable();
        let mut line = 1;
        let mut col = 1;
        use TokenKind::*;
        while let Some(next_char) = index.next() {
            match next_char {
                '(' => self.tokens.push(Token::new(
                    LeftParen,
                    CodeLocation::LineNumAndPosition {
                        line_num: line,
                        col,
                    },
                )),
                ')' => self.tokens.push(Token::new(
                    RightParen,
                    CodeLocation::LineNumAndPosition {
                        line_num: line,
                        col,
                    },
                )),
                '{' => self.tokens.push(Token::new(
                    LeftBrace,
                    CodeLocation::LineNumAndPosition {
                        line_num: line,
                        col,
                    },
                )),
                '}' => self.tokens.push(Token::new(
                    RightBrace,
                    CodeLocation::LineNumAndPosition {
                        line_num: line,
                        col,
                    },
                )),
                ',' => self.tokens.push(Token::new(
                    Comma,
                    CodeLocation::LineNumAndPosition {
                        line_num: line,
                        col,
                    },
                )),
                '.' => self.tokens.push(Token::new(
                    Dot,
                    CodeLocation::LineNumAndPosition {
                        line_num: line,
                        col,
                    },
                )),
                '-' => self.tokens.push(Token::new(
                    Minus,
                    CodeLocation::LineNumAndPosition {
                        line_num: line,
                        col,
                    },
                )),
                '+' => self.tokens.push(Token::new(
                    Plus,
                    CodeLocation::LineNumAndPosition {
                        line_num: line,
                        col,
                    },
                )),
                ';' => self.tokens.push(Token::new(
                    Semicolon,
                    CodeLocation::LineNumAndPosition {
                        line_num: line,
                        col,
                    },
                )),
                '*' => self.tokens.push(Token::new(
                    Star,
                    CodeLocation::LineNumAndPosition {
                        line_num: line,
                        col,
                    },
                )),
                '%' => self.tokens.push(Token::new(
                    Percent,
                    CodeLocation::LineNumAndPosition {
                        line_num: line,
                        col,
                    },
                )),
                '!' => match index.peek() {
                    Some('=') => {
                        self.tokens.push(Token::new(
                            BangEq,
                            CodeLocation::LineNumAndPosition {
                                line_num: line,
                                col,
                            },
                        ));
                        index.next();
                        col += 2;
                        continue;
                    }
                    _ => self.tokens.push(Token::new(
                        Bang,
                        CodeLocation::LineNumAndPosition {
                            line_num: line,
                            col,
                        },
                    )),
                },
                '=' => match index.peek() {
                    Some('=') => {
                        self.tokens.push(Token::new(
                            EqEq,
                            CodeLocation::LineNumAndPosition {
                                line_num: line,
                                col,
                            },
                        ));
                        index.next();
                        col += 2;
                        continue;
                    }
                    _ => self.tokens.push(Token::new(
                        Eq,
                        CodeLocation::LineNumAndPosition {
                            line_num: line,
                            col,
                        },
                    )),
                },
                '<' => match index.peek() {
                    Some('=') => {
                        self.tokens.push(Token::new(
                            Le,
                            CodeLocation::LineNumAndPosition {
                                line_num: line,
                                col,
                            },
                        ));
                        index.next();
                        col += 2;
                        continue;
                    }
                    _ => self.tokens.push(Token::new(
                        Lt,
                        CodeLocation::LineNumAndPosition {
                            line_num: line,
                            col,
                        },
                    )),
                },
                '>' => match index.peek() {
                    Some('=') => {
                        self.tokens.push(Token::new(
                            Ge,
                            CodeLocation::LineNumAndPosition {
                                line_num: line,
                                col,
                            },
                        ));
                        index.next();
                        col += 2;
                        continue;
                    }
                    _ => self.tokens.push(Token::new(
                        Gt,
                        CodeLocation::LineNumAndPosition {
                            line_num: line,
                            col,
                        },
                    )),
                },
                '/' => {
                    match index.peek() {
                        Some('/') => {
                            // comment
                            index.find(|&c| c == '\n');
                            line += 1;
                            col = 1;
                            continue;
                        }
                        _ => self.tokens.push(Token::new(
                            Slash,
                            CodeLocation::LineNumAndPosition {
                                line_num: line,
                                col,
                            },
                        )),
                    }
                }
                '"' => {
                    let mut literal = String::new();
                    self.tokens.push(loop {
                        let c = index.next();
                        match c {
                            Some('"') => {
                                let old_col = col;
                                col += literal.len() + 2;
                                break Token::new(
                                    StringLiteral(literal),
                                    CodeLocation::LineNumAndPosition {
                                        line_num: line,
                                        col: old_col,
                                    },
                                );
                            }
                            None | Some('\n') => {
                                let old_line = line;
                                let old_col = col;
                                line += 1;
                                col = 1;
                                self.found_error = true;
                                break Token::new(
                                    StringLiteralError(format!("\"{}", literal)),
                                    CodeLocation::LineNumAndPosition {
                                        line_num: old_line,
                                        col: old_col,
                                    },
                                );
                            }
                            Some(c) => {
                                literal.push(c);
                            }
                        }
                    });
                    continue;
                }
                '0'..='9' => {
                    let mut literal = String::new();
                    literal.push(next_char);
                    self.tokens.push(loop {
                        let c = index.peek();
                        match c {
                            Some(c) if c.is_numeric() || c == &'.' => {
                                literal.push(*c);
                                index.next();
                            }
                            _ => {
                                let old_col = col;
                                col += literal.len();
                                break Token::new(
                                    NumberLiteral(literal),
                                    CodeLocation::LineNumAndPosition {
                                        line_num: line,
                                        col: old_col,
                                    },
                                );
                            }
                        }
                    });
                    continue;
                }
                'a'..='z' | 'A'..='Z' | '_' | '$' => {
                    let mut name = String::new();
                    name.push(next_char);
                    loop {
                        let c = index.peek();
                        match c {
                            Some(c) if c.is_alphanumeric() || c == &'_' || c == &'$' => {
                                name.push(*c);
                                index.next();
                            }
                            _ => break,
                        }
                    }
                    let old_col = col;
                    col += name.len();
                    if let Some(keyword) = self.keywords.get(name.as_str()) {
                        self.tokens.push(Token::new(
                            keyword.clone(),
                            CodeLocation::LineNumAndPosition {
                                line_num: line,
                                col: old_col,
                            },
                        ));
                    } else {
                        self.tokens.push(Token::new(
                            Identifier(name),
                            CodeLocation::LineNumAndPosition {
                                line_num: line,
                                col: old_col,
                            },
                        ));
                    }
                    continue;
                }
                ' ' | '\t' | '\r' => {}
                '\n' => {
                    line += 1;
                    col = 1;
                    continue;
                }
                c => {
                    self.found_error = true;
                    self.tokens.push(Token::new(
                        IllegalCharacterError(c),
                        CodeLocation::LineNumAndPosition {
                            line_num: line,
                            col,
                        },
                    ));
                }
            }
            col += 1;
        }
        self.tokens.shrink_to_fit();
    }
}

impl Lexer for NativeLexer {
    /// Create a new lexer
    fn new(source: &str) -> NativeLexer {
        let keywords = [
            ("and", TokenKind::And),
            ("AST", TokenKind::AST),
            ("class", TokenKind::Class),
            ("else", TokenKind::Else),
            ("false", TokenKind::False),
            ("for", TokenKind::For),
            ("fun", TokenKind::Fun),
            ("if", TokenKind::If),
            ("nil", TokenKind::Nil),
            ("new", TokenKind::New),
            ("or", TokenKind::Or),
            ("return", TokenKind::Return),
            ("super", TokenKind::Super),
            ("this", TokenKind::This),
            ("true", TokenKind::True),
            ("var", TokenKind::Var),
            ("while", TokenKind::While),
        ]
        .iter()
        .cloned()
        .collect();
        let mut lexer = NativeLexer {
            tokens: Vec::new(),
            keywords,
            found_error: false,
        };
        lexer.scan_tokens(source);
        lexer
    }

    fn tokens(&self) -> &[Token] {
        self.tokens.as_slice()
    }

    fn found_error(&self) -> bool {
        self.found_error
    }
}

/// A token. It has a kind and it has its location in the file.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Token {
    pub kind: TokenKind,
    pub location: CodeLocation,
}
impl Token {
    pub fn new(kind: TokenKind, location: CodeLocation) -> Token {
        Token { kind, location }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// The kinds of valid tokens in Lox
pub enum TokenKind {
    // One or two symbol keywords
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Percent,
    Bang,
    BangEq,
    Eq,
    EqEq,
    Gt,
    Ge,
    Lt,
    Le,
    // Word keywords
    And,
    AST,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    New,
    Nil,
    Or,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    // Literals
    Identifier(String),
    StringLiteral(String),
    NumberLiteral(String),
    // Other
    EOF,
    StringLiteralError(String),
    IllegalCharacterError(char),
}
