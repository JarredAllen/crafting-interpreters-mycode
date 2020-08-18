mod visitors;

use super::{CodeLocation, Token, TokenKind};

use std::fmt::{self, Display};

use thiserror::Error;

pub trait Parser {
    /// Create a new parser from the given token sequence
    fn new(tokens: &[Token]) -> Self;
    /// Returns the root of the tree made by the parser, or None if no
    /// tree was made.
    fn get_tree(&self) -> &Vec<AstStmt>;
    /// Returns true iff parsing the operations resulted in errors
    fn has_errors(&self) -> bool;

    /// Returns a Vec containing all of its errors
    fn get_errors(&self) -> Vec<&AstError> {
        let mut errors = Vec::new();
        self.get_tree().iter().for_each(|stmt| {
            errors.extend(visitors::ErrorListingVisitor::list_errors_in_stmt(stmt))
        });
        errors
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
enum ExprGrammarRule {
    Expression,     // -> Equality;
    Assignment,     // -> IDENTIFIER "=" Assignment | LogicalOr
    LogicalOr,      // -> LogicalAnd ( "or" LogicalAnd )*
    LogicalAnd,     // -> Equality ("and" Equality)*
    Equality,       // -> Comparison ( ( "!=" | "==" ) Comparison )*;
    Comparison,     // -> Addition ( ( ">" | ">=" | "<" | "<=" ) Addition )*;
    Addition,       // -> Multiplication ( ( "+" | "-" ) Multiplication )*
    Multiplication, // -> Unary ( ( "*" | "/" ) Unary )*;
    Unary,          // -> ( "!" | "-" ) unary | primary;
    Call,           // -> New ( "(" (Expression ( "," Expression )* ","? )? ")" | "." IDENTIFIER )*
    New,            // -> ( "new" New "(" ( Expression ( "," Expression )* ","? )? ")" | PRIMARY )
    Primary, // -> NUMBER | STRING | IDENTIFIER | "false" | "true" | "nil" | "(" Expression ")";
}

pub struct NativeParser {
    stmt_list: Vec<AstStmt>,
    has_errors: bool,
}
impl NativeParser {
    fn parse_tokens(mut tokens: &mut std::iter::Peekable<std::slice::Iter<Token>>) -> Vec<AstStmt> {
        let mut stmts = Vec::new();
        while tokens.peek().is_some() {
            let stmt = NativeParser::parse_stmt(tokens);
            if let AstStmtKind::ErrorStmt(_) = &stmt.kind {
                // synchronize by skipping to the next semi-colon
                for token in &mut tokens {
                    if token.kind == TokenKind::Semicolon {
                        break;
                    }
                }
            }
            stmts.push(stmt);
        }
        visitors::MethodCallMutator::mutate_code(&mut stmts);
        stmts
    }
    fn parse_stmt(tokens: &mut std::iter::Peekable<std::slice::Iter<Token>>) -> AstStmt {
        match tokens.peek().map(|token| &token.kind) {
            Some(TokenKind::Var) => {
                let var_token = tokens.next().unwrap();
                let id = match tokens.next() {
                    Some(Token {
                        kind: TokenKind::Identifier(id),
                        ..
                    }) => id.clone(),
                    Some(Token { location, .. }) => {
                        return AstStmt {
                            kind: AstStmtKind::ErrorStmt(AstError {
                                kind: AstErrorKind::ExpectedIdentifier,
                                location: location.clone(),
                            }),
                            location: location.clone(),
                        }
                    }
                    None => {
                        return AstStmt {
                            kind: AstStmtKind::ErrorStmt(AstError {
                                kind: AstErrorKind::ExpectedIdentifier,
                                location: CodeLocation::EndOfFile,
                            }),
                            location: CodeLocation::EndOfFile,
                        }
                    }
                };
                match tokens.peek() {
                    Some(Token {
                        kind: TokenKind::Semicolon,
                        ..
                    }) => {
                        tokens.next();
                        AstStmt {
                            kind: AstStmtKind::VarStmt(id, None),
                            location: var_token.location.clone(),
                        }
                    }
                    Some(Token {
                        kind: TokenKind::Eq,
                        ..
                    }) => {
                        tokens.next();
                        let expr = NativeParser::parse_expr(tokens, ExprGrammarRule::Expression);
                        if let Some(Token {
                            kind: TokenKind::Semicolon,
                            ..
                        }) = tokens.peek()
                        {
                            tokens.next();
                            AstStmt {
                                kind: AstStmtKind::VarStmt(id, Some(Box::new(expr))),
                                location: var_token.location.clone(),
                            }
                        } else {
                            AstStmt {
                                kind: AstStmtKind::ErrorStmt(AstError {
                                    kind: AstErrorKind::UnterminatedStatement,
                                    location: var_token.location.clone(),
                                }),
                                location: var_token.location.clone(),
                            }
                        }
                    }
                    Some(token) => AstStmt {
                        kind: AstStmtKind::ErrorStmt(AstError {
                            kind: AstErrorKind::UnexpectedAfterVarDef,
                            location: token.location.clone(),
                        }),
                        location: token.location.clone(),
                    },
                    None => AstStmt {
                        kind: AstStmtKind::ErrorStmt(AstError {
                            kind: AstErrorKind::UnexpectedEof,
                            location: CodeLocation::EndOfFile,
                        }),
                        location: CodeLocation::EndOfFile,
                    },
                }
            }
            Some(TokenKind::LeftBrace) => {
                let error_location = &tokens.peek().unwrap().location;
                NativeParser::parse_block(tokens).map_or_else(
                    |e| AstStmt {
                        kind: AstStmtKind::ErrorStmt(e),
                        location: error_location.clone(),
                    },
                    |block| AstStmt {
                        location: block.location.clone(),
                        kind: AstStmtKind::BlockStmt(block),
                    },
                )
            }
            Some(TokenKind::If) => {
                let if_location = tokens.next().unwrap().location.clone();
                let cond = NativeParser::parse_expr(tokens, ExprGrammarRule::Expression);
                let if_body = match NativeParser::parse_block(tokens) {
                    Ok(block) => block,
                    Err(e) => {
                        return AstStmt {
                            location: e.location.clone(),
                            kind: AstStmtKind::ErrorStmt(e),
                        }
                    }
                };
                let else_body = if tokens.peek().map(|t| &t.kind) == Some(&TokenKind::Else) {
                    tokens.next();
                    match NativeParser::parse_block(tokens) {
                        Ok(block) => Some(block),
                        Err(e) => {
                            return AstStmt {
                                location: e.location.clone(),
                                kind: AstStmtKind::ErrorStmt(e),
                            }
                        }
                    }
                } else {
                    None
                };
                AstStmt {
                    location: if_location,
                    kind: AstStmtKind::IfStmt(Box::new(cond), if_body, else_body),
                }
            }
            Some(TokenKind::AST) => {
                let ast_token = tokens.next().unwrap();
                let stmt = NativeParser::parse_stmt(tokens);
                AstStmt {
                    kind: AstStmtKind::AstStmt(Box::new(stmt)),
                    location: ast_token.location.clone(),
                }
            }
            Some(TokenKind::While) => {
                let while_location = tokens.next().unwrap().location.clone();
                let cond = NativeParser::parse_expr(tokens, ExprGrammarRule::Expression);
                let body = match NativeParser::parse_block(tokens) {
                    Ok(block) => block,
                    Err(e) => {
                        return AstStmt {
                            location: e.location.clone(),
                            kind: AstStmtKind::ErrorStmt(e),
                        }
                    }
                };
                AstStmt {
                    location: while_location,
                    kind: AstStmtKind::WhileStmt(Box::new(cond), body),
                }
            }
            Some(TokenKind::For) => {
                // Desugar into a while loop plus a block
                let for_location = tokens.next().unwrap().location.clone();
                let mut desugar_stmts = Vec::new();
                let parens = match tokens.peek().map(|t| &t.kind) {
                    Some(TokenKind::LeftParen) => {
                        tokens.next();
                        true
                    }
                    _ => false,
                };
                // Initializer
                desugar_stmts.push(NativeParser::parse_stmt(tokens));
                let update = NativeParser::parse_stmt(tokens);
                let cond = NativeParser::parse_expr(tokens, ExprGrammarRule::Expression);
                if parens {
                    match tokens.next() {
                        Some(Token {
                            kind: TokenKind::RightParen,
                            ..
                        }) => (),
                        Some(_token) => {
                            panic!("Missing close paren");
                        }
                        None => {
                            panic!("Missing close paren");
                        }
                    }
                }
                let mut body = match NativeParser::parse_block(tokens) {
                    Ok(block) => block,
                    Err(e) => {
                        return AstStmt {
                            location: e.location.clone(),
                            kind: AstStmtKind::ErrorStmt(e),
                        }
                    }
                };
                body.stmts.push(update);
                desugar_stmts.push(AstStmt {
                    location: for_location.clone(),
                    kind: AstStmtKind::WhileStmt(Box::new(cond), body),
                });
                AstStmt {
                    location: for_location.clone(),
                    kind: AstStmtKind::BlockStmt(AstBlock {
                        stmts: desugar_stmts,
                        location: for_location,
                    }),
                }
            }
            Some(TokenKind::Semicolon) => {
                let location = tokens.next().unwrap().location.clone();
                AstStmt {
                    kind: AstStmtKind::EmptyStmt,
                    location,
                }
            }
            Some(TokenKind::Fun) => {
                let location = tokens.peek().unwrap().location.clone();
                match NativeParser::parse_function(tokens) {
                    FuncKind::Named(name, args, body) => AstStmt {
                        kind: AstStmtKind::FnDefnStmt(name, args, body),
                        location,
                    },
                    FuncKind::Error(e) => AstStmt {
                        location: e.location.clone(),
                        kind: AstStmtKind::ErrorStmt(e),
                    },
                    FuncKind::Unnamed(_, _) => AstStmt {
                        location: location.clone(),
                        kind: AstStmtKind::ErrorStmt(AstError {
                            kind: AstErrorKind::LambdaStatement,
                            location,
                        }),
                    },
                }
            }
            Some(TokenKind::Return) => {
                let location = tokens.next().unwrap().location.clone();
                let expr = NativeParser::parse_expr(tokens, ExprGrammarRule::Expression);
                if let Some(Token {
                    kind: TokenKind::Semicolon,
                    ..
                }) = tokens.peek()
                {
                    tokens.next();
                    AstStmt {
                        kind: AstStmtKind::ReturnStmt(Box::new(expr)),
                        location,
                    }
                } else {
                    AstStmt {
                        kind: AstStmtKind::ErrorStmt(AstError {
                            kind: AstErrorKind::UnterminatedStatement,
                            location: location.clone(),
                        }),
                        location,
                    }
                }
            }
            Some(TokenKind::Class) => {
                let location = tokens.next().unwrap().location.clone();
                let name = match tokens.next() {
                    Some(Token {
                        kind: TokenKind::Identifier(cls_name),
                        ..
                    }) => cls_name.clone(),
                    Some(token) => {
                        return AstStmt {
                            location: token.location.clone(),
                            kind: AstStmtKind::ErrorStmt(AstError {
                                location: token.location.clone(),
                                kind: AstErrorKind::ExpectedIdentifier,
                            }),
                        }
                    }
                    None => {
                        return AstStmt {
                            location: CodeLocation::EndOfFile,
                            kind: AstStmtKind::ErrorStmt(AstError {
                                location: CodeLocation::EndOfFile,
                                kind: AstErrorKind::ExpectedIdentifier,
                            }),
                        }
                    }
                };
                if tokens.next().map(|t| &t.kind) != Some(&TokenKind::LeftBrace) {
                    return AstStmt {
                        location: location.clone(),
                        kind: AstStmtKind::ErrorStmt(AstError {
                            location,
                            kind: AstErrorKind::MissingOpenBrace,
                        }),
                    };
                }
                let mut class_functions = Vec::new();
                while tokens.peek().map(|t| &t.kind) == Some(&TokenKind::Fun) {
                    match NativeParser::parse_function(tokens) {
                        FuncKind::Named(name, mut args, block) => {
                            class_functions.push((name, args, block));
                        }
                        FuncKind::Unnamed(_, block) => {
                            return AstStmt {
                                location: block.location.clone(),
                                kind: AstStmtKind::ErrorStmt(AstError {
                                    location: block.location,
                                    kind: AstErrorKind::NamedLambda,
                                }),
                            }
                        }
                        FuncKind::Error(e) => {
                            return AstStmt {
                                location: e.location.clone(),
                                kind: AstStmtKind::ErrorStmt(e),
                            }
                        }
                    }
                }
                if tokens.next().map(|t| &t.kind) != Some(&TokenKind::RightBrace) {
                    return AstStmt {
                        location: location.clone(),
                        kind: AstStmtKind::ErrorStmt(AstError {
                            location,
                            kind: AstErrorKind::MissingCloseBrace,
                        }),
                    };
                }
                AstStmt {
                    kind: AstStmtKind::ClassDefnStmt(name, class_functions),
                    location,
                }
            }
            // Didn't match a keword, so assume it's an expression statement
            Some(_) => {
                // expression statement
                let expr = NativeParser::parse_expr(tokens, ExprGrammarRule::Expression);
                if let Some(Token {
                    kind: TokenKind::Semicolon,
                    ..
                }) = tokens.peek()
                {
                    tokens.next();
                    AstStmt {
                        location: expr.location.clone(),
                        kind: AstStmtKind::ExprStmt(Box::new(expr)),
                    }
                } else {
                    // eprintln!("Unterminated statment with this AST:\n{:#?}", expr);
                    AstStmt {
                        kind: AstStmtKind::ErrorStmt(AstError {
                            kind: AstErrorKind::UnterminatedStatement,
                            location: expr.location.clone(),
                        }),
                        location: expr.location,
                    }
                }
            }
            None => unreachable!(),
        }
    }
    fn parse_block(
        tokens: &mut std::iter::Peekable<std::slice::Iter<Token>>,
    ) -> Result<AstBlock, AstError> {
        let block_location = match tokens.next() {
            Some(Token {
                kind: TokenKind::LeftBrace,
                location,
            }) => location.clone(),
            Some(Token { location, .. }) => {
                return Err(AstError {
                    kind: AstErrorKind::MissingOpenBrace,
                    location: location.clone(),
                })
            }
            None => {
                return Err(AstError {
                    kind: AstErrorKind::MissingOpenBrace,
                    location: CodeLocation::EndOfFile,
                })
            }
        };
        let mut stmts = Vec::new();
        while tokens.peek().map(|token| &token.kind) != Some(&TokenKind::RightBrace) {
            stmts.push(NativeParser::parse_stmt(tokens));
            if tokens.peek().is_none() {
                return Err(AstError {
                    kind: AstErrorKind::MissingCloseBrace,
                    location: CodeLocation::EndOfFile,
                });
            }
        }
        tokens.next();
        Ok(AstBlock {
            stmts,
            location: block_location,
        })
    }

    fn parse_expr(
        tokens: &mut std::iter::Peekable<std::slice::Iter<Token>>,
        rule: ExprGrammarRule,
    ) -> AstExpr {
        use ExprGrammarRule::*;
        match rule {
            Expression => NativeParser::parse_expr(tokens, Assignment),
            Assignment => {
                let expr = NativeParser::parse_expr(tokens, LogicalOr);
                if tokens.peek().map(|t| &t.kind) == Some(&TokenKind::Eq) {
                    tokens.next();
                    let right = NativeParser::parse_expr(tokens, Expression);
                    match NativeParser::parse_lvalue(&expr) {
                        Ok(left) => AstExpr {
                            location: expr.location.clone(),
                            kind: AstExprKind::Assign {
                                left,
                                right: Box::new(right),
                            },
                        },
                        Err(e) => AstExpr {
                            location: e.location.clone(),
                            kind: AstExprKind::Error(e),
                        },
                    }
                } else {
                    expr
                }
            }
            LogicalOr => {
                let mut root = NativeParser::parse_expr(tokens, LogicalAnd);
                loop {
                    match tokens.peek() {
                        Some(Token {
                            kind: TokenKind::Or,
                            ..
                        }) => {
                            tokens.next();
                            let location = root.location.clone();
                            root = AstExpr {
                                kind: AstExprKind::Logical {
                                    op: LogicalOpKind::Or,
                                    left: Box::new(root),
                                    right: Box::new(NativeParser::parse_expr(tokens, LogicalAnd)),
                                },
                                location,
                            }
                        }
                        _ => break root,
                    }
                }
            }
            LogicalAnd => {
                let mut root = NativeParser::parse_expr(tokens, Equality);
                loop {
                    match tokens.peek() {
                        Some(Token {
                            kind: TokenKind::And,
                            ..
                        }) => {
                            tokens.next();
                            let location = root.location.clone();
                            root = AstExpr {
                                kind: AstExprKind::Logical {
                                    op: LogicalOpKind::And,
                                    left: Box::new(root),
                                    right: Box::new(NativeParser::parse_expr(tokens, Equality)),
                                },
                                location,
                            }
                        }
                        _ => break root,
                    }
                }
            }
            Equality => {
                let mut root = NativeParser::parse_expr(tokens, Comparison);
                loop {
                    match tokens.peek() {
                        Some(token)
                            if token.kind == TokenKind::EqEq || token.kind == TokenKind::BangEq =>
                        {
                            let op = if token.kind == TokenKind::EqEq {
                                BinOpKind::Eq
                            } else {
                                BinOpKind::Ne
                            };
                            tokens.next();
                            let location = root.location.clone();
                            root = AstExpr {
                                kind: AstExprKind::BinOp {
                                    op,
                                    left: Box::new(root),
                                    right: Box::new(NativeParser::parse_expr(tokens, Comparison)),
                                },
                                location,
                            }
                        }
                        _ => break root,
                    }
                }
            }
            Comparison => {
                let mut root = NativeParser::parse_expr(tokens, Addition);
                loop {
                    match tokens.peek() {
                        Some(token)
                            if token.kind == TokenKind::Lt
                                || token.kind == TokenKind::Le
                                || token.kind == TokenKind::Ge
                                || token.kind == TokenKind::Gt =>
                        {
                            let op = match token.kind {
                                TokenKind::Lt => BinOpKind::Lt,
                                TokenKind::Le => BinOpKind::Le,
                                TokenKind::Gt => BinOpKind::Gt,
                                TokenKind::Ge => BinOpKind::Ge,
                                _ => unreachable!(),
                            };
                            tokens.next();
                            let location = root.location.clone();
                            root = AstExpr {
                                kind: AstExprKind::BinOp {
                                    op,
                                    left: Box::new(root),
                                    right: Box::new(NativeParser::parse_expr(tokens, Addition)),
                                },
                                location,
                            }
                        }
                        _ => break root,
                    }
                }
            }
            Addition => {
                let mut root = NativeParser::parse_expr(tokens, Multiplication);
                loop {
                    match tokens.peek() {
                        Some(token)
                            if token.kind == TokenKind::Plus || token.kind == TokenKind::Minus =>
                        {
                            let op = if token.kind == TokenKind::Plus {
                                BinOpKind::Add
                            } else {
                                BinOpKind::Sub
                            };
                            tokens.next();
                            let location = root.location.clone();
                            root = AstExpr {
                                kind: AstExprKind::BinOp {
                                    op,
                                    left: Box::new(root),
                                    right: Box::new(NativeParser::parse_expr(
                                        tokens,
                                        Multiplication,
                                    )),
                                },
                                location,
                            }
                        }
                        _ => break root,
                    }
                }
            }
            Multiplication => {
                let mut root = NativeParser::parse_expr(tokens, Unary);
                loop {
                    match tokens.peek() {
                        Some(token)
                            if token.kind == TokenKind::Star
                                || token.kind == TokenKind::Slash
                                || token.kind == TokenKind::Percent =>
                        {
                            let op = match &token.kind {
                                TokenKind::Star => BinOpKind::Mul,
                                TokenKind::Slash => BinOpKind::Div,
                                TokenKind::Percent => BinOpKind::Mod,
                                _ => unreachable!(),
                            };
                            tokens.next();
                            let location = root.location.clone();
                            root = AstExpr {
                                kind: AstExprKind::BinOp {
                                    op,
                                    left: Box::new(root),
                                    right: Box::new(NativeParser::parse_expr(tokens, Unary)),
                                },
                                location,
                            }
                        }
                        _ => break root,
                    }
                }
            }
            Unary => match tokens.peek() {
                Some(token) if token.kind == TokenKind::Bang || token.kind == TokenKind::Minus => {
                    let op = if token.kind == TokenKind::Bang {
                        UnOpKind::Not
                    } else {
                        UnOpKind::Neg
                    };
                    let location = token.location.clone();
                    tokens.next();
                    AstExpr {
                        kind: AstExprKind::UnOp {
                            op,
                            arg: Box::new(NativeParser::parse_expr(tokens, Unary)),
                        },
                        location,
                    }
                }
                Some(token) if token.kind == TokenKind::Plus => {
                    let location = tokens.next().unwrap().location.clone();
                    AstExpr {
                        kind: AstExprKind::Error(AstError {
                            kind: AstErrorKind::NoUnaryPlus,
                            location: location.clone(),
                        }),
                        location,
                    }
                }
                Some(token) if token.kind == TokenKind::Star => {
                    let location = tokens.next().unwrap().location.clone();
                    AstExpr {
                        kind: AstExprKind::Error(AstError {
                            kind: AstErrorKind::NoUnaryMul,
                            location: location.clone(),
                        }),
                        location,
                    }
                }
                Some(token) if token.kind == TokenKind::Slash => {
                    let location = tokens.next().unwrap().location.clone();
                    AstExpr {
                        kind: AstExprKind::Error(AstError {
                            kind: AstErrorKind::NoUnaryDiv,
                            location: location.clone(),
                        }),
                        location,
                    }
                }
                _ => NativeParser::parse_expr(tokens, Call),
            },
            Call => {
                let mut expr = NativeParser::parse_expr(tokens, New);
                loop {
                    // while let Some(&TokenKind::LeftParen) | Some(&TokenKind::Dot) = tokens.peek().map(|t| &t.kind) {
                    match tokens.peek().map(|t| &t.kind) {
                        // Match a function call
                        Some(&TokenKind::LeftParen) => {
                            tokens.next();
                            let mut args = Vec::new();
                            while tokens.peek().map(|t| &t.kind) != Some(&TokenKind::RightParen) {
                                args.push(NativeParser::parse_expr(tokens, Expression));
                                if args.len() > 255 {
                                    let location = args[args.len() - 1].location.clone();
                                    args.push(AstExpr {
                                        kind: AstExprKind::Error(AstError {
                                            kind: AstErrorKind::TooManyArgs,
                                            location: location.clone(),
                                        }),
                                        location,
                                    });
                                }
                                match tokens.peek() {
                                    Some(Token {
                                        kind: TokenKind::Comma,
                                        ..
                                    }) => {
                                        tokens.next();
                                    }
                                    Some(Token {
                                        kind: TokenKind::RightParen,
                                        ..
                                    }) => (),
                                    Some(t) => args.push(AstExpr {
                                        kind: AstExprKind::Error(AstError {
                                            kind: AstErrorKind::MissingComma,
                                            location: t.location.clone(),
                                        }),
                                        location: t.location.clone(),
                                    }),
                                    None => args.push(AstExpr {
                                        kind: AstExprKind::Error(AstError {
                                            kind: AstErrorKind::MissingComma,
                                            location: CodeLocation::EndOfFile,
                                        }),
                                        location: CodeLocation::EndOfFile,
                                    }),
                                }
                            }
                            tokens.next();
                            expr = AstExpr {
                                location: expr.location.clone(),
                                kind: AstExprKind::Call {
                                    callee: Box::new(expr),
                                    args,
                                },
                            };
                        }
                        // Match a field access
                        Some(&TokenKind::Dot) => {
                            tokens.next();
                            match tokens.next() {
                                Some(Token {
                                    kind: TokenKind::Identifier(ident),
                                    location: _,
                                }) => {
                                    expr = AstExpr {
                                        location: expr.location.clone(),
                                        kind: AstExprKind::Field {
                                            target: Box::new(expr),
                                            field_name: ident.clone(),
                                        },
                                    }
                                }
                                Some(token) => {
                                    expr = AstExpr {
                                        location: token.location.clone(),
                                        kind: AstExprKind::Error(AstError {
                                            kind: AstErrorKind::ExpectedIdentifier,
                                            location: token.location.clone(),
                                        }),
                                    }
                                }
                                None => {
                                    expr = AstExpr {
                                        location: CodeLocation::EndOfFile,
                                        kind: AstExprKind::Error(AstError {
                                            kind: AstErrorKind::ExpectedIdentifier,
                                            location: CodeLocation::EndOfFile,
                                        }),
                                    }
                                }
                            };
                        }
                        // Neither a function nor a field access, so we break
                        _ => break,
                    }
                }
                expr
            }
            New => match tokens.peek().map(|t| &t.kind) {
                Some(TokenKind::New) => {
                    let location = tokens.next().unwrap().location.clone();
                    let callee = NativeParser::parse_expr(tokens, New);
                    if tokens.peek().map(|t| &t.kind) != Some(&TokenKind::LeftParen) {
                        unimplemented!("new statement without parentheses");
                    }
                    tokens.next();
                    let mut args = Vec::new();
                    while tokens.peek().map(|t| &t.kind) != Some(&TokenKind::RightParen) {
                        args.push(NativeParser::parse_expr(tokens, Expression));
                        if args.len() > 255 {
                            let location = args[args.len() - 1].location.clone();
                            args.push(AstExpr {
                                kind: AstExprKind::Error(AstError {
                                    kind: AstErrorKind::TooManyArgs,
                                    location: location.clone(),
                                }),
                                location,
                            });
                        }
                        match tokens.peek() {
                            Some(Token {
                                kind: TokenKind::Comma,
                                ..
                            }) => {
                                tokens.next();
                            }
                            Some(Token {
                                kind: TokenKind::RightParen,
                                ..
                            }) => (),
                            Some(t) => args.push(AstExpr {
                                kind: AstExprKind::Error(AstError {
                                    kind: AstErrorKind::MissingComma,
                                    location: t.location.clone(),
                                }),
                                location: t.location.clone(),
                            }),
                            None => args.push(AstExpr {
                                kind: AstExprKind::Error(AstError {
                                    kind: AstErrorKind::MissingComma,
                                    location: CodeLocation::EndOfFile,
                                }),
                                location: CodeLocation::EndOfFile,
                            }),
                        }
                    }
                    tokens.next();
                    AstExpr {
                        location,
                        kind: AstExprKind::New(Box::new(callee), args),
                    }
                }
                _ => NativeParser::parse_expr(tokens, Primary),
            },
            Primary => match tokens.peek() {
                Some(&token)
                    if token.kind == TokenKind::False
                        || token.kind == TokenKind::True
                        || token.kind == TokenKind::Nil =>
                {
                    let leaf = match token.kind {
                        TokenKind::False => LeafKind::False,
                        TokenKind::True => LeafKind::True,
                        TokenKind::Nil => LeafKind::Nil,
                        _ => unreachable!(),
                    };
                    let location = tokens.next().unwrap().location.clone();
                    AstExpr {
                        kind: AstExprKind::Leaf(leaf),
                        location,
                    }
                }
                Some(Token {
                    kind: TokenKind::StringLiteral(string),
                    location,
                }) => {
                    let leaf = AstExprKind::Leaf(LeafKind::StringLiteral(string.clone()));
                    tokens.next();
                    AstExpr {
                        kind: leaf,
                        location: location.clone(),
                    }
                }
                Some(Token {
                    kind: TokenKind::NumberLiteral(num),
                    location,
                }) => {
                    let leaf = num.parse::<isize>().ok().map_or_else(
                        || {
                            num.parse::<f64>().ok().map_or_else(
                                || {
                                    AstExprKind::Error(AstError {
                                        kind: AstErrorKind::NumberParseError(num.clone()),
                                        location: location.clone(),
                                    })
                                },
                                |n| AstExprKind::Leaf(LeafKind::FloatLiteral(n)),
                            )
                        },
                        |n| AstExprKind::Leaf(LeafKind::IntegerLiteral(n)),
                    );
                    tokens.next();
                    AstExpr {
                        kind: leaf,
                        location: location.clone(),
                    }
                }
                Some(Token {
                    kind: TokenKind::LeftParen,
                    location,
                }) => {
                    let open_location = location.clone();
                    tokens.next();
                    if tokens.peek().map(|t| &t.kind) == Some(&TokenKind::RightParen) {
                        let location = tokens.next().unwrap().location.clone();
                        return AstExpr {
                            kind: AstExprKind::Error(AstError {
                                kind: AstErrorKind::NoExprInParens,
                                location: location.clone(),
                            }),
                            location,
                        };
                    }
                    let node = NativeParser::parse_expr(tokens, Expression);
                    match tokens.next() {
                        Some(Token {
                            kind: TokenKind::RightParen,
                            ..
                        }) => node,
                        _ => AstExpr {
                            kind: AstExprKind::Error(AstError {
                                kind: AstErrorKind::MissingCloseParen,
                                location: open_location,
                            }),
                            ..node
                        },
                    }
                }
                Some(Token {
                    kind: TokenKind::Identifier(id),
                    ..
                }) => {
                    let id = id.clone();
                    let location = tokens.next().unwrap().location.clone();
                    AstExpr {
                        kind: AstExprKind::Leaf(LeafKind::Identifier(id)),
                        location,
                    }
                }
                Some(Token {
                    kind: TokenKind::Fun,
                    location,
                }) => match NativeParser::parse_function(tokens) {
                    FuncKind::Unnamed(args, body) => AstExpr {
                        kind: AstExprKind::Lambda(args, body),
                        location: location.clone(),
                    },
                    FuncKind::Named(_, _, _) => AstExpr {
                        kind: AstExprKind::Error(AstError {
                            kind: AstErrorKind::NamedLambda,
                            location: location.clone(),
                        }),
                        location: location.clone(),
                    },
                    FuncKind::Error(e) => AstExpr {
                        location: e.location.clone(),
                        kind: AstExprKind::Error(e),
                    },
                },
                Some(&token) => {
                    let location = tokens.next().unwrap().location.clone();
                    AstExpr {
                        kind: AstExprKind::Error(AstError {
                            location: location.clone(),
                            kind: AstErrorKind::IllegalToken(token.clone()),
                        }),
                        location,
                    }
                }
                None => AstExpr {
                    kind: AstExprKind::Error(AstError {
                        kind: AstErrorKind::UnexpectedEof,
                        location: CodeLocation::EndOfFile,
                    }),
                    location: CodeLocation::EndOfFile,
                },
            },
        }
    }

    fn parse_function(tokens: &mut std::iter::Peekable<std::slice::Iter<Token>>) -> FuncKind {
        tokens.next();
        let name = match tokens.peek() {
            Some(Token {
                kind: TokenKind::Identifier(name),
                ..
            }) => {
                let name = name.clone();
                tokens.next();
                Some(name)
            }
            Some(Token {
                kind: TokenKind::LeftParen,
                ..
            }) => None,
            Some(t) => {
                return FuncKind::Error(AstError {
                    kind: AstErrorKind::ExpectedIdentifier,
                    location: t.location.clone(),
                })
            }
            None => {
                return FuncKind::Error(AstError {
                    kind: AstErrorKind::ExpectedIdentifier,
                    location: CodeLocation::EndOfFile,
                })
            }
        };
        match tokens.next() {
            Some(Token {
                kind: TokenKind::LeftParen,
                ..
            }) => {}
            Some(t) => {
                return FuncKind::Error(AstError {
                    kind: AstErrorKind::MissingOpenParen,
                    location: t.location.clone(),
                })
            }
            None => {
                return FuncKind::Error(AstError {
                    kind: AstErrorKind::MissingOpenParen,
                    location: CodeLocation::EndOfFile,
                })
            }
        }
        let mut args = Vec::new();
        while tokens.peek().map(|t| &t.kind) != Some(&TokenKind::RightParen) {
            let next_token = tokens.next();
            match next_token {
                Some(Token {
                    kind: TokenKind::Identifier(name),
                    ..
                }) => args.push(name.clone()),
                Some(t) => {
                    return FuncKind::Error(AstError {
                        kind: AstErrorKind::ExpectedIdentifier,
                        location: t.location.clone(),
                    })
                }
                None => {
                    return FuncKind::Error(AstError {
                        kind: AstErrorKind::ExpectedIdentifier,
                        location: CodeLocation::EndOfFile,
                    })
                }
            }
            if args.len() > 255 {
                return FuncKind::Error(AstError {
                    kind: AstErrorKind::TooManyArgs,
                    location: next_token.unwrap().location.clone(),
                });
            }
            match tokens.peek() {
                Some(Token {
                    kind: TokenKind::Comma,
                    ..
                }) => {
                    tokens.next();
                }
                Some(Token {
                    kind: TokenKind::RightParen,
                    ..
                }) => (),
                Some(t) => {
                    return FuncKind::Error(AstError {
                        kind: AstErrorKind::MissingComma,
                        location: t.location.clone(),
                    })
                }
                None => {
                    return FuncKind::Error(AstError {
                        kind: AstErrorKind::MissingComma,
                        location: CodeLocation::EndOfFile,
                    })
                }
            }
        }
        tokens.next();
        let body = match NativeParser::parse_block(tokens) {
            Ok(block) => block.clone(),
            Err(e) => return FuncKind::Error(e),
        };
        match name {
            Some(name) => FuncKind::Named(name, args, body),
            None => FuncKind::Unnamed(args, body),
        }
    }

    fn parse_lvalue(expr: &AstExpr) -> Result<AstLValue, AstError> {
        match &expr.kind {
            AstExprKind::Leaf(LeafKind::Identifier(id)) => Ok(AstLValue::Identifier(id.clone())),
            AstExprKind::Field { target, field_name } => Ok(AstLValue::Field(
                Box::new(NativeParser::parse_lvalue(target.as_ref())?),
                field_name.clone(),
            )),
            _ => Err(AstError {
                kind: AstErrorKind::RValueAssign,
                location: expr.location.clone(),
            }),
        }
    }
}
impl Parser for NativeParser {
    fn new(tokens: &[Token]) -> NativeParser {
        let mut tokens_iter = tokens.iter().peekable();
        let stmt_list = NativeParser::parse_tokens(&mut tokens_iter);
        let has_errors = stmt_list
            .iter()
            .any(|stmt| visitors::ErrorCheckingVisitor::check_stmt_for_errors(stmt));
        NativeParser {
            stmt_list,
            has_errors,
        }
    }
    fn get_tree(&self) -> &Vec<AstStmt> {
        &self.stmt_list
    }
    fn has_errors(&self) -> bool {
        self.has_errors
    }
}

enum FuncKind {
    Named(String, Vec<String>, AstBlock),
    Unnamed(Vec<String>, AstBlock),
    Error(AstError),
}

#[derive(Debug, Clone, PartialEq)]
pub struct AstExpr {
    pub kind: AstExprKind,
    pub location: CodeLocation,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AstExprKind {
    BinOp {
        op: BinOpKind,
        left: Box<AstExpr>,
        right: Box<AstExpr>,
    },
    Logical {
        op: LogicalOpKind,
        left: Box<AstExpr>,
        right: Box<AstExpr>,
    },
    Assign {
        left: AstLValue,
        right: Box<AstExpr>,
    },
    Call {
        callee: Box<AstExpr>,
        args: Vec<AstExpr>,
    },
    MethodCall {
        callee: Box<AstExpr>,
        method_name: String,
        args: Vec<AstExpr>,
    },
    UnOp {
        op: UnOpKind,
        arg: Box<AstExpr>,
    },
    Field {
        target: Box<AstExpr>,
        field_name: String,
    },
    Lambda(Vec<String>, AstBlock),
    New(Box<AstExpr>, Vec<AstExpr>),
    Leaf(LeafKind),
    Error(AstError),
}

#[derive(Debug, Clone, PartialEq)]
pub enum AstLValue {
    Identifier(String),
    Field(Box<AstLValue>, String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum LeafKind {
    StringLiteral(String),
    IntegerLiteral(isize),
    FloatLiteral(f64),
    True,
    False,
    Nil,
    Identifier(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LogicalOpKind {
    And,
    Or,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnOpKind {
    Neg,
    Not,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinOpKind {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Clone, PartialEq, Debug)]
pub struct AstStmt {
    pub kind: AstStmtKind,
    pub location: CodeLocation,
}

#[derive(Clone, PartialEq, Debug)]
pub enum AstStmtKind {
    /// A statement which does nothing
    EmptyStmt,
    /// An expression which is evaluated and the result discarded.
    ExprStmt(Box<AstExpr>),
    /// The declaring of a new variable, with an optional expression
    /// to initialize the variable to.
    VarStmt(String, Option<Box<AstExpr>>),
    /// A block of code.
    BlockStmt(AstBlock),
    /// An if or if-else statment
    IfStmt(Box<AstExpr>, AstBlock, Option<AstBlock>),
    /// A while loop (which may be the result of desugaring a for-loop)
    WhileStmt(Box<AstExpr>, AstBlock),
    /// An AST <blah> expression, which prints the AST of <blah> (to be
    /// removed at a later point)
    AstStmt(Box<AstStmt>),
    /// A return statement
    ReturnStmt(Box<AstExpr>),
    /// The definition of a new function
    FnDefnStmt(String, Vec<String>, AstBlock),
    /// The definition of a new class
    ClassDefnStmt(String, Vec<(String, Vec<String>, AstBlock)>),
    /// An error in parsing
    ErrorStmt(AstError),
}

#[derive(Clone, PartialEq, Debug)]
pub struct AstBlock {
    pub stmts: Vec<AstStmt>,
    pub location: CodeLocation,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Error)]
#[error("AST Error: {kind}\n{location}")]
pub struct AstError {
    pub kind: AstErrorKind,
    pub location: CodeLocation,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AstErrorKind {
    ExpectedIdentifier,
    IllegalToken(Token),
    LambdaStatement,
    MissingOpenParen,
    MissingCloseParen,
    MissingOpenBrace,
    MissingCloseBrace,
    MissingComma,
    NamedLambda,
    NoExprInParens,
    NoUnaryPlus,
    NoUnaryMul,
    NoUnaryDiv,
    NumberParseError(String),
    RValueAssign,
    TooManyArgs,
    UnexpectedAfterVarDef,
    UnexpectedEof,
    UnterminatedStatement,
}
impl Display for AstErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                AstErrorKind::ExpectedIdentifier => String::from("Expected a valid identifier"),
                AstErrorKind::IllegalToken(token) => format!("Illegal token: {:?}", token),
                AstErrorKind::LambdaStatement => String::from("Functions must have a name"),
                AstErrorKind::MissingOpenParen => String::from("Missing open parenthesis"),
                AstErrorKind::MissingCloseParen => String::from("Missing close parenthesis"),
                AstErrorKind::MissingOpenBrace => String::from("Missing open brace"),
                AstErrorKind::MissingCloseBrace => String::from("Missing close brace"),
                AstErrorKind::MissingComma => String::from("Missing comma"),
                AstErrorKind::NamedLambda => String::from("Lambda functions cannot have a name"),
                AstErrorKind::NoExprInParens => String::from("Expected an expression, found `)`"),
                AstErrorKind::NoUnaryPlus => String::from("Unary `+` expressions aren't supported"),
                AstErrorKind::NoUnaryMul => String::from("Unary `*` expressions aren't supported"),
                AstErrorKind::NoUnaryDiv => String::from("Unary `/` expressions aren't supported"),
                AstErrorKind::NumberParseError(lit) =>
                    format!("Couldn't parse number literal: {}", lit),
                AstErrorKind::RValueAssign => String::from("Attempted assignment to r-value"),
                AstErrorKind::TooManyArgs =>
                    String::from("Functions can have at most 255 arguments"),
                AstErrorKind::UnexpectedAfterVarDef =>
                    String::from("Expected `=` or `;` after variable definition"),
                AstErrorKind::UnexpectedEof => String::from("Unexpected End of File"),
                AstErrorKind::UnterminatedStatement => String::from("Statement missing `;` at end"),
            }
        )
    }
}
