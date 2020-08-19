use crate::{
    parser::{AstBlock, AstExpr, AstExprKind, AstStmt, AstStmtKind, Parser},
    visitor::{walk_block, walk_expr, walk_stmt, Visitor},
    CodeLocation,
};
use std::collections::HashMap;

use thiserror::Error;

#[derive(Default)]
struct ReturnOutsideFunctionChecker {
    errors: Vec<CodeLocation>,
}
impl<'ast> Visitor<'ast> for ReturnOutsideFunctionChecker {
    fn visit_stmt(&mut self, stmt: &'ast AstStmt) {
        if let AstStmtKind::FnDefnStmt(..) | AstStmtKind::ClassDefnStmt(..) = stmt.kind {
        } else {
            if let AstStmtKind::ReturnStmt(_) = stmt.kind {
                self.errors.push(stmt.location.clone());
            }
            walk_stmt(self, stmt);
        }
    }

    fn visit_expr(&mut self, expr: &'ast AstExpr) {
        if let AstExprKind::Lambda(..) = expr.kind {
        } else {
            walk_expr(self, expr);
        }
    }
}

#[derive(Default)]
struct UnreachableAfterReturnChecker {
    errors: Vec<CodeLocation>,
}
impl<'ast> Visitor<'ast> for UnreachableAfterReturnChecker {
    fn visit_block(&mut self, block: &AstBlock) {
        walk_block(self, block);
        let mut seen_return = false;
        for stmt in &block.stmts {
            if seen_return {
                self.errors.push(stmt.location.clone());
                return;
            }
            if let AstStmtKind::ReturnStmt(_) = stmt.kind {
                seen_return = true;
            }
        }
    }
}

#[derive(Default)]
struct DuplicateMethodsInClassChecker {
    errors: Vec<(CodeLocation, String, String)>,
}
impl<'ast> Visitor<'ast> for DuplicateMethodsInClassChecker {
    fn visit_stmt(&mut self, stmt: &'ast AstStmt) {
        match &stmt.kind {
            AstStmtKind::ClassDefnStmt(class_name, args) => {
                let mut map: HashMap<_, usize> = HashMap::new();
                args.iter()
                    .map(|(name, ..)| name)
                    .for_each(|name| *map.entry(name.clone()).or_default() += 1);
                map.into_iter()
                    .filter(|(_, count)| *count > 1)
                    .for_each(|(func_name, _)| {
                        self.errors
                            .push((stmt.location.clone(), class_name.clone(), func_name));
                    });
            }
            _ => walk_stmt(self, stmt),
        }
    }
}

pub fn static_check_for_errors<P: Parser>(p: &P) -> Vec<StaticCheckError> {
    let mut errors = Vec::new();
    errors.extend({
        let mut checker = ReturnOutsideFunctionChecker::default();
        checker.visit_stmt_list(p.get_tree());
        checker.errors.into_iter().map(|location| StaticCheckError {
            kind: StaticCheckErrorKind::ReturnOutsideFunction,
            location,
        })
    });
    errors.extend({
        let mut checker = UnreachableAfterReturnChecker::default();
        checker.visit_stmt_list(p.get_tree());
        checker.errors.into_iter().map(|location| StaticCheckError {
            kind: StaticCheckErrorKind::UnreachableAfterReturnChecker,
            location,
        })
    });
    errors.extend({
        let mut checker = DuplicateMethodsInClassChecker::default();
        checker.visit_stmt_list(p.get_tree());
        checker
            .errors
            .into_iter()
            .map(|(location, cls_name, func_name)| StaticCheckError {
                kind: StaticCheckErrorKind::DuplicateMethodNameInClass(cls_name, func_name),
                location,
            })
    });
    errors
}

#[derive(Clone, Debug, Error)]
#[error("{kind}\n{location}")]
pub struct StaticCheckError {
    pub kind: StaticCheckErrorKind,
    pub location: CodeLocation,
}

#[derive(Clone, Debug, Error)]
pub enum StaticCheckErrorKind {
    #[error("Return outside of a function")]
    ReturnOutsideFunction,
    #[error("Unreachable statement after return")]
    UnreachableAfterReturnChecker,
    #[error("Method {1} used multiple times in class {0}")]
    DuplicateMethodNameInClass(String, String),
}
