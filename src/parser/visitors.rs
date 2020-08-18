use super::{AstError, AstExpr, AstExprKind, AstStmt, AstStmtKind};
use crate::visitor::{walk_expr, walk_expr_mut, walk_stmt, MutatingVisitor, Visitor};

/// A Visitor which checks the AST for errors
pub struct ErrorCheckingVisitor {
    seen_errors: bool,
}
impl ErrorCheckingVisitor {
    #![allow(dead_code)]
    pub fn new() -> ErrorCheckingVisitor {
        ErrorCheckingVisitor { seen_errors: false }
    }

    /// Returns true iff an error is found in the AST rooted at the
    /// given stmt.
    pub fn check_stmt_for_errors(stmt: &AstStmt) -> bool {
        let mut visitor = ErrorCheckingVisitor::new();
        visitor.visit_stmt(stmt);
        visitor.seen_errors
    }

    /// Returns true iff an error is found in the AST rooted at the
    /// given expr.
    pub fn check_expr_for_errors(expr: &AstExpr) -> bool {
        let mut visitor = ErrorCheckingVisitor::new();
        visitor.visit_expr(expr);
        visitor.seen_errors
    }
}
impl<'ast> Visitor<'ast> for ErrorCheckingVisitor {
    fn visit_expr(&mut self, expr: &'ast AstExpr) {
        if !self.seen_errors {
            match &expr.kind {
                AstExprKind::Error(_) => self.seen_errors = true,
                _ => walk_expr(self, expr),
            }
        }
    }

    fn visit_stmt(&mut self, stmt: &'ast AstStmt) {
        if !self.seen_errors {
            match &stmt.kind {
                AstStmtKind::ErrorStmt(_) => self.seen_errors = true,
                _ => walk_stmt(self, stmt),
            }
        }
    }
}

pub struct ErrorListingVisitor<'ast> {
    errors: Vec<&'ast AstError>,
}
impl<'ast> ErrorListingVisitor<'ast> {
    #![allow(dead_code)]
    pub fn new() -> ErrorListingVisitor<'ast> {
        ErrorListingVisitor { errors: Vec::new() }
    }

    /// Returns a vector of all errors in the AST rooted at the given
    /// statement
    pub fn list_errors_in_stmt(stmt: &'ast AstStmt) -> Vec<&'ast AstError> {
        let mut visitor = ErrorListingVisitor::new();
        visitor.visit_stmt(stmt);
        visitor.errors
    }

    /// Returns a vector of all errors in the AST rooted at the given
    /// expression
    pub fn list_errors_in_expr(expr: &'ast AstExpr) -> Vec<&'ast AstError> {
        let mut visitor = ErrorListingVisitor::new();
        visitor.visit_expr(expr);
        visitor.errors
    }
}
impl<'ast> Visitor<'ast> for ErrorListingVisitor<'ast> {
    fn visit_expr(&mut self, expr: &'ast AstExpr) {
        if let AstExprKind::Error(e) = &expr.kind {
            self.errors.push(e);
        }
        walk_expr(self, expr);
    }

    fn visit_stmt(&mut self, stmt: &'ast AstStmt) {
        if let AstStmtKind::ErrorStmt(e) = &stmt.kind {
            self.errors.push(e);
        }
        walk_stmt(self, stmt);
    }
}

/// A visitor which replaces calls on object fields with method calls
pub struct MethodCallMutator;
impl MethodCallMutator {
    pub fn mutate_code(code: &mut [AstStmt]) {
        MethodCallMutator {}.visit_stmt_list(code);
    }
}
impl<'ast> MutatingVisitor<'ast> for MethodCallMutator {
    fn visit_expr(&mut self, expr: &'ast mut AstExpr) {
        if let AstExprKind::Call { callee, args } = &mut expr.kind {
            if let AstExprKind::Field { target, field_name } = &mut callee.kind {
                *expr = AstExpr {
                    kind: AstExprKind::MethodCall {
                        callee: target.clone(),
                        method_name: field_name.clone(),
                        args: args.clone(),
                    },
                    location: expr.location.clone(),
                };
            }
        }
        walk_expr_mut(self, expr);
    }
}
