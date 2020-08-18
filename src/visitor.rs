use crate::parser::{AstBlock, AstExpr, AstExprKind, AstStmt, AstStmtKind};

/// A trait which walks through the AST Tree and does something
///
/// The methods default to walking through the tree recursively (using
/// the walk_* functions defined in this module, but not doing anything
/// with them.
pub trait Visitor<'ast>: Sized {
    fn visit_expr(&mut self, expr: &'ast AstExpr) {
        walk_expr(self, expr);
    }

    fn visit_stmt(&mut self, stmt: &'ast AstStmt) {
        walk_stmt(self, stmt);
    }

    fn visit_block(&mut self, block: &'ast AstBlock) {
        walk_block(self, block);
    }

    fn visit_stmt_list(&mut self, stmts: &'ast [AstStmt]) {
        stmts.iter().for_each(|stmt| self.visit_stmt(stmt));
    }
}

pub trait MutatingVisitor<'ast>: Sized {
    fn visit_expr(&mut self, expr: &'ast mut AstExpr) {
        walk_expr_mut(self, expr);
    }

    fn visit_stmt(&mut self, stmt: &'ast mut AstStmt) {
        walk_stmt_mut(self, stmt);
    }

    fn visit_block(&mut self, block: &'ast mut AstBlock) {
        walk_block_mut(self, block);
    }

    fn visit_stmt_list(&mut self, stmts: &'ast mut [AstStmt]) {
        stmts.iter_mut().for_each(|stmt| self.visit_stmt(stmt));
    }
}

/// Calls the appropriate visit function for each AST node directly
/// under the given expression
pub fn walk_expr_mut<'ast, V: MutatingVisitor<'ast>>(visitor: &mut V, expr: &'ast mut AstExpr) {
    use AstExprKind::*;
    match &mut expr.kind {
        BinOp { op: _, left, right } | Logical { op: _, left, right } => {
            visitor.visit_expr(left);
            visitor.visit_expr(right);
        }
        UnOp { op: _, arg: expr }
        | Assign {
            left: _,
            right: expr,
        } => {
            visitor.visit_expr(expr);
        }
        Call { callee, args }
        | New(callee, args)
        | MethodCall {
            callee,
            method_name: _,
            args,
        } => {
            visitor.visit_expr(callee);
            for arg in args {
                visitor.visit_expr(arg);
            }
        }
        Field {
            target,
            field_name: _,
        } => visitor.visit_expr(target),
        Leaf(_) | Error(_) => {}
        Lambda(_, body) => visitor.visit_block(body),
    }
}

/// Calls the appropriate visit function for each AST node directly
/// under the given statement
pub fn walk_stmt_mut<'ast, V: MutatingVisitor<'ast>>(visitor: &mut V, stmt: &'ast mut AstStmt) {
    use AstStmtKind::*;
    match &mut stmt.kind {
        ExprStmt(expr) | VarStmt(_, Some(expr)) | ReturnStmt(expr) => visitor.visit_expr(expr),
        BlockStmt(block) | FnDefnStmt(_, _, block) => visitor.visit_block(block),
        IfStmt(cond, left, Some(right)) => {
            visitor.visit_expr(cond);
            visitor.visit_block(left);
            visitor.visit_block(right);
        }
        IfStmt(cond, body, None) | WhileStmt(cond, body) => {
            visitor.visit_expr(cond);
            visitor.visit_block(body);
        }
        ErrorStmt(_) | VarStmt(_, None) | EmptyStmt | AstStmt(_) => {}
        ClassDefnStmt(_, ref mut methods) => {
            for method in methods {
                visitor.visit_block(&mut method.2);
            }
        }
    }
}

pub fn walk_block_mut<'ast, V: MutatingVisitor<'ast>>(visitor: &mut V, block: &'ast mut AstBlock) {
    for stmt in block.stmts.iter_mut() {
        visitor.visit_stmt(stmt);
    }
}

/// Calls the appropriate visit function for each AST node directly
/// under the given expression
pub fn walk_expr<'ast, V: Visitor<'ast>>(visitor: &mut V, expr: &'ast AstExpr) {
    use AstExprKind::*;
    match &expr.kind {
        BinOp { op: _, left, right } | Logical { op: _, left, right } => {
            visitor.visit_expr(left);
            visitor.visit_expr(right);
        }
        UnOp { op: _, arg: expr }
        | Assign {
            left: _,
            right: expr,
        } => {
            visitor.visit_expr(expr);
        }
        Call { callee, args }
        | New(callee, args)
        | MethodCall {
            callee,
            method_name: _,
            args,
        } => {
            visitor.visit_expr(callee);
            for arg in args {
                visitor.visit_expr(arg);
            }
        }
        Field {
            target,
            field_name: _,
        } => visitor.visit_expr(target),
        Leaf(_) | Error(_) => {}
        Lambda(_, body) => visitor.visit_block(body),
    }
}

/// Calls the appropriate visit function for each AST node directly
/// under the given statement
pub fn walk_stmt<'ast, V: Visitor<'ast>>(visitor: &mut V, stmt: &'ast AstStmt) {
    use AstStmtKind::*;
    match &stmt.kind {
        ExprStmt(expr) | VarStmt(_, Some(expr)) | ReturnStmt(expr) => visitor.visit_expr(expr),
        BlockStmt(block) | FnDefnStmt(_, _, block) => visitor.visit_block(block),
        IfStmt(cond, left, Some(right)) => {
            visitor.visit_expr(cond);
            visitor.visit_block(left);
            visitor.visit_block(right);
        }
        IfStmt(cond, body, None) | WhileStmt(cond, body) => {
            visitor.visit_expr(cond);
            visitor.visit_block(body);
        }
        ErrorStmt(_) | VarStmt(_, None) | EmptyStmt | AstStmt(_) => {}
        ClassDefnStmt(_, ref methods) => {
            for method in methods {
                visitor.visit_block(&method.2);
            }
        }
    }
}

pub fn walk_block<'ast, V: Visitor<'ast>>(visitor: &mut V, block: &'ast AstBlock) {
    for stmt in &block.stmts {
        visitor.visit_stmt(stmt);
    }
}
