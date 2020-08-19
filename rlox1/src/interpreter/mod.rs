use std::{
    collections::HashMap,
    fmt::{self, Debug, Display},
    marker::PhantomData,
};

use thiserror::Error;

mod envs;
mod native;
mod static_checker;

use envs::{Environment, GlobalEnvironment};

use super::{
    parser::{self, AstError, AstErrorKind, AstExprKind, AstLValue, LeafKind},
    CodeLocation, Lexer, Parser, Token, TokenKind,
};

pub trait Interpreter {
    /// Execute the following string as code
    fn run_code(&mut self, code: &str) -> Result<(), Vec<InterpreterError>>;
    /// Returns the result of evaluating an expression
    fn evaluate_expr(
        &mut self,
        expr: &parser::AstExpr,
    ) -> Result<LoxObject, (LoxExprError, CodeLocation)>;
    /// Evaluates the given statement and returns any error which may have been raised
    fn evaluate_stmt(
        &mut self,
        stmt: &parser::AstStmt,
        can_return: bool,
    ) -> Result<Option<LoxObject>, (LoxExprError, CodeLocation)>;
    /// Evaluates the given block and returns any error which may have been raised
    fn evaluate_block(
        &mut self,
        block: &parser::AstBlock,
    ) -> Result<(), (LoxExprError, CodeLocation)>;
    fn evaluate_function(
        &mut self,
        func: &LoxFunction,
        args: &[LoxObject],
    ) -> Result<LoxObject, (LoxExprError, CodeLocation)>;
}

#[derive(Debug)]
/// The interpreter for Lox that this crate is around
pub struct NativeInterpreter<L, P> {
    environment: GlobalEnvironment,
    l: PhantomData<L>,
    p: PhantomData<P>,
}

impl<L, P> NativeInterpreter<L, P> {
    /// Creates a new interpreter with a clean state
    pub fn new() -> NativeInterpreter<L, P> {
        NativeInterpreter {
            environment: GlobalEnvironment::with_globals(native::get_std_items()),
            l: PhantomData,
            p: PhantomData,
        }
    }
}

impl<L, P> Interpreter for NativeInterpreter<L, P>
where
    L: Lexer + 'static,
    P: Parser + 'static,
{
    fn evaluate_expr(
        &mut self,
        expr: &parser::AstExpr,
    ) -> Result<LoxObject, (LoxExprError, CodeLocation)> {
        Ok(match &expr.kind {
            AstExprKind::Leaf(leaf) => {
                use LeafKind::*;
                match leaf {
                    StringLiteral(s) => LoxObject::String(s.clone()),
                    IntegerLiteral(i) => LoxObject::Integer(*i),
                    FloatLiteral(f) => LoxObject::Float(*f),
                    True => LoxObject::Boolean(true),
                    False => LoxObject::Boolean(false),
                    Nil => LoxObject::Nil,
                    Identifier(id) => self.environment.get(id).ok_or_else(
                        || (
                            LoxExprError::UndefinedIdentifier(id.clone()),
                            expr.location.clone(),
                        ),
                    )?.clone(),
                }
            }
            AstExprKind::UnOp { op, arg } => {
                use parser::UnOpKind::*;
                match op {
                    Neg => self
                        .evaluate_expr(arg)?
                        .negate()
                        .map_err(|e| (e, expr.location.clone()))?,
                    Not => self.evaluate_expr(arg)?.not(),
                }
            }
            AstExprKind::BinOp { op, left, right } => {
                use parser::BinOpKind::*;
                match op {
                    Add => LoxObject::add(&self.evaluate_expr(left)?, &self.evaluate_expr(right)?)
                        .map_err(|e| (e, expr.location.clone()))?,
                    Sub => LoxObject::sub(&self.evaluate_expr(left)?, &self.evaluate_expr(right)?)
                        .map_err(|e| (e, expr.location.clone()))?,
                    Mul => LoxObject::mul(&self.evaluate_expr(left)?, &self.evaluate_expr(right)?)
                        .map_err(|e| (e, expr.location.clone()))?,
                    Mod => {
                        LoxObject::modulo(&self.evaluate_expr(left)?, &self.evaluate_expr(right)?)
                            .map_err(|e| (e, expr.location.clone()))?
                    }
                    Div => LoxObject::div(&self.evaluate_expr(left)?, &self.evaluate_expr(right)?)
                        .map_err(|e| (e, expr.location.clone()))?,
                    Eq => LoxObject::eq(&self.evaluate_expr(left)?, &self.evaluate_expr(right)?),
                    Ne => LoxObject::ne(&self.evaluate_expr(left)?, &self.evaluate_expr(right)?),
                    Gt => LoxObject::gt(&self.evaluate_expr(left)?, &self.evaluate_expr(right)?)
                        .map_err(|e| (e, expr.location.clone()))?,
                    Ge => LoxObject::ge(&self.evaluate_expr(left)?, &self.evaluate_expr(right)?)
                        .map_err(|e| (e, expr.location.clone()))?,
                    Lt => LoxObject::lt(&self.evaluate_expr(left)?, &self.evaluate_expr(right)?)
                        .map_err(|e| (e, expr.location.clone()))?,
                    Le => LoxObject::le(&self.evaluate_expr(left)?, &self.evaluate_expr(right)?)
                        .map_err(|e| (e, expr.location.clone()))?,
                }
            }
            AstExprKind::Assign { left, right } => {
                let value = self.evaluate_expr(right)?;
                match left {
                    AstLValue::Identifier(ident) => {
                        self.environment
                            .assign(ident, value.clone())
                            .map_err(|()| {
                                (
                                    LoxExprError::UndefinedIdentifier(ident.clone()),
                                    expr.location.clone(),
                                )
                            })?;
                    }
                    AstLValue::Field(object, identifier) => {
                        self.get_recursive(object)
                            .map_or_else(
                                || {
                                    Err((
                                        LoxExprError::UndefinedIdentifier(format!("{:?}", object)),
                                        expr.location.clone(),
                                    ))
                                },
                                |obj| Ok(obj),
                            )?
                            .set_field(identifier.clone(), value.clone())
                            .map_or_else(
                                || {
                                    Err((
                                        LoxExprError::TypeWeirdness(
                                            "Only objects support field assignment".to_string(),
                                        ),
                                        expr.location.clone(),
                                    ))
                                },
                                |()| Ok(()),
                            )?;
                    }
                }
                value
            }
            AstExprKind::Logical { op, left, right } => {
                use parser::LogicalOpKind;
                match op {
                    LogicalOpKind::And => {
                        let left_value = self.evaluate_expr(left)?;
                        if left_value.is_truthy() {
                            self.evaluate_expr(right)?
                        } else {
                            left_value
                        }
                    }
                    LogicalOpKind::Or => {
                        let left_value = self.evaluate_expr(left)?;
                        if left_value.is_truthy() {
                            left_value
                        } else {
                            self.evaluate_expr(right)?
                        }
                    }
                }
            }
            AstExprKind::MethodCall {
                callee,
                method_name,
                args,
            } => {
                let object = self.evaluate_expr(callee)?;
                let func = match object.get_field(method_name).map(LoxObject::callable) {
                    Some(Some(func)) => func,
                    Some(None) => {
                        return Err((
                            LoxExprError::CallOnNonFunction(
                                object
                                    .get_field(method_name)
                                    .unwrap()
                                    .type_str()
                                    .to_string(),
                            ),
                            callee.location.clone(),
                        ))
                    }
                    None => return Err((LoxExprError::NoSuchField, callee.location.clone())),
                };
                if args.len() + 1 != func.arity() {
                    return Err((
                        LoxExprError::WrongNumArgs(func.arity(), args.len() + 1),
                        callee.location.clone(),
                    ));
                }
                let _arg_values = args
                    .iter()
                    .map(|arg_expr| self.evaluate_expr(arg_expr))
                    .collect::<Result<Vec<_>, _>>()?;
                unimplemented!()
                // func.method_call(self, &mut object, &arg_values)?
            }
            AstExprKind::Call { callee, args } => {
                let object = self.evaluate_expr(callee)?;
                let func = match object.callable() {
                    Some(func) => func,
                    None => {
                        return Err((
                            LoxExprError::CallOnNonFunction(object.type_str().to_string()),
                            callee.location.clone(),
                        ))
                    }
                };
                if args.len() != func.arity() {
                    return Err((
                        LoxExprError::WrongNumArgs(func.arity(), args.len()),
                        callee.location.clone(),
                    ));
                }
                let arg_values = args
                    .iter()
                    .map(|arg_expr| self.evaluate_expr(arg_expr))
                    .collect::<Result<Vec<_>, _>>()?;
                func.call(self, &arg_values)?
            }
            AstExprKind::Lambda(args, body) => LoxObject::Function(LoxFunction {
                param_list: args.clone(),
                body: body.clone(),
            }),
            AstExprKind::New(class, args) => {
                let class = self.evaluate_expr(class)?;
                let args = args
                    .iter()
                    .map(|expr| self.evaluate_expr(expr))
                    .collect::<Result<Vec<_>, _>>()?;
                class.initialize(self, &args)?
            }
            AstExprKind::Field { target, field_name } => {
                match self.evaluate_expr(target)?.get_field(field_name) {
                    Some(value) => value.clone(),
                    None => return Err((LoxExprError::NoSuchField, expr.location.clone())),
                }
            }
            AstExprKind::Error(_) => unreachable!(),
        })
    }

    fn evaluate_stmt(
        &mut self,
        stmt: &parser::AstStmt,
        can_return: bool,
    ) -> Result<Option<LoxObject>, (LoxExprError, CodeLocation)> {
        use parser::AstStmtKind::*;
        match &stmt.kind {
            AstStmt(stmt) => println!("{:#?}", stmt),
            ExprStmt(expr) => {
                self.evaluate_expr(expr)?;
            }
            VarStmt(id, Some(expr)) => {
                let value = self.evaluate_expr(expr)?;
                self.environment.insert(id.clone(), value);
            }
            VarStmt(id, None) => {
                self.environment.insert(id.clone(), LoxObject::Nil);
            }
            BlockStmt(block) => {
                if can_return {
                    if let Some(ret_val) = self.evaluate_function_block(block)? {
                        return Ok(Some(ret_val));
                    }
                } else {
                    self.evaluate_block(block)?;
                }
            }
            IfStmt(cond, if_block, optional_else_block) => {
                if self.evaluate_expr(cond)?.is_truthy() {
                    if can_return {
                        if let Some(ret_val) = self.evaluate_function_block(if_block)? {
                            return Ok(Some(ret_val));
                        }
                    } else {
                        self.evaluate_block(if_block)?;
                    }
                } else if let Some(else_block) = optional_else_block {
                    if can_return {
                        if let Some(ret_val) = self.evaluate_function_block(else_block)? {
                            return Ok(Some(ret_val));
                        }
                    } else {
                        self.evaluate_block(else_block)?;
                    }
                }
            }
            WhileStmt(cond, block) => {
                while self.evaluate_expr(cond)?.is_truthy() {
                    if can_return {
                        if let Some(ret_val) = self.evaluate_function_block(block)? {
                            return Ok(Some(ret_val));
                        }
                    } else {
                        self.evaluate_block(block)?;
                    }
                }
            }
            ReturnStmt(expr) => {
                if can_return {
                    return Ok(Some(self.evaluate_expr(expr)?));
                } else {
                    return Err((LoxExprError::ReturnOutsideFunction, expr.location.clone()));
                }
            }
            FnDefnStmt(name, args, body) => {
                self.environment.insert(
                    name.clone(),
                    LoxObject::Function(LoxFunction {
                        param_list: args.clone(),
                        body: body.clone(),
                    }),
                );
            }
            ClassDefnStmt(name, funcs) => {
                self.environment.insert(
                    name.clone(),
                    LoxObject::Class(
                        funcs
                            .iter()
                            .cloned()
                            .map(|(name, param_list, body)| {
                                (name, LoxObject::Method(LoxMethod { param_list, body }))
                            })
                            .collect(),
                    ),
                );
            }
            EmptyStmt => {}
            ErrorStmt(_) => unreachable!(),
        }
        Ok(None)
    }

    fn evaluate_block(
        &mut self,
        block: &parser::AstBlock,
    ) -> Result<(), (LoxExprError, CodeLocation)> {
        self.environment.push_scope();
        for stmt in &block.stmts {
            self.evaluate_stmt(stmt, false)?;
        }
        self.environment.pop_scope();
        Ok(())
    }

    fn evaluate_function(
        &mut self,
        func: &LoxFunction,
        args: &[LoxObject],
    ) -> Result<LoxObject, (LoxExprError, CodeLocation)> {
        self.environment.push_function_environment();
        for (name, args) in func.param_list.iter().zip(args.iter()) {
            self.environment.insert(name.clone(), args.clone());
        }
        let result = self.evaluate_function_block(&func.body);
        self.environment.pop_function_environment();
        result.map(|result| result.map_or(LoxObject::Nil, |x| x))
    }

    fn run_code(&mut self, code: &str) -> Result<(), Vec<InterpreterError>> {
        let lexer = L::new(code);
        if lexer.found_error() {
            let lines = code
                .lines()
                .map(str::trim)
                .map(String::from)
                .collect::<Vec<String>>();
            return Err(lexer
                .tokens()
                .iter()
                .filter_map(|token| match &token.kind {
                    TokenKind::StringLiteralError(malformed_literal) => Some(InterpreterError {
                        kind: InterpreterErrorKind::UnterminatedStringLiteral(
                            malformed_literal.clone(),
                        ),
                        location: token
                            .location
                            .add_line(lines[token.location.get_line_num().unwrap() - 1].clone()),
                    }),
                    TokenKind::IllegalCharacterError(_) => Some(InterpreterError {
                        kind: InterpreterErrorKind::IllegalToken(token.clone()),
                        location: token
                            .location
                            .add_line(lines[token.location.get_line_num().unwrap() - 1].clone()),
                    }),
                    _ => None,
                })
                .collect());
        }
        let parser = P::new(lexer.tokens());
        if parser.has_errors() {
            let lines = code
                .lines()
                .map(str::trim)
                .map(String::from)
                .collect::<Vec<String>>();
            return Err(parser
                .get_errors()
                .iter()
                .cloned()
                .cloned()
                .map(InterpreterError::from)
                .map(|mut e| {
                    e.add_line_from_slice(&lines);
                    e
                })
                .collect());
        }
        let static_errors = static_checker::static_check_for_errors(&parser);
        if !static_errors.is_empty() {
            let lines = code
                .lines()
                .map(str::trim)
                .map(String::from)
                .collect::<Vec<String>>();
            return Err(static_errors
                .into_iter()
                .map(InterpreterError::from)
                .map(|mut e| {
                    e.add_line_from_slice(&lines);
                    e
                })
                .collect());
        }
        self.evaluate_stmts(parser.get_tree())
            .map_err(|(e, location)| {
                let lines = code
                    .lines()
                    .map(str::trim)
                    .map(String::from)
                    .collect::<Vec<String>>();
                vec![InterpreterError {
                    kind: InterpreterErrorKind::ExprEvalError(e),
                    location: location.add_line_from_slice(&lines),
                }]
            })
    }
}
impl<L, P> NativeInterpreter<L, P>
where
    L: Lexer + 'static,
    P: Parser + 'static,
{
    fn evaluate_stmts(
        &mut self,
        stmts: &[parser::AstStmt],
    ) -> Result<(), (LoxExprError, CodeLocation)> {
        for stmt in stmts {
            self.evaluate_stmt(stmt, false)?;
        }
        Ok(())
    }

    /// Evaluate the given block of code as a function, and return its result
    fn evaluate_function_block(
        &mut self,
        block: &parser::AstBlock,
    ) -> Result<Option<LoxObject>, (LoxExprError, CodeLocation)> {
        self.environment.push_scope();
        for stmt in &block.stmts {
            match self.evaluate_stmt(stmt, true)? {
                Some(object) => return Ok(Some(object)),
                None => {}
            }
        }
        self.environment.pop_scope();
        Ok(None)
    }

    fn get_recursive(&mut self, object: &AstLValue) -> Option<&mut LoxObject> {
        match object {
            AstLValue::Identifier(ident) => self.environment.get_mut(ident),
            AstLValue::Field(object, field) => self
                .get_recursive(object)
                .and_then(|object| object.get_field_mut(field)),
        }
    }
}
impl<L, P> Default for NativeInterpreter<L, P> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum LoxObject {
    Integer(isize),
    Float(f64),
    String(String),
    Boolean(bool),
    Nil,
    Function(LoxFunction),
    Method(LoxMethod),
    Native(NativeFunction),
    Object(HashMap<String, LoxObject>),
    Class(HashMap<String, LoxObject>),
}
impl LoxObject {
    /// Gets the field with the given name in the given object, mutably
    fn get_field_mut(&mut self, name: &str) -> Option<&mut LoxObject> {
        match self {
            LoxObject::Object(fields) => fields.get_mut(name),
            _ => None,
        }
    }
    /// Gets the field with the given name in the given object
    fn get_field(&self, name: &str) -> Option<&LoxObject> {
        match self {
            LoxObject::Object(fields) => fields.get(name),
            _ => None,
        }
    }
    /// Sets the value of the given field to the given value, if this is
    /// an object.
    ///
    /// If this method is called on a non-object, then it returns `None`
    /// and does not change the object.
    fn set_field(&mut self, name: String, value: LoxObject) -> Option<()> {
        match self {
            LoxObject::Object(fields) => {
                fields.insert(name, value);
                Some(())
            }
            _ => None,
        }
    }
    /// If the given object is a class, it will initialize a new
    /// instance of that class. An object is considered to be a class if
    /// it has a function whose name is `$new`.
    ///
    /// When a class is defined, it has a `$new` function which creates
    /// a new object, adds all of its methods to the object (except
    /// `$new` itself), and then calls the new object's `$init`
    /// function with the arguments that it is given. However, this
    /// behavior may be changed by specifying a custom `$new` function.
    fn initialize(
        &self,
        interpreter: &mut dyn Interpreter,
        args: &[LoxObject],
    ) -> Result<LoxObject, (LoxExprError, CodeLocation)> {
        match self {
            LoxObject::Class(funcs) => {
                match funcs.get("$new") {
                    Some(func) => {
                        if let Some(init_func) = func.callable() {
                            init_func.call(interpreter, args)
                        } else {
                            unreachable!("`$new` has been overwritten, but is no longer callable")
                        }
                    }
                    None => {
                        let obj = LoxObject::Object(funcs.clone());
                        // TODO Call the `$init` function
                        Ok(obj)
                    }
                }
            }
            LoxObject::Object(_) => {
                if let Some(init_func) = self.get_field("$new") {
                    if let Some(init_func) = init_func.callable() {
                        init_func.call(interpreter, args)
                    } else {
                        unimplemented!("New on non-object type")
                    }
                } else {
                    unimplemented!("New on non-object type")
                }
            }
            _ => unimplemented!("New on non-object type"),
        }
    }
    /// Turn this object into a callable. A function or native function
    /// will be turned into the contained callable. An object will
    /// retrieve the callable in its `$call` field (or return None if
    /// no such callable exists).
    fn callable(&self) -> Option<&dyn LoxCallable> {
        use LoxObject::*;
        match self {
            Function(f) => Some(f),
            Method(f) => Some(f),
            Native(f) => Some(f),
            Object(fields) => fields.get("$call").and_then(|f| f.callable()),
            _ => None,
        }
    }
    /// Get a string representation of this LoxObject's type. Primitives
    /// have a pre-defined type. Objects can override this by placing a
    /// value in their `$type` field (which defaults to the name of the
    /// class which it is instantiated from).
    fn type_str(&self) -> String {
        use LoxObject::*;
        match self {
            Integer(_) => "Integer".to_string(),
            Float(_) => "Float".to_string(),
            String(_) => "String".to_string(),
            Boolean(_) => "Boolean".to_string(),
            Nil => "Nil".to_string(),
            Function(_) => "Function".to_string(),
            Method(_) => "Method".to_string(),
            Native(_) => "Native Function".to_string(),
            Object(fields) => fields
                .get("$type")
                .map_or("object".to_string(), |class| format!("{}", class)),
            Class(_) => "class".to_string(),
        }
    }
    fn not(&self) -> Self {
        LoxObject::Boolean(!self.is_truthy())
    }
    fn is_truthy(&self) -> bool {
        use LoxObject::*;
        match self {
            Nil => false,
            Boolean(b) => *b,
            Integer(i) => *i != 0,
            Float(f) => *f != 0.0,
            String(s) => !s.is_empty(),
            Function(_) => true,
            Method(_) => true,
            Native(_) => true,
            Object(_) => true,
            Class(_) => true,
        }
    }
    fn negate(&self) -> FallibleLoxObject {
        Ok(match self {
            LoxObject::Nil => LoxObject::Nil,
            LoxObject::Boolean(b) => LoxObject::Integer(if *b { -1 } else { 0 }),
            LoxObject::Integer(i) => LoxObject::Integer(-i),
            LoxObject::Float(f) => LoxObject::Float(-f),
            _ => {
                return Err(LoxExprError::TypeWeirdness(format!(
                    "Can't negate a {}",
                    self.type_str()
                )))
            }
        })
    }
    fn add(left: &LoxObject, right: &LoxObject) -> FallibleLoxObject {
        use LoxObject::*;
        Ok(match (left, right) {
            (Float(a), Float(b)) => Float(a + b),
            (Float(a), Integer(b)) => Float(a + *b as f64),
            (Integer(a), Float(b)) => Float(*a as f64 + b),
            (Float(a), Boolean(b)) => Float(a + if *b { 1.0 } else { 0.0 }),
            (Boolean(a), Float(b)) => Float(if *a { 1.0 } else { 0.0 } + b),
            (Integer(a), Integer(b)) => Integer(a + b),
            (Integer(a), Boolean(b)) => Integer(a + if *b { 1 } else { 0 }),
            (Boolean(a), Integer(b)) => Integer(if *a { 1 } else { 0 } + b),
            (String(a), String(b)) => String(a.to_owned() + b),
            _ => {
                return Err(LoxExprError::TypeWeirdness(format!(
                    "Can't add {} to {}",
                    left.type_str(),
                    right.type_str()
                )))
            }
        })
    }
    fn sub(left: &LoxObject, right: &LoxObject) -> FallibleLoxObject {
        use LoxObject::*;
        Ok(match (left, right) {
            (Float(a), Float(b)) => Float(a - b),
            (Float(a), Integer(b)) => Float(a - *b as f64),
            (Integer(a), Float(b)) => Float(*a as f64 - b),
            (Float(a), Boolean(b)) => Float(a - if *b { 1.0 } else { 0.0 }),
            (Boolean(a), Float(b)) => Float(if *a { 1.0 } else { 0.0 } - b),
            (Integer(a), Integer(b)) => Integer(a - b),
            (Integer(a), Boolean(b)) => Integer(a - if *b { 1 } else { 0 }),
            (Boolean(a), Integer(b)) => Integer(if *a { 1 } else { 0 } - b),
            _ => {
                return Err(LoxExprError::TypeWeirdness(format!(
                    "Can't subtract {} from {}",
                    right.type_str(),
                    left.type_str()
                )))
            }
        })
    }
    fn mul(left: &LoxObject, right: &LoxObject) -> FallibleLoxObject {
        use LoxObject::*;
        Ok(match (left, right) {
            (Float(a), Float(b)) => Float(a * b),
            (Float(a), Integer(b)) => Float(a * *b as f64),
            (Integer(a), Float(b)) => Float(*a as f64 * b),
            (Float(a), Boolean(b)) => Float(a * if *b { 1.0 } else { 0.0 }),
            (Boolean(a), Float(b)) => Float(if *a { 1.0 } else { 0.0 } * b),
            (Integer(a), Integer(b)) => Integer(a * b),
            (Integer(a), Boolean(b)) => Integer(a * if *b { 1 } else { 0 }),
            (Boolean(a), Integer(b)) => Integer(if *a { 1 } else { 0 } * b),
            _ => {
                return Err(LoxExprError::TypeWeirdness(format!(
                    "Can't multiply {} by {}",
                    left.type_str(),
                    right.type_str()
                )))
            }
        })
    }
    fn div(left: &LoxObject, right: &LoxObject) -> FallibleLoxObject {
        use LoxObject::*;
        Ok(match (left, right) {
            (Float(a), Float(b)) => Float(a / b),
            (Float(a), Integer(b)) => Float(a / *b as f64),
            (Integer(a), Float(b)) => Float(*a as f64 / b),
            (Float(a), Boolean(b)) => Float(a / if *b { 1.0 } else { 0.0 }),
            (Boolean(a), Float(b)) => Float(if *a { 1.0 } else { 0.0 } / b),
            (_, Integer(0)) => return Err(LoxExprError::DivideByZero),
            (Integer(a), Integer(b)) => Integer(a / b),
            (Integer(a), Boolean(true)) => Integer(*a),
            (_, Boolean(false)) => return Err(LoxExprError::DivideByZero),
            (Boolean(true), Integer(b)) => Integer(1 / b),
            (Boolean(false), Integer(_)) => Integer(0),
            _ => {
                return Err(LoxExprError::TypeWeirdness(format!(
                    "Can't divide {} by {}",
                    left.type_str(),
                    right.type_str()
                )))
            }
        })
    }
    fn modulo(left: &LoxObject, right: &LoxObject) -> FallibleLoxObject {
        use LoxObject::*;
        Ok(match (left, right) {
            (Float(a), Float(b)) => Float(a % b),
            (Float(a), Integer(b)) => Float(a % *b as f64),
            (Integer(a), Float(b)) => Float(*a as f64 % b),
            (Float(a), Boolean(b)) => Float(a % if *b { 1.0 } else { 0.0 }),
            (Boolean(a), Float(b)) => Float(if *a { 1.0 } else { 0.0 } % b),
            (_, Integer(0)) => return Err(LoxExprError::DivideByZero),
            (Integer(a), Integer(b)) => Integer(a % b),
            (Integer(a), Boolean(true)) => Integer(*a),
            (_, Boolean(false)) => return Err(LoxExprError::DivideByZero),
            (Boolean(true), Integer(b)) => Integer(1 % b),
            (Boolean(false), Integer(_)) => Integer(0),
            _ => {
                return Err(LoxExprError::TypeWeirdness(format!(
                    "Can't modulus {} by {}",
                    left.type_str(),
                    right.type_str()
                )))
            }
        })
    }
    fn eq(left: &LoxObject, right: &LoxObject) -> Self {
        LoxObject::Boolean(left == right)
    }
    fn ne(left: &LoxObject, right: &LoxObject) -> Self {
        LoxObject::Boolean(left != right)
    }
    fn gt(left: &LoxObject, right: &LoxObject) -> FallibleLoxObject {
        use LoxObject::*;
        Ok(Boolean(match (left, right) {
            (Integer(a), Integer(b)) => a > b,
            (Float(a), Float(b)) => a > b,
            (String(a), String(b)) => a > b,
            _ => {
                return Err(LoxExprError::TypeWeirdness(format!(
                    "Can't compare {} to {}",
                    left.type_str(),
                    right.type_str()
                )))
            }
        }))
    }
    fn ge(left: &LoxObject, right: &LoxObject) -> FallibleLoxObject {
        use LoxObject::*;
        Ok(Boolean(match (left, right) {
            (Integer(a), Integer(b)) => a >= b,
            (Float(a), Float(b)) => a >= b,
            (String(a), String(b)) => a >= b,
            _ => {
                return Err(LoxExprError::TypeWeirdness(format!(
                    "Can't compare {} to {}",
                    left.type_str(),
                    right.type_str()
                )))
            }
        }))
    }
    fn lt(left: &LoxObject, right: &LoxObject) -> FallibleLoxObject {
        use LoxObject::*;
        Ok(Boolean(match (left, right) {
            (Integer(a), Integer(b)) => a < b,
            (Float(a), Float(b)) => a < b,
            (String(a), String(b)) => a < b,
            _ => {
                return Err(LoxExprError::TypeWeirdness(format!(
                    "Can't compare {} to {}",
                    left.type_str(),
                    right.type_str()
                )))
            }
        }))
    }
    fn le(left: &LoxObject, right: &LoxObject) -> FallibleLoxObject {
        use LoxObject::*;
        Ok(Boolean(match (left, right) {
            (Integer(a), Integer(b)) => a <= b,
            (Float(a), Float(b)) => a <= b,
            (String(a), String(b)) => a <= b,
            _ => {
                return Err(LoxExprError::TypeWeirdness(format!(
                    "Can't compare {} to {}",
                    left.type_str(),
                    right.type_str()
                )))
            }
        }))
    }
}
impl Display for LoxObject {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use LoxObject::*;
        write!(
            f,
            "{}",
            match self {
                Integer(i) => format!("{}", i),
                Float(f) => format!("{}", f),
                String(s) => s.clone(),
                Boolean(b) => format!("{}", b),
                Nil => "nil".to_string(),
                Function(f) => format!("{:?}", f),
                Method(f) => format!("{:?}", f),
                Native(f) => format!("{:?}", f),
                Object(fields) => format!("{:?}", fields),
                Class(fields) => format!("{:?}", fields),
            }
        )
    }
}

trait LoxCallable {
    fn arity(&self) -> usize;

    fn call(
        &self,
        interpreter: &mut dyn Interpreter,
        args: &[LoxObject],
    ) -> Result<LoxObject, (LoxExprError, CodeLocation)>;

    fn method_call(
        &self,
        interpreter: &mut dyn Interpreter,
        callee: &mut LoxObject,
        args: &[LoxObject],
    ) -> Result<LoxObject, (LoxExprError, CodeLocation)> {
        let args = Some(callee.clone())
            .into_iter()
            .chain(args.iter().cloned())
            .collect::<Vec<_>>();
        self.call(interpreter, &args)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct LoxFunction {
    param_list: Vec<String>,
    body: parser::AstBlock,
}
impl LoxCallable for LoxFunction {
    fn arity(&self) -> usize {
        self.param_list.len()
    }

    fn call(
        &self,
        interpreter: &mut dyn Interpreter,
        args: &[LoxObject],
    ) -> Result<LoxObject, (LoxExprError, CodeLocation)> {
        interpreter.evaluate_function(&self, args)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct LoxMethod {
    param_list: Vec<String>,
    body: parser::AstBlock,
}
impl LoxCallable for LoxMethod {
    fn arity(&self) -> usize {
        self.param_list.len() + 1
    }

    fn call(
        &self,
        _interpreter: &mut dyn Interpreter,
        _args: &[LoxObject],
    ) -> Result<LoxObject, (LoxExprError, CodeLocation)> {
        unimplemented!("class methods")
    }

    fn method_call(
        &self,
        _interpreter: &mut dyn Interpreter,
        _callee: &mut LoxObject,
        _args: &[LoxObject],
    ) -> Result<LoxObject, (LoxExprError, CodeLocation)> {
        unimplemented!("class methods")
    }
}

#[derive(Clone)]
pub struct NativeFunction {
    arity: usize,
    func: &'static dyn Fn(
        &mut dyn Interpreter,
        &[LoxObject],
    ) -> Result<LoxObject, (LoxExprError, CodeLocation)>,
    debug: &'static str,
}
impl LoxCallable for NativeFunction {
    fn arity(&self) -> usize {
        self.arity
    }

    fn call(
        &self,
        interpreter: &mut dyn Interpreter,
        args: &[LoxObject],
    ) -> Result<LoxObject, (LoxExprError, CodeLocation)> {
        (self.func)(interpreter, args)
    }
}
impl Debug for NativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<Native Function: {}>", self.debug)
    }
}
impl PartialEq for NativeFunction {
    fn eq(&self, other: &NativeFunction) -> bool {
        self.arity == other.arity && self.debug == other.debug
    }
}

type FallibleLoxObject = Result<LoxObject, LoxExprError>;

#[derive(Clone, Debug, Eq, Hash, PartialEq, Error)]
pub enum LoxExprError {
    #[error("Type Error: {0}")]
    TypeWeirdness(String),
    #[error("Divide by zero")]
    DivideByZero,
    #[error("Name `{0}` is not defined")]
    UndefinedIdentifier(String),
    #[error("Type `{0}` is not callable")]
    CallOnNonFunction(String),
    #[error("Function has arity {0}, but was called with {1} args")]
    WrongNumArgs(usize, usize),
    #[error("Can only return inside a function")]
    ReturnOutsideFunction,
    #[error("Can't find given field on object")]
    NoSuchField,
    #[error("{0}")]
    /// Catch-all for kinds of errors not in here
    RuntimeError(String),
    #[error("Assertion failed: {0}")]
    AssertionFailed(String),
}

#[derive(Debug, Clone, Error)]
#[error("Error: {kind}\n{location}")]
pub struct InterpreterError {
    kind: InterpreterErrorKind,
    location: CodeLocation,
}

impl InterpreterError {
    fn add_line_from_slice(&mut self, slice: &[String]) {
        self.location = self.location.add_line_from_slice(slice);
    }
}

#[derive(Debug, Clone)]
enum InterpreterErrorKind {
    UnterminatedStringLiteral(String),
    IllegalToken(Token),
    ParserError(AstErrorKind),
    StaticError(static_checker::StaticCheckErrorKind),
    ExprEvalError(LoxExprError),
}
impl Display for InterpreterErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                InterpreterErrorKind::UnterminatedStringLiteral(lit) =>
                    format!("Unterminated string literal: {}", lit),
                InterpreterErrorKind::IllegalToken(token) => format!("Illegal token: {:?}", token),
                InterpreterErrorKind::ParserError(e) => format!("Parsing error: {}", e),
                InterpreterErrorKind::StaticError(e) => format!("{}", e),
                InterpreterErrorKind::ExprEvalError(e) => format!("{}", e),
            }
        )
    }
}

impl From<static_checker::StaticCheckError> for InterpreterError {
    fn from(e: static_checker::StaticCheckError) -> InterpreterError {
        InterpreterError {
            kind: InterpreterErrorKind::StaticError(e.kind),
            location: e.location,
        }
    }
}
impl From<AstError> for InterpreterError {
    fn from(e: AstError) -> InterpreterError {
        InterpreterError {
            kind: InterpreterErrorKind::ParserError(e.kind),
            location: e.location,
        }
    }
}
