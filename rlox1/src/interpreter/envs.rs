use std::collections::HashMap;

use super::LoxObject;

pub trait Environment {
    /// Create a new variable in the lowest-level scope that currently
    /// exists
    fn insert(&mut self, identifier: String, value: LoxObject);
    /// Get the value of the given identifier in the inner-most scope
    /// which contains a value for that identifier, mutably.
    fn get_mut(&mut self, identifier: &str) -> Option<&mut LoxObject>;
    /// Get the value of the given identifier in the inner-most scope
    /// which contains a value for that identifier.
    fn get(&self, identifier: &str) -> Option<&LoxObject>;
    /// Add a new empty scope to the stack
    fn push_scope(&mut self);
    /// Remove the top scope from the stack and return it (if a
    /// non-global scope previously exists).
    fn pop_scope(&mut self) -> Option<HashMap<String, LoxObject>>;
    /// Assigna value to the variable in the inner-most scope which
    /// defines it (if such a scope exists). Returns Ok if it exists
    /// somewhere to assign to, or Err if it does not exist.
    fn assign(&mut self, identifier: &str, value: LoxObject) -> Result<(), ()>;
}

#[derive(Debug)]
pub struct GlobalEnvironment {
    globals: HashMap<String, LoxObject>,
    stack: Vec<HashMap<String, LoxObject>>,
    function_stack: Vec<FunctionEnvironment>,
}
impl GlobalEnvironment {
    /// Create a new Environment with no globals and an empty stack
    pub fn new() -> Self {
        Self::with_globals(HashMap::new())
    }

    /// Create a new Environment with the given globals and an empty
    /// stack
    pub fn with_globals(globals: HashMap<String, LoxObject>) -> Self {
        GlobalEnvironment {
            globals,
            stack: Vec::new(),
            function_stack: Vec::new(),
        }
    }

    /// Create a new, empty function environment
    pub fn push_function_environment(&mut self) {
        self.function_stack.push(FunctionEnvironment::new());
    }

    /// Remove and return the inner-most function environment
    pub fn pop_function_environment(&mut self) -> Option<FunctionEnvironment> {
        self.function_stack.pop()
    }
}
impl Environment for GlobalEnvironment {
    fn insert(&mut self, identifier: String, value: LoxObject) {
        if self.function_stack.is_empty() {
            if self.stack.is_empty() {
                self.globals.insert(identifier, value);
            } else {
                let top_index = self.stack.len() - 1;
                self.stack[top_index].insert(identifier, value);
            }
        } else {
            let top_index = self.function_stack.len() - 1;
            self.function_stack[top_index].insert(identifier, value);
        }
    }

    fn get_mut(&mut self, identifier: &str) -> Option<&mut LoxObject> {
        if self.function_stack.is_empty() {
            for scope in self.stack.iter_mut().rev() {
                if let Some(value) = scope.get_mut(identifier) {
                    return Some(value);
                }
            }
        } else {
            for scope in self.function_stack.iter_mut().rev() {
                if let Some(value) = scope.get_mut(identifier) {
                    return Some(value);
                }
            }
        }
        self.globals.get_mut(identifier)
    }

    fn get(&self, identifier: &str) -> Option<&LoxObject> {
        if self.function_stack.is_empty() {
            for scope in self.stack.iter().rev() {
                if let Some(value) = scope.get(identifier) {
                    return Some(value);
                }
            }
        } else {
            for scope in self.function_stack.iter().rev() {
                if let Some(value) = scope.get(identifier) {
                    return Some(value);
                }
            }
        }
        self.globals.get(identifier)
    }

    fn push_scope(&mut self) {
        if self.function_stack.is_empty() {
            self.stack.push(HashMap::new());
        } else {
            let top_index = self.function_stack.len() - 1;
            self.function_stack[top_index].push_scope();
        }
    }

    fn pop_scope(&mut self) -> Option<HashMap<String, LoxObject>> {
        if self.function_stack.is_empty() {
            self.stack.pop()
        } else {
            let top_index = self.function_stack.len() - 1;
            self.function_stack[top_index].pop_scope()
        }
    }

    fn assign(&mut self, identifier: &str, value: LoxObject) -> Result<(), ()> {
        if self.function_stack.is_empty() {
            for scope in self.stack.iter_mut().rev() {
                if let Some(map_value) = scope.get_mut(identifier) {
                    *map_value = value;
                    return Ok(());
                }
            }
        } else {
            let top_index = self.function_stack.len() - 1;
            if self.function_stack[top_index]
                .assign(identifier, value.clone())
                .is_ok()
            {
                return Ok(());
            }
        }
        if let Some(map_value) = self.globals.get_mut(identifier) {
            *map_value = value;
            Ok(())
        } else {
            Err(())
        }
    }
}
impl Default for GlobalEnvironment {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub struct FunctionEnvironment {
    stack: Vec<HashMap<String, LoxObject>>,
}
impl FunctionEnvironment {
    fn new() -> Self {
        FunctionEnvironment {
            stack: vec![HashMap::new()],
        }
    }
}
impl Environment for FunctionEnvironment {
    fn insert(&mut self, identifier: String, value: LoxObject) {
        let top_index = self.stack.len() - 1;
        self.stack[top_index].insert(identifier, value);
    }
    fn get_mut(&mut self, identifier: &str) -> Option<&mut LoxObject> {
        for scope in self.stack.iter_mut().rev() {
            if let Some(value) = scope.get_mut(identifier) {
                return Some(value);
            }
        }
        None
    }
    fn get(&self, identifier: &str) -> Option<&LoxObject> {
        for scope in self.stack.iter().rev() {
            if let Some(value) = scope.get(identifier) {
                return Some(value);
            }
        }
        None
    }
    fn push_scope(&mut self) {
        self.stack.push(HashMap::new());
    }
    fn pop_scope(&mut self) -> Option<HashMap<String, LoxObject>> {
        self.stack.pop()
    }
    fn assign(&mut self, identifier: &str, value: LoxObject) -> Result<(), ()> {
        for scope in self.stack.iter_mut().rev() {
            if let Some(map_value) = scope.get_mut(identifier) {
                *map_value = value;
                return Ok(());
            }
        }
        Err(())
    }
}
impl Default for FunctionEnvironment {
    fn default() -> Self {
        Self::new()
    }
}
