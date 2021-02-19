use crate::runtime::{Scope, Value};
use std::collections::HashMap;

#[derive(Debug)]
pub struct Env {
    scopes: Vec<Scope>,
}

impl Env {
    pub fn new() -> Env {
        Env { scopes: Vec::new() }
    }

    pub fn push(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn pop(&mut self) {
        self.scopes.pop();
    }

    pub fn insert(&mut self, key: impl Into<String>, val: Value) -> Option<()> {
        let scope = self.scopes.last_mut()?;
        scope.insert(key.into(), val);
        Some(())
    }

    pub fn get(&self, key: impl AsRef<str>) -> Option<&Value> {
        self.scopes
            .iter()
            .rev()
            .flat_map(|scope| scope.get(key.as_ref()))
            .next()
    }

    pub fn get_mut(&mut self, key: impl AsRef<str>) -> Option<&mut Value> {
        self.scopes
            .iter_mut()
            .rev()
            .flat_map(|scope| scope.get_mut(key.as_ref()))
            .next()
    }
}
