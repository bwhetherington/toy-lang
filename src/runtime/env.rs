use crate::{
    parser::Identifier,
    runtime::{Scope, Value},
};
use nohash_hasher::IntMap as HashMap;

#[derive(Debug)]
pub struct Env {
    scopes: Vec<Scope>,
}

impl Env {
    pub fn new() -> Env {
        Env { scopes: Vec::new() }
    }

    pub fn push(&mut self) {
        self.scopes.push(HashMap::default());
    }

    pub fn pop(&mut self) {
        self.scopes.pop();
    }

    pub fn insert_global(&mut self, key: Identifier, val: Value) -> Option<()> {
        let scope = self.scopes.first_mut()?;
        scope.insert(key.0, val);
        Some(())
    }

    pub fn insert(&mut self, key: Identifier, val: Value) -> Option<()> {
        let scope = self.scopes.last_mut()?;
        scope.insert(key.0, val);
        Some(())
    }

    pub fn get(&self, key: Identifier) -> Option<&Value> {
        self.scopes
            .iter()
            .rev()
            .flat_map(|scope| scope.get(&key.0))
            .next()
    }

    pub fn get_mut(&mut self, key: Identifier) -> Option<&mut Value> {
        self.scopes
            .iter_mut()
            .rev()
            .flat_map(|scope| scope.get_mut(&key.0))
            .next()
    }
}
