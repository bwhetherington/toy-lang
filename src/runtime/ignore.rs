use crate::parser::Identifier;
use std::collections::HashSet;

pub struct IgnoreScope {
    ignored: Vec<HashSet<Identifier>>,
}

impl IgnoreScope {
    pub fn new() -> IgnoreScope {
        IgnoreScope {
            ignored: Vec::new(),
        }
    }

    pub fn push(&mut self) {
        self.ignored.push(HashSet::new());
    }

    pub fn pop(&mut self) {
        self.ignored.pop();
    }

    pub fn insert(&mut self, s: Identifier) {
        if let Some(scope) = self.ignored.last_mut() {
            scope.insert(s);
        }
    }

    pub fn contains(&self, s: Identifier) -> bool {
        self.ignored
            .iter()
            .rev()
            .filter(|scope| scope.contains(&s))
            .next()
            .is_some()
    }
}
