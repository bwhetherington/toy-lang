use crate::parser::Identifier;
use nohash_hasher::IntSet as HashSet;

pub struct IgnoreScope {
    ignored: Vec<HashSet<usize>>,
}

impl IgnoreScope {
    pub fn new() -> IgnoreScope {
        IgnoreScope {
            ignored: Vec::new(),
        }
    }

    pub fn push(&mut self) {
        self.ignored.push(HashSet::default());
    }

    pub fn pop(&mut self) {
        self.ignored.pop();
    }

    pub fn insert(&mut self, s: Identifier) {
        if let Some(scope) = self.ignored.last_mut() {
            scope.insert(s.0);
        }
    }

    pub fn contains(&self, s: Identifier) -> bool {
        self.ignored
            .iter()
            .rev()
            .filter(|scope| scope.contains(&s.0))
            .next()
            .is_some()
    }
}
