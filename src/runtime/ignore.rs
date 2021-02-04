use std::collections::HashSet;

pub struct IgnoreScope {
    ignored: Vec<HashSet<String>>,
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

    pub fn insert(&mut self, s: impl Into<String>) {
        if let Some(scope) = self.ignored.last_mut() {
            scope.insert(s.into());
        }
    }

    pub fn contains(&self, s: impl AsRef<str>) -> bool {
        let s = s.as_ref();
        self.ignored
            .iter()
            .rev()
            .filter(|scope| scope.contains(s))
            .next()
            .is_some()
    }
}
