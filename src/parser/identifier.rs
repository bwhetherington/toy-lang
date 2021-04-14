use std::collections::HashMap;
use std::fmt;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Identifier(usize);

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug)]
pub struct IdentifierEncoder {
    cur_id: usize,
    map: HashMap<String, Identifier>,
    reverse: HashMap<Identifier, String>,
}

impl IdentifierEncoder {
    pub fn new() -> Self {
        Self {
            cur_id: 0,
            map: HashMap::new(),
            reverse: HashMap::new(),
        }
    }

    pub fn encode(&mut self, key: impl Into<String>) -> Identifier {
        let key = key.into();
        if let Some(ident) = self.map.get(&key) {
            *ident
        } else {
            self.cur_id += 1;
            let ident = Identifier(self.cur_id);
            self.map.insert(key.clone(), ident);
            self.reverse.insert(ident, key);
            ident
        }
    }

    pub fn decode(&self, key: Identifier) -> Option<&String> {
        self.reverse.get(&key)
    }
}
