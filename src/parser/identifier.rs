use std::collections::HashMap;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Identifier(usize);

pub struct IdentifierEncoder {
    cur_id: usize,
    map: HashMap<String, Identifier>,
}

impl IdentifierEncoder {
    pub fn new() -> Self {
        Self {
            cur_id: 0,
            map: HashMap::new(),
        }
    }

    pub fn encode(&mut self, key: impl Into<String>) -> Identifier {
        let key = key.into();
        if let Some(ident) = self.map.get(&key) {
            *ident
        } else {
            self.cur_id += 1;
            let ident = Identifier(self.cur_id);
            self.map.insert(key, ident);
            ident
        }
    }
}
