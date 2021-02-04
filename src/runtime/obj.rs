use crate::runtime::{Ptr, Scope, Value};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Object {
    pub proto: Option<Ptr<Object>>,
    pub fields: Scope,
}

impl Object {
    pub fn new() -> Object {
        Object {
            proto: None,
            fields: HashMap::new(),
        }
    }

    pub fn from_proto(proto: Ptr<Object>, fields: &Object) -> Object {
        let mut obj = fields.clone();
        obj.proto = Some(proto);
        obj
    }

    pub fn get(&self, field: impl AsRef<str>) -> Option<Value> {
        self.fields
            .get(field.as_ref())
            .map(|val| val.clone())
            .or_else(|| {
                let proto = self.proto.as_ref()?;
                let res = proto.borrow().get(field)?;
                Some(res)
            })
    }

    pub fn insert(&mut self, field: impl Into<String>, val: Value) {
        self.fields.insert(field.into(), val);
    }

    pub fn get_mut_or_none(&mut self, field: impl Into<String>) -> &mut Value {
        self.fields
            .entry(field.into())
            .or_insert_with(|| Value::None)
    }

    pub fn super_proto(&self) -> Option<Ptr<Object>> {
        let proto = self.proto.as_ref()?.borrow();
        let super_proto = proto.proto.as_ref()?;
        Some(super_proto.clone())
    }
}
