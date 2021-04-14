use crate::common::TLResult;
use crate::parser::Identifier;
use crate::runtime::{ptr, Ptr, Scope, Value};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Object {
    pub proto: Option<Ptr<Object>>,
    pub fields: Ptr<Scope>,
}

impl Object {
    pub fn new() -> Object {
        Object {
            proto: None,
            fields: ptr(HashMap::new()),
        }
    }

    pub fn len(&self) -> usize {
        self.fields.borrow().len()
    }

    pub fn with_proto(&self, proto: Ptr<Object>) -> Object {
        let fields = self.fields.clone();
        Object {
            proto: Some(proto),
            fields,
        }
    }

    pub fn from_proto(proto: Ptr<Object>, fields: &Object) -> Object {
        let mut obj = fields.clone();
        obj.proto = Some(proto);
        obj
    }

    pub fn get(&self, field: Identifier) -> Option<Value> {
        self.fields
            .borrow()
            .get(&field)
            .map(|val| val.clone())
            .or_else(|| {
                let proto = self.proto.as_ref()?;
                let res = proto.borrow().get(field)?;
                Some(res)
            })
    }

    pub fn insert(&mut self, field: Identifier, val: Value) {
        self.fields.borrow_mut().insert(field, val);
    }

    pub fn mutate_field(
        &mut self,
        field: Identifier,
        f: impl FnOnce(&mut Value) -> TLResult<()>,
    ) -> TLResult<()> {
        let mut fields = self.fields.borrow_mut();
        let entry = fields.entry(field).or_insert_with(|| Value::None);
        f(entry)
    }

    pub fn super_proto(&self) -> Option<Ptr<Object>> {
        let proto = self.proto.as_ref()?.borrow();
        let super_proto = proto.proto.as_ref()?.clone();
        let super_obj = self.with_proto(super_proto);
        Some(ptr(super_obj))
    }
}
