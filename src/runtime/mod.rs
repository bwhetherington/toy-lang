use crate::{common::TLError, parser::DStatement};

use std::{
    cell::RefCell,
    collections::HashMap,
    fmt,
    rc::{Rc, Weak},
};

mod builtins;
use builtins::Init;

mod engine;
mod env;
mod ignore;
mod obj;

pub use engine::Engine;
pub use ignore::IgnoreScope;
pub use obj::Object;

pub type Scope = HashMap<String, Value>;

pub type BuiltinFn = Rc<dyn Fn(&[Value], &mut Engine) -> Result<Value, TLError>>;

pub fn builtin(f: impl Fn(&[Value], &mut Engine) -> Result<Value, TLError> + 'static) -> Value {
    Value::Builtin(Builtin(Rc::new(f)))
}

#[derive(Debug, Clone)]
pub struct Function {
    pub self_value: Option<Value>,
    pub params: Vec<String>,
    pub last: Option<String>,
    pub body: Vec<DStatement>,
    pub closure: Scope,
}

#[derive(Clone)]
pub struct Builtin(BuiltinFn);

#[derive(Clone)]
pub enum Value {
    Alias(Str),
    Number(f64),
    Boolean(bool),
    List(Ptr<Vec<Value>>),
    Function(Rc<Function>),
    Builtin(Builtin),
    String(Str),
    Object(Ptr<Object>),
    None,
}

impl fmt::Debug for Builtin {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<func>")
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Alias(a) => write!(f, "->{}", a),
            Value::Number(n) => write!(f, "{}", n),
            Value::Boolean(b) if *b => write!(f, "True"),
            Value::Boolean(..) => write!(f, "False"),
            Value::List(xs) => write!(f, "{:?}", xs.borrow()),
            Value::Function(..) => write!(f, "<func>"),
            Value::Builtin(..) => write!(f, "<func>"),
            Value::String(s) => write!(f, "{:?}", s),
            Value::Object(o) => write!(
                f,
                "{{ proto: {:?}, fields: {:?} }}",
                o.borrow().proto.as_ref(),
                o.borrow().fields
            ),
            Value::None => write!(f, "None"),
        }
    }
}

pub fn type_of(value: &Value) -> &'static str {
    match value {
        Value::Alias(..) => "Alias",
        Value::Number(..) => "Number",
        Value::Boolean(..) => "Boolean",
        Value::List(..) => "List",
        Value::Function(..) => "Function",
        Value::Builtin(..) => "Function",
        Value::String(..) => "String",
        Value::Object(..) => "Object",
        Value::None => "None",
    }
}

pub fn type_error(expected: impl Into<String>, found: &Value) -> TLError {
    let found = type_of(found);
    TLError::TypeMismatch(expected.into(), found.to_string())
}

pub fn init_engine() -> Engine {
    let mut engine = Engine::new();
    engine.init().unwrap();
    engine
}

pub type Ptr<T> = Rc<RefCell<T>>;

pub fn ptr<T>(val: T) -> Ptr<T> {
    Rc::new(RefCell::new(val))
}

pub type Str = Rc<str>;
