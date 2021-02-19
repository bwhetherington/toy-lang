use crate::{common::TLError, module::ModuleLoader, parser::DStatement};

use std::{cell::RefCell, collections::HashMap, fmt, ops::Deref, rc::Rc};

mod builtins;
pub use builtins::Init;

mod engine;
mod env;
mod ignore;
mod obj;

pub use engine::{Engine, EngineState};
pub use ignore::IgnoreScope;
pub use obj::Object;

pub type Scope = HashMap<String, Value>;

#[derive(Clone)]
pub struct BuiltinFn {
    pub self_value: Option<Value>,
    pub func: Rc<dyn Fn(&[Value], &mut Engine, Option<&Value>) -> Result<Value, TLError>>,
}

pub fn builtin(
    f: impl Fn(&[Value], &mut Engine, Option<&Value>) -> Result<Value, TLError> + 'static,
) -> Value {
    Value::Builtin(Rc::new(BuiltinFn {
        self_value: None,
        func: Rc::new(f),
    }))
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
pub enum Value {
    Number(f64),
    Boolean(bool),
    List(Ptr<Vec<Value>>),
    Function(Rc<Function>),
    Builtin(Rc<BuiltinFn>),
    String(Str),
    Object(Ptr<Object>),
    None,
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::Boolean(b) if *b => write!(f, "True"),
            Value::Boolean(..) => write!(f, "False"),
            Value::List(xs) => write!(f, "{:?}", xs.borrow()),
            Value::Function(..) => write!(f, "<func>"),
            Value::Builtin(..) => write!(f, "<func>"),
            Value::String(s) => write!(f, "{:?}", s),
            Value::Object(o) => write!(f, "{:?}", o.borrow().fields),
            Value::None => write!(f, "None"),
        }
    }
}

impl Value {
    pub fn type_of(&self) -> &'static str {
        match self {
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
}

pub fn type_error(expected: impl Into<String>, found: &Value) -> TLError {
    let found = found.type_of();
    TLError::TypeMismatch(expected.into(), found.to_string())
}

pub fn init_engine(loader: impl ModuleLoader + 'static) -> Engine {
    let mut engine = Engine::new(loader);
    engine.init().unwrap();
    engine
}

pub type Ptr<T> = Rc<RefCell<T>>;

pub fn ptr<T>(val: T) -> Ptr<T> {
    Rc::new(RefCell::new(val))
}

pub fn ref_eq<T>(a: impl Deref<Target = T>, b: impl Deref<Target = T>) -> bool {
    let a = a.deref() as *const T;
    let b = b.deref() as *const T;
    a == b
}

pub type Str = Rc<str>;
