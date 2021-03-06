use crate::{
    common::{Str, TLError},
    module::{ModuleLoader, NoopLoader},
    parser::{DStatement, Identifier},
};

use nohash_hasher::IntMap as HashMap;
use std::{cell::RefCell, fmt, ops::Deref, rc::Rc};

mod builtins;
pub use builtins::Init;

mod engine;
mod env;
mod ignore;
mod obj;

pub use engine::{Engine, EngineState};
pub use ignore::IgnoreScope;
pub use obj::Object;

pub type Scope = HashMap<usize, Value>;

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
    pub params: Vec<Identifier>,
    pub last: Option<Identifier>,
    pub body: Vec<DStatement>,
    pub closure: Scope,
}

#[derive(Clone)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    List(Ptr<Vec<Value>>),
    Function(Ptr<Function>),
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
            Value::Function(..) => write!(f, "<fn>"),
            Value::Builtin(..) => write!(f, "<fn>"),
            Value::String(s) => write!(f, "{:?}", s),
            Value::Object(o) => {
                let obj = o.borrow();
                let fields = obj.fields.borrow();
                write!(f, "{{")?;
                let mut is_started = false;
                for (key, value) in fields.iter() {
                    if is_started {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {:?}", key, value)?;
                    is_started = true;
                }
                write!(f, "}}")
            }
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

    pub fn insert_recursive_reference(&mut self, name: Identifier, value: &Value) {
        match self {
            Value::Function(f) => {
                let mut f = f.borrow_mut();
                if !f.closure.contains_key(&name.0) {
                    f.closure.insert(name.0, value.clone());
                }
            }
            Value::Object(obj) => {
                let obj = obj.borrow_mut();
                {
                    let mut fields = obj.fields.borrow_mut();
                    for field in fields.values_mut() {
                        field.insert_recursive_reference(name, value);
                    }
                }
                if let Some(mut proto) = obj.proto.clone().map(Value::Object) {
                    proto.insert_recursive_reference(name, value);
                }
            }
            Value::List(items) => {
                for item in items.borrow_mut().iter_mut() {
                    item.insert_recursive_reference(name, value);
                }
            }
            _ => (),
        }
    }
}

pub fn type_error(expected: impl Into<String>, found: &Value) -> TLError {
    let found = found.type_of();
    TLError::TypeMismatch(expected.into(), found.to_string())
}

pub fn init_engine(loader: impl ModuleLoader + 'static) -> Engine {
    let mut engine = Engine::new(loader);

    if let Err(e) = engine.init() {
        println!("{:#?}", engine);
        println!("{}", e);
    }
    engine
}

pub fn noop_engine() -> Engine {
    init_engine(NoopLoader)
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

impl Into<Value> for f64 {
    fn into(self) -> Value {
        Value::Number(self)
    }
}

impl Into<Value> for bool {
    fn into(self) -> Value {
        Value::Boolean(self)
    }
}

impl<'a> Into<Value> for &'a str {
    fn into(self) -> Value {
        Value::String(self.into())
    }
}

impl Into<Value> for String {
    fn into(self) -> Value {
        Value::String(self.into())
    }
}

impl Into<Value> for Str {
    fn into(self) -> Value {
        Value::String(self)
    }
}

impl Into<Value> for () {
    fn into(self) -> Value {
        Value::None
    }
}

impl<T: Into<Value> + Clone, const S: usize> Into<Value> for [T; S] {
    fn into(self) -> Value {
        let mut vec = Vec::with_capacity(S);
        for x in self.iter() {
            vec.push(x.clone().into());
        }
        Value::List(ptr(vec))
    }
}
