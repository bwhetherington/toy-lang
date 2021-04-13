use crate::{
    common::TLError,
    runtime::{builtin, ptr, type_error, Engine, Object, Value},
};
use std::rc::Rc;

pub trait Init {
    type Error;
    fn init(&mut self) -> Result<(), Self::Error>;
}

fn unary_fn(
    f: impl Fn(f64) -> f64 + 'static,
) -> impl Fn(&[Value], &mut Engine, Option<&Value>) -> Result<Value, TLError> {
    move |args, _, _| match args {
        [Value::Number(n), ..] => Ok(Value::Number(f(*n))),
        [other, ..] => Err(type_error("Number", other)),
        [] => Err(type_error("Number", &Value::None)),
    }
}

fn binary_fn(
    f: impl Fn(f64, f64) -> f64 + 'static,
) -> impl Fn(&[Value], &mut Engine, Option<&Value>) -> Result<Value, TLError> {
    move |args, _, _| match args {
        [Value::Number(x), Value::Number(y), ..] => Ok(Value::Number(f(*x, *y))),
        [Value::Number(_), y, ..] => Err(type_error("Number", y)),
        [x, ..] => Err(type_error("Number", x)),
        [] => Err(type_error("Number", &Value::None)),
    }
}

fn create_proto(init: impl Fn(&mut Object) -> ()) -> Value {
    let mut obj = Object::new();
    init(&mut obj);
    Value::Object(ptr(obj))
}

fn create_string_proto() -> Value {
    create_proto(|obj| {
        obj.insert(
            "len",
            builtin(|_, _, self_value| match self_value {
                Some(Value::String(s)) => Ok(Value::Number(s.len() as f64)),
                Some(other) => Err(type_error("String", other)),
                None => Err(type_error("String", &Value::None)),
            }),
        );
        obj.insert(
            "lower",
            builtin(|_, _, self_value| match self_value {
                Some(Value::String(s)) => Ok(Value::String(s.to_lowercase().into())),
                Some(other) => Err(type_error("String", other)),
                None => Err(type_error("String", &Value::None)),
            }),
        );
        obj.insert(
            "upper",
            builtin(|_, _, self_value| match self_value {
                Some(Value::String(s)) => Ok(Value::String(s.to_uppercase().into())),
                Some(other) => Err(type_error("String", other)),
                None => Err(type_error("String", &Value::None)),
            }),
        );
        obj.insert(
            "plus",
            builtin(|args, _, self_value| match (self_value, args) {
                (Some(Value::String(s)), [other, ..]) => {
                    let out_str = match other {
                        Value::String(other_s) => format!("{}{}", s, other_s),
                        other => format!("{}{:?}", s, other),
                    };
                    Ok(Value::String(out_str.into()))
                }
                (Some(other), _) => Err(type_error("String", other)),
                (None, _) => Err(type_error("String", &Value::None)),
            }),
        )
    })
}

fn create_list_proto() -> Value {
    create_proto(|obj| {
        obj.insert(
            "len",
            builtin(|_, _, self_value| match self_value {
                Some(Value::List(l)) => Ok(Value::Number(l.borrow().len() as f64)),
                Some(other) => Err(type_error("List", other)),
                None => Err(type_error("List", &Value::None)),
            }),
        );

        obj.insert(
            "push",
            builtin(|args, _, self_value| match self_value {
                Some(Value::List(l)) => {
                    let mut l = l.borrow_mut();
                    for arg in args {
                        l.push(arg.clone());
                    }
                    Ok(Value::None)
                }
                Some(other) => Err(type_error("List", other)),
                None => Err(type_error("List", &Value::None)),
            }),
        );

        obj.insert(
            "pop",
            builtin(|_, _, self_value| match self_value {
                Some(Value::List(l)) => {
                    let mut l = l.borrow_mut();
                    let val = l.pop();
                    match val {
                        Some(val) => Ok(val.clone()),
                        None => Ok(Value::None),
                    }
                }
                Some(other) => Err(type_error("List", other)),
                None => Err(type_error("List", &Value::None)),
            }),
        );
    })
}

fn create_number_proto() -> Value {
    create_proto(|_| {})
}

fn create_boolean_proto() -> Value {
    create_proto(|_| {})
}

fn create_function_proto() -> Value {
    create_proto(|_| {})
}

fn create_object_proto() -> Value {
    create_proto(|_| {})
}

fn create_none_proto() -> Value {
    create_proto(|_| {})
}

impl Init for Engine {
    type Error = TLError;

    fn init(&mut self) -> Result<(), Self::Error> {
        use Value::*;

        self.define_builtin("import", |args, engine, _| match args {
            [String(s)] => engine.load_module(s.as_ref()),
            [other, ..] => Err(type_error("String", other)),
            [] => Err(type_error("String", &Value::None)),
        });

        fn builtin_print(args: &[Value]) {
            let mut is_first = true;
            for arg in args {
                let s = match arg {
                    Value::String(s) => s.to_string(),
                    other => format!("{:?}", other),
                };
                if !is_first {
                    print!(" ");
                }
                print!("{}", s);
                is_first = false;
            }
        }

        self.define_builtin("__print__", |args, _, _| {
            builtin_print(args);
            Ok(Value::None)
        });

        self.define_builtin("create_object", |args, _, _| match args {
            [Object(proto), Object(new), ..] => {
                let obj = crate::runtime::Object::from_proto(proto.clone(), &new.borrow());
                Ok(Object(ptr(obj)))
            }
            [Object(..), b, ..] => Err(type_error("Object", &b)),
            [a, ..] => Err(type_error("Object", &a)),
            [] => Err(type_error("Object", &Value::None)),
        });

        self.define_builtin("copy_object", |args, _, _| match args {
            [Object(obj)] => Ok(Object(ptr(obj.borrow().clone()))),
            _ => todo!(),
        });

        self.define_builtin("abs", unary_fn(f64::abs));
        self.define_builtin("sqrt", unary_fn(f64::sqrt));

        self.define_builtin("sin", unary_fn(f64::sin));
        self.define_builtin("cos", unary_fn(f64::cos));
        self.define_builtin("tan", unary_fn(f64::tan));
        self.define_builtin("asin", unary_fn(f64::asin));
        self.define_builtin("acos", unary_fn(f64::acos));
        self.define_builtin("atan", unary_fn(f64::atan));
        self.define_builtin("atan2", binary_fn(f64::atan2));

        self.define_builtin("type_of", |args, _, _| match args {
            [x, ..] => Ok(Value::String(x.type_of().into())),
            [] => Ok(Value::String(Value::None.type_of().into())),
        });

        self.define_builtin("instance_of", |args, engine, _| match args {
            [x, y, ..] => Ok(Value::Boolean(engine.instance_of(x, y))),
            _ => Ok(Value::Boolean(false)),
        });

        self.define("List", create_list_proto());
        self.define("String", create_string_proto());
        self.define("Number", create_number_proto());
        self.define("Boolean", create_boolean_proto());
        self.define("Function", create_function_proto());
        self.define("Object", create_object_proto());
        self.define("NoneProto", create_none_proto());

        // Include core libraries
        self.push_scope();

        // Class support
        let class_src = include_str!("core/class.rsc");
        let class_decls = self.run_src_map(class_src)?;
        for (name, value) in class_decls {
            self.define_global(name, value);
        }

        self.pop_scope();
        self.push_scope();

        // Iterator support
        let iter_src = include_str!("core/iter.rsc");
        let iter_decls = self.run_src_map(iter_src)?;
        for (name, value) in iter_decls {
            self.define_global(name, value);
        }

        self.pop_scope();
        self.push_scope();

        // Prototype methods
        let protos_src = include_str!("core/protos.rsc");
        self.run_src_map(protos_src)?;

        // IO functions
        let io_src = include_str!("core/io.rsc");
        let io_decls = self.run_src_map(io_src)?;
        for (name, value) in io_decls {
            self.define_global(name, value);
        }

        self.pop_scope();

        Ok(())
    }
}
