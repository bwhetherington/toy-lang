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

fn create_string_proto() -> Value {
    let mut obj = Object::new();

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

    Value::Object(ptr(obj))
}

fn create_list_proto() -> Value {
    let mut obj = Object::new();

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

    Value::Object(ptr(obj))
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

        self.define_builtin("print", |args, _, _| {
            builtin_print(args);
            Ok(Value::None)
        });

        self.define_builtin("println", |args, _, _| {
            builtin_print(args);
            println!();
            Ok(Value::None)
        });

        self.define_builtin("write_number", |args, _, _| match args {
            [Value::Number(x)] => {
                print!("{}", x);
                Ok(Value::None)
            }
            _ => todo!(),
        });

        self.define_builtin("write_string", |args, _, _| match args {
            [Value::String(s)] => {
                print!("{}", s);
                Ok(Value::None)
            }
            _ => todo!(),
        });

        self.define_builtin("write_newline", |_, _, _| {
            println!("");
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
        self.define_builtin("sin", unary_fn(f64::sin));
        self.define_builtin("cos", unary_fn(f64::cos));
        self.define_builtin("tan", unary_fn(f64::tan));

        self.define("List", create_list_proto());
        self.define("String", create_string_proto());

        // Include core libraries
        self.push_scope();

        // Class support
        let class_src = include_str!("core/class.rsc");
        let class = self.run_src(class_src)?;
        self.define_global("class", class);

        self.pop_scope();
        self.push_scope();

        // Iterator support
        let iterator_src = include_str!("core/iter.rsc");
        let iter = self.run_src(iterator_src)?;
        self.define_global("iter", iter);

        self.pop_scope();

        Ok(())
    }
}
