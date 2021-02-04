use crate::runtime::ptr;
use crate::{
    common::TLError,
    runtime::{type_error, Engine, Value},
};

pub trait Init {
    type Error;
    fn init(&mut self) -> Result<(), Self::Error>;
}

fn unary_fn(
    f: impl Fn(f64) -> f64 + 'static,
) -> impl Fn(&[Value], &mut Engine) -> Result<Value, TLError> {
    move |args, _| match args {
        [Value::Number(n), ..] => Ok(Value::Number(f(*n))),
        [other, ..] => Err(type_error("Number", other)),
        [] => Err(type_error("Number", &Value::None)),
    }
}

impl Init for Engine {
    type Error = TLError;

    fn init(&mut self) -> Result<(), Self::Error> {
        use Value::*;

        self.define_builtin("import", |args, engine| match args {
            [String(s)] => engine.import(s.as_ref()),
            [other, ..] => Err(type_error("String", other)),
            [] => Err(type_error("String", &Value::None)),
        });

        self.define_builtin("print", |args, _| {
            println!("{:?}", args);
            Ok(Value::None)
        });

        self.define_builtin("create_object", |args, _| match args {
            [Object(proto), Object(new), ..] => {
                let obj = crate::runtime::Object::from_proto(proto.clone(), &new.borrow());
                Ok(Object(ptr(obj)))
            }
            _ => todo!(),
        });

        self.define_builtin("sin", unary_fn(f64::sin));
        self.define_builtin("cos", unary_fn(f64::cos));
        self.define_builtin("tan", unary_fn(f64::tan));

        Ok(())
    }
}
