use crate::{
    common::{TLError, TLResult},
    module::ModuleLoader,
    parser::{BinaryOp, DExpression, DField, DStatement, LValue, Program, Spread, UnaryOp},
    runtime::{
        builtin, env::Env, init_engine, ptr, type_error, Builtin, Function, IgnoreScope, Object,
        Ptr, Scope, Value,
    },
};
use std::rc::Rc;

pub struct Engine {
    env: Env,
    ret_val: Value,
    loader: Option<Box<dyn ModuleLoader>>,
}

pub type EvalResult<T> = TLResult<T>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EngineState {
    Run,
    Block,
    Break,
    Return,
}

struct NoneIterator;

impl Iterator for NoneIterator {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        Some(Value::None)
    }
}

impl Engine {
    pub fn new() -> Engine {
        let mut engine = Engine {
            env: Env::new(),
            ret_val: Value::None,
            loader: None,
        };
        engine.env.push();
        engine
    }

    pub fn set_loader(&mut self, loader: Box<dyn ModuleLoader>) {
        self.loader = Some(loader);
    }

    pub fn import(&mut self, path: &str) -> TLResult<Value> {
        let loader = self
            .loader
            .as_ref()
            .map(Clone::clone)
            .ok_or_else(|| TLError::ModuleNotFound(path.to_string()))?;

        let mut module_engine = init_engine();
        module_engine.set_loader(loader.clone());

        let module = loader
            .clone()
            .load_module(path, &mut module_engine)?
            .ok_or_else(|| TLError::ModuleNotFound(path.to_string()))?;
        Ok(module)
    }

    pub fn define(&mut self, name: impl Into<String>, val: Value) {
        self.env.insert(name, val);
    }

    pub fn define_builtin(
        &mut self,
        name: impl Into<String>,
        f: impl Fn(&[Value], &mut Engine) -> EvalResult<Value> + 'static,
    ) {
        let f = builtin(f);
        self.define(name, f);
    }

    fn lookup(&self, key: impl AsRef<str>) -> EvalResult<&Value> {
        self.env
            .get(key.as_ref())
            .ok_or_else(|| TLError::Undefined(key.as_ref().to_string()))
    }

    fn lookup_mut(&mut self, key: impl AsRef<str>) -> EvalResult<&mut Value> {
        self.env
            .get_mut(key.as_ref())
            .ok_or_else(|| TLError::Undefined(key.as_ref().to_string()))
    }

    fn extract_statement_to(&self, stmt: &DStatement, map: &mut Scope, ignore: &mut IgnoreScope) {
        match stmt {
            DStatement::Definition(is_pub, name, value) => {
                ignore.insert(name);
                self.extract_expr_to(value, map, ignore);
            }
            DStatement::Return(val) => self.extract_expr_to(val, map, ignore),
            DStatement::Assignment(_, val) => self.extract_expr_to(val, map, ignore),
            DStatement::Loop(body) => {
                ignore.push();
                for stmt in body {
                    self.extract_statement_to(stmt, map, ignore);
                }
                ignore.pop();
            }
            DStatement::Conditional(cond, then, otherwise) => {
                self.extract_expr_to(cond, map, ignore);
                ignore.push();
                for stmt in then {
                    self.extract_statement_to(stmt, map, ignore);
                }
                ignore.pop();
                ignore.push();
                for stmt in otherwise {
                    self.extract_statement_to(stmt, map, ignore);
                }
                ignore.pop();
            }
            DStatement::Expression(expr) => self.extract_expr_to(expr, map, ignore),
            DStatement::Break => (),
        }
    }

    fn extract_expr_to(&self, expr: &DExpression, map: &mut Scope, ignore: &mut IgnoreScope) {
        match expr {
            DExpression::Identifier(i) if !ignore.contains(i) => {
                if let Ok(value) = self.lookup(i) {
                    map.insert(i.clone(), value.clone());
                }
            }
            DExpression::List(list) => {
                for Spread { value, .. } in list {
                    self.extract_expr_to(value, map, ignore);
                }
            }
            DExpression::Call(f, args) => {
                self.extract_expr_to(f.as_ref(), map, ignore);
                for Spread { value, .. } in args {
                    self.extract_expr_to(value, map, ignore);
                }
            }
            DExpression::Unary(_, val) => self.extract_expr_to(val.as_ref(), map, ignore),
            DExpression::Binary(_, lhs, rhs) => {
                self.extract_expr_to(lhs.as_ref(), map, ignore);
                self.extract_expr_to(rhs.as_ref(), map, ignore);
            }
            DExpression::Member(obj, _) => self.extract_expr_to(obj.as_ref(), map, ignore),
            DExpression::Object(decls) => {
                for DField { value, .. } in decls {
                    self.extract_expr_to(value, map, ignore);
                }
            }
            DExpression::Lambda(params, last, body) => {
                // Ignore all parameters
                ignore.push();
                let iter = params.iter().chain(last.iter());
                for param in iter {
                    ignore.insert(param);
                }
                for stmt in body {
                    self.extract_statement_to(stmt, map, ignore);
                }
                ignore.pop();
            }
            _ => (),
        }
    }

    fn create_closure(
        &self,
        params: &Vec<String>,
        last: &Option<String>,
        body: &Vec<DStatement>,
    ) -> Function {
        let mut ignore = IgnoreScope::new();
        let mut closure = Scope::new();

        ignore.push();
        ignore.insert("self");
        ignore.insert("super");

        for param in params.iter().chain(last.iter()) {
            ignore.insert(param);
        }

        for stmt in body {
            self.extract_statement_to(stmt, &mut closure, &mut ignore);
        }

        ignore.pop();

        Function {
            self_value: None,
            params: params.clone(),
            last: last.clone(),
            body: body.clone(),
            closure,
        }
    }

    fn eval_unary(&mut self, op: UnaryOp, val: &DExpression) -> EvalResult<Value> {
        use UnaryOp::*;
        use Value::*;

        let val = self.eval_expr(val)?;
        match (op, val) {
            (Negative, Number(x)) => Ok(Number(-x)),
            (Not, Boolean(b)) => Ok(Boolean(!b)),
            _ => todo!(),
        }
    }

    fn eval_binary(
        &mut self,
        op: BinaryOp,
        lhs: &DExpression,
        rhs: &DExpression,
    ) -> EvalResult<Value> {
        use BinaryOp::*;
        use Value::*;

        let lhs = self.eval_expr(lhs)?;
        let rhs = self.eval_expr(rhs)?;
        match (op, lhs, rhs) {
            (Add, Number(x), Number(y)) => Ok(Number(x + y)),
            (Subtract, Number(x), Number(y)) => Ok(Number(x - y)),
            (Multiply, Number(x), Number(y)) => Ok(Number(x * y)),
            (Divide, Number(x), Number(y)) => Ok(Number(x / y)),
            (Mod, Number(x), Number(y)) => Ok(Number(x % y)),
            (Power, Number(x), Number(y)) => Ok(Number(x.powf(y))),
            (Equals, Number(x), Number(y)) => Ok(Boolean(x == y)),
            (NotEquals, Number(x), Number(y)) => Ok(Boolean(x != y)),
            (GT, Number(x), Number(y)) => Ok(Boolean(x > y)),
            (GTE, Number(x), Number(y)) => Ok(Boolean(x >= y)),
            (LT, Number(x), Number(y)) => Ok(Boolean(x < y)),
            (LTE, Number(x), Number(y)) => Ok(Boolean(x <= y)),
            (Index, List(l), Number(i)) => l
                .borrow()
                .get(i as usize)
                .cloned()
                .ok_or_else(|| TLError::OutOfBounds(i as usize)),
            other => {
                dbg!(other);
                todo!()
            }
        }
    }

    fn iterate_spread_list(
        &mut self,
        list: &[Spread<DExpression>],
        mut f: impl FnMut(&Value) -> EvalResult<()>,
    ) -> EvalResult<()> {
        for Spread { is_spread, value } in list {
            let value = self.eval_expr(value)?;
            match (is_spread, value) {
                (true, Value::List(list)) => {
                    for arg in list.borrow().iter() {
                        f(arg)?;
                    }
                }
                (true, other) => return Err(type_error("List", &other)),
                (false, other) => f(&other)?,
            }
        }
        Ok(())
    }

    fn eval_spread_list(&mut self, list: &[Spread<DExpression>]) -> EvalResult<Vec<Value>> {
        let mut out = Vec::new();

        self.iterate_spread_list(list, |val| {
            out.push(val.clone());
            Ok(())
        })?;

        Ok(out)
    }

    pub fn eval_expr(&mut self, expr: &DExpression) -> EvalResult<Value> {
        // println!("eval {:?}", expr);
        match expr {
            DExpression::None => Ok(Value::None),
            DExpression::Number(n) => Ok(Value::Number(*n)),
            DExpression::Boolean(b) => Ok(Value::Boolean(*b)),
            DExpression::Identifier(key) => self.lookup(key).map(Clone::clone),
            DExpression::List(exprs) => {
                let vals: Vec<_> = self.eval_spread_list(exprs)?;
                Ok(Value::List(ptr(vals)))
            }
            DExpression::Unary(op, val) => self.eval_unary(*op, val),
            DExpression::Binary(op, lhs, rhs) => self.eval_binary(*op, lhs, rhs),
            DExpression::Lambda(params, last, body) => Ok(Value::Function(Rc::new(
                self.create_closure(params, last, body),
            ))),
            DExpression::String(s) => Ok(Value::String(s.to_string().into())),
            DExpression::Call(f, args) => {
                let f = self.eval_expr(f)?;
                match f {
                    Value::Function(f) => {
                        let Function {
                            self_value,
                            params,
                            last,
                            body,
                            closure,
                        } = f.as_ref();
                        let args = self.eval_spread_list(args)?;
                        let args_iter = args.iter().map(Clone::clone).chain(NoneIterator);

                        self.env.push();

                        // Insert self
                        if let Some(self_value) = self_value {
                            self.env.insert("self", self_value.clone());

                            if let Value::Object(obj) = self_value {
                                if let Some(super_value) = obj.borrow().super_proto() {
                                    self.env.insert("super", Value::Object(super_value));
                                }
                            }
                        }

                        // Insert arguments at parameters
                        let param_pairs = params.iter().zip(args_iter);
                        for (param, arg) in param_pairs {
                            self.env.insert(param, arg.clone());
                        }

                        // Insert any remaining parameters into a list
                        if let Some(last) = last {
                            // Check if there are any remaining arguments
                            let rest = if args.len() >= params.len() {
                                let rest = &args[params.len()..];
                                let rest: Vec<_> = rest.iter().map(Clone::clone).collect();
                                rest
                            } else {
                                vec![]
                            };
                            let rest = Value::List(ptr(rest));
                            self.env.insert(last, rest);
                        }

                        // Insert variables from closure
                        for (key, value) in closure {
                            self.env.insert(key, value.clone());
                        }

                        let end = self.eval_block(EngineState::Run, &body)?;
                        let val = if let EngineState::Return = end {
                            self.ret_val.clone()
                        } else {
                            Value::None
                        };

                        self.env.pop();

                        Ok(val)
                    }
                    Value::Builtin(Builtin(f)) => {
                        self.env.push();
                        let args = self.eval_spread_list(args)?;
                        let val = f(&args, self)?;
                        self.env.pop();
                        self.ret_val = val.clone();

                        Ok(val)
                    }
                    other => {
                        println!("call: {:?}", other);
                        todo!()
                    }
                }
            }
            DExpression::Object(fields) => {
                let mut obj = Object::new();

                for DField { name, value } in fields {
                    let value = self.eval_expr(value)?;
                    obj.insert(name, value);
                }

                Ok(Value::Object(ptr(obj)))
            }
            DExpression::Member(obj, field) => {
                let parent = self.eval_expr(obj)?;
                match &parent {
                    Value::Object(obj) => {
                        let obj = obj.borrow();
                        let val = obj.get(field).unwrap_or_else(|| Value::None);
                        match val {
                            Value::Function(f) => {
                                let mut new_f = f.as_ref().clone();
                                new_f.self_value = Some(parent.clone());
                                Ok(Value::Function(Rc::new(new_f)))
                            }
                            other => Ok(other),
                        }
                    }
                    other => Err(type_error("Object", &other)),
                }
            }
        }
    }

    fn mutate_location(
        &mut self,
        loc: &LValue,
        f: impl FnOnce(&mut Value) -> EvalResult<()>,
    ) -> EvalResult<()> {
        match loc {
            LValue::Identifier(i) => {
                let loc = self.lookup_mut(i)?;
                f(loc)
            }
            LValue::Index(x, y) => {
                let x = self.eval_expr(x)?;
                let y = self.eval_expr(y)?;
                match (x, y) {
                    (Value::List(list), Value::Number(i)) => {
                        // Logic
                        let index = i as usize;
                        let mut list = list.borrow_mut();
                        let loc = list
                            .get_mut(index)
                            .ok_or_else(|| TLError::OutOfBounds(index))?;
                        f(loc)
                    }
                    (Value::List(..), y) => Err(type_error("Number", &y)),
                    (x, _) => Err(type_error("List", &x)),
                }
            }
            LValue::Member(obj, field) => {
                let obj = self.eval_expr(obj)?;
                match obj {
                    Value::Object(obj) => {
                        let mut obj = obj.borrow_mut();
                        f(obj.get_mut_or_none(field))
                    }
                    other => Err(type_error("Object", &other)),
                }
            }
        }
    }

    fn eval_block(&mut self, state: EngineState, block: &[DStatement]) -> EvalResult<EngineState> {
        let cur_state = EngineState::Block;
        for stmt in block {
            let next_state = self.eval_stmt(cur_state, stmt, None)?;
            match next_state {
                EngineState::Return => {
                    return Ok(EngineState::Return);
                }
                EngineState::Break => {
                    return Ok(EngineState::Break);
                }
                _ => (),
            }
        }

        Ok(state)
    }

    pub fn eval_module(&mut self, module: &[DStatement]) -> EvalResult<Value> {
        let mut export = Object::new();

        self.env.push();
        for stmt in module {
            self.eval_stmt(EngineState::Run, stmt, Some(&mut export))?;
        }
        self.env.pop();

        Ok(Value::Object(ptr(export)))
    }

    pub fn eval_stmt(
        &mut self,
        state: EngineState,
        stmt: &DStatement,
        export: Option<&mut Object>,
    ) -> EvalResult<EngineState> {
        // println!("exe {:?}", stmt);
        match stmt {
            DStatement::Expression(expr) => {
                self.eval_expr(expr)?;
                Ok(state)
            }
            DStatement::Conditional(cond, then, otherwise) => {
                let cond = self.eval_expr(cond)?;
                match cond {
                    Value::Boolean(b) => {
                        if b {
                            self.env.push();
                            let state = self.eval_block(state, then)?;
                            self.env.pop();
                            Ok(state)
                        } else {
                            self.env.push();
                            let state = self.eval_block(state, otherwise)?;
                            self.env.pop();
                            Ok(state)
                        }
                    }
                    other => Err(type_error("Boolean", &other)),
                }
            }
            DStatement::Assignment(loc, value) => {
                let value = self.eval_expr(value)?;
                self.mutate_location(loc, move |loc| {
                    *loc = value;
                    Ok(())
                })?;
                Ok(state)
            }
            DStatement::Definition(is_pub, name, value) => {
                let value = self.eval_expr(value)?;
                if *is_pub {
                    if let Some(export) = export {
                        export.insert(name, value.clone());
                    }
                }
                self.env.insert(name, value);
                Ok(state)
            }
            DStatement::Break => Ok(match state {
                EngineState::Block => EngineState::Break,
                other => other,
            }),
            DStatement::Return(val) => {
                let val = self.eval_expr(val)?;
                self.ret_val = val;
                Ok(EngineState::Return)
            }
            DStatement::Loop(body) => {
                loop {
                    self.env.push();
                    let next_state = self.eval_block(state, body)?;
                    self.env.pop();
                    match next_state {
                        EngineState::Break => break,
                        EngineState::Return => return Ok(EngineState::Return),
                        _ => (),
                    }
                }
                Ok(state)
            }
        }
    }

    pub fn eval_program(&mut self, body: &Program) -> EvalResult<()> {
        // Run program
        for stmt in body {
            self.eval_stmt(EngineState::Run, stmt, None)?;
        }
        Ok(())
    }
}
