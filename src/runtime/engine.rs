use crate::runtime::Ptr;
use crate::{
    common::{Str, TLError, TLResult},
    module::ModuleLoader,
    parser::{
        desugar_statements, module, BinaryOp, DExpression, DField, DStatement, LValue, Spread,
        UnaryOp,
    },
    runtime::{
        builtin, env::Env, ptr, ref_eq, type_error, BuiltinFn, Function, IgnoreScope, Init, Object,
        Scope, Value,
    },
};
use std::{collections::HashMap, mem, rc::Rc};

pub struct Engine {
    env: Env,
    ret_val: Value,
    loader: Box<dyn ModuleLoader>,
    module_stack: Vec<Str>,
    module_cache: HashMap<Str, Value>,
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
    pub fn from_dyn_loader(loader: Box<dyn ModuleLoader>) -> Engine {
        let mut engine = Engine {
            env: Env::new(),
            ret_val: Value::None,
            loader,
            module_stack: Vec::new(),
            module_cache: HashMap::new(),
        };
        engine.env.push();
        engine
    }

    pub fn new(loader: impl ModuleLoader + 'static) -> Engine {
        Engine::from_dyn_loader(Box::new(loader))
    }

    pub fn entry_point(&mut self, path: &str) {
        self.loader.entry_point(path);
        self.module_stack.push(path.into());
    }

    pub fn eval_module_with(
        &mut self,
        module: &[DStatement],
        mut define: impl FnMut(&str, Value) -> (),
    ) -> TLResult<()> {
        self.env.push();
        for stmt in module {
            self.eval_stmt(EngineState::Run, stmt, &mut define)?;
        }
        self.env.pop();
        Ok(())
    }

    fn run_src_with(&mut self, src: &str, define: impl FnMut(&str, Value) -> ()) -> TLResult<()> {
        let full_src = format!("{}\n", src);
        let stage_one = module(&full_src)?;
        let stage_two = desugar_statements(&stage_one)?;
        self.eval_module_with(&stage_two, define)
    }

    pub fn run_src_map(&mut self, src: &str) -> TLResult<HashMap<String, Value>> {
        let mut export = HashMap::new();
        self.run_src_with(src, |name, value| {
            export.insert(name.to_string(), value);
        })?;
        Ok(export)
    }

    pub fn run_src(&mut self, src: &str) -> TLResult<Value> {
        let mut export = Object::new();

        self.run_src_with(src, |name, value| {
            export.insert(name, value);
        })?;

        Ok(Value::Object(ptr(export)))
    }

    pub fn push_scope(&mut self) {
        self.env.push();
    }

    pub fn pop_scope(&mut self) {
        self.env.pop();
    }

    pub fn push_module_path(&mut self, path: Str) {
        self.module_stack.push(path);
    }

    pub fn load_module(&mut self, path: &str) -> TLResult<Value> {
        // Check if we have the module cached
        let cached = self.module_cache.get(path);
        match cached {
            Some(val) => Ok(val.clone()),
            None => {
                // Store existing environment
                let env = mem::replace(&mut self.env, Env::new());
                self.env.push();
                self.init()?;

                let path: Str = path.into();

                // Check if we have circular dependency
                if self.module_stack.iter().any(|existing| existing == &path) {
                    return Err(TLError::CircularDependency);
                }

                self.push_module_path(path.clone());

                let contents = self
                    .loader
                    .load_module(path.as_ref())
                    .ok_or_else(|| TLError::ModuleNotFound(path.as_ref().to_string()))?;

                let output = self.run_src(&contents)?;

                // Pop the module stack and save value
                self.module_cache.insert(path.clone(), output.clone());
                self.module_stack.pop();

                // Reset environment
                self.env = env;
                self.loader.after_load_module();

                Ok(output)
            }
        }
    }

    pub fn define(&mut self, name: impl Into<String>, val: Value) {
        self.env.insert(name, val);
    }

    pub fn define_global(&mut self, name: impl Into<String>, val: Value) {
        self.env.insert_global(name, val);
    }

    pub fn define_builtin(
        &mut self,
        name: impl Into<String>,
        f: impl Fn(&[Value], &mut Engine, Option<&Value>) -> EvalResult<Value> + 'static,
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
            DStatement::Block(body) => {
                ignore.push();
                for stmt in body {
                    self.extract_statement_to(stmt, map, ignore);
                }
                ignore.pop();
            }
            DStatement::Definition(_, name, value) => {
                ignore.insert(name);
                self.extract_expr_to(value, map, ignore);
            }
            DStatement::Return(val) => self.extract_expr_to(val, map, ignore),
            DStatement::Assignment(_, val) => self.extract_expr_to(val, map, ignore),
            DStatement::Loop(body) => self.extract_statement_to(body.as_ref(), map, ignore),
            DStatement::Conditional(cond, then, otherwise) => {
                self.extract_expr_to(cond, map, ignore);
                self.extract_statement_to(then, map, ignore);
                self.extract_statement_to(otherwise, map, ignore);
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
            DExpression::Call(_, f, args) => {
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
            DExpression::Member(_, obj, _) => self.extract_expr_to(obj.as_ref(), map, ignore),
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

    fn iterate_value(
        &mut self,
        val: &Value,
        mut f: impl FnMut(&Value) -> TLResult<()>,
    ) -> TLResult<()> {
        let iter = self.eval_method(val, "iter", &[])?;
        loop {
            match self.eval_method(&iter, "next", &[])? {
                Value::None => break,
                other => f(&other)?,
            }
        }
        Ok(())
    }

    fn eval_unary(&mut self, op: UnaryOp, val: &DExpression) -> EvalResult<Value> {
        use UnaryOp::*;
        use Value::*;

        let val = self.eval_expr(val)?;
        match (op, val) {
            (Negative, Number(x)) => Ok(Number(-x)),
            (Not, Boolean(b)) => Ok(Boolean(!b)),
            (op, other) => {
                println!("{:?}, {:?}", op, other.type_of());
                todo!()
            }
        }
    }

    fn eval_equals(&self, a: &Value, b: &Value) -> bool {
        match (a, b) {
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::String(a), Value::String(b)) => ref_eq(a, b),
            (Value::Function(a), Value::Function(b)) => ref_eq(a.as_ref(), b.as_ref()),
            (Value::Builtin(a), Value::Builtin(b)) => ref_eq(a.as_ref(), b.as_ref()),
            (Value::List(a), Value::List(b)) => ref_eq(a.borrow(), b.borrow()),
            (Value::Object(a), Value::Object(b)) => ref_eq(a.borrow(), b.borrow()),
            (Value::None, Value::None) => true,
            (_, _) => false,
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

        let lhs = self.eval_expr(lhs);
        let rhs = self.eval_expr(rhs);
        match (op, lhs, rhs) {
            (op, Ok(lhs), Ok(rhs)) => match (op, lhs, rhs) {
                (Add, Number(x), Number(y)) => Ok(Number(x + y)),
                (Add, x, y) => self.eval_method(&x, "plus", &[y]),
                (Subtract, Number(x), Number(y)) => Ok(Number(x - y)),
                (Subtract, x, y) => self.eval_method(&x, "subtract", &[y]),
                (Multiply, Number(x), Number(y)) => Ok(Number(x * y)),
                (Multiply, x, y) => self.eval_method(&x, "multiply", &[y]),
                (Divide, Number(x), Number(y)) => Ok(Number(x / y)),
                (Divide, x, y) => self.eval_method(&x, "divide", &[y]),
                (Mod, Number(x), Number(y)) => Ok(Number(x % y)),
                (Mod, x, y) => self.eval_method(&x, "modulo", &[y]),
                (Power, Number(x), Number(y)) => Ok(Number(x.powf(y))),
                (Power, x, y) => self.eval_method(&x, "exponentiate", &[y]),
                (GT, Number(x), Number(y)) => Ok(Boolean(x > y)),
                (GTE, Number(x), Number(y)) => Ok(Boolean(x >= y)),
                (LT, Number(x), Number(y)) => Ok(Boolean(x < y)),
                (LTE, Number(x), Number(y)) => Ok(Boolean(x <= y)),
                (Index, List(l), Number(i)) => l
                    .borrow()
                    .get(i as usize)
                    .cloned()
                    .ok_or_else(|| TLError::OutOfBounds(i as usize)),
                (Equals, x, y) => Ok(Boolean(self.eval_equals(&x, &y))),
                (NotEquals, x, y) => Ok(Boolean(!self.eval_equals(&x, &y))),
                (LogicOr, Boolean(x), Boolean(y)) => Ok(Boolean(x || y)),
                (LogicAnd, Boolean(x), Boolean(y)) => Ok(Boolean(x && y)),
                (Elvis, None, y) => Ok(y),
                (Elvis, x, _) => Ok(x),
                (op, lhs, rhs) => Err(TLError::Syntax(format!(
                    "{:?} {:?} {:?} is not supported",
                    op, lhs, rhs
                ))),
            },
            (LogicOr, Ok(Boolean(true)), _) | (LogicOr, _, Ok(Boolean(true))) => Ok(Boolean(true)),
            (LogicAnd, Ok(Boolean(false)), _) | (LogicAnd, _, Ok(Boolean(false))) => {
                Ok(Boolean(false))
            }
            (_, Err(e), _) | (_, _, Err(e)) => Err(e),
        }
    }

    fn get_string_proto(&self) -> TLResult<Ptr<Object>> {
        match self.lookup("String")? {
            Value::Object(obj) => Ok(obj.clone()),
            other => Err(type_error("Object", other)),
        }
    }

    fn get_list_proto(&self) -> TLResult<Ptr<Object>> {
        match self.lookup("List")? {
            Value::Object(obj) => Ok(obj.clone()),
            other => Err(type_error("Object", other)),
        }
    }

    fn get_number_proto(&self) -> TLResult<Ptr<Object>> {
        match self.lookup("Number")? {
            Value::Object(obj) => Ok(obj.clone()),
            other => Err(type_error("Object", other)),
        }
    }

    fn get_boolean_proto(&self) -> TLResult<Ptr<Object>> {
        match self.lookup("Boolean")? {
            Value::Object(obj) => Ok(obj.clone()),
            other => Err(type_error("Object", other)),
        }
    }

    fn get_function_proto(&self) -> TLResult<Ptr<Object>> {
        match self.lookup("Function")? {
            Value::Object(obj) => Ok(obj.clone()),
            other => Err(type_error("Object", other)),
        }
    }

    fn get_object_proto(&self) -> TLResult<Ptr<Object>> {
        match self.lookup("Object")? {
            Value::Object(obj) => Ok(obj.clone()),
            other => Err(type_error("Object", other)),
        }
    }

    fn get_none_proto(&self) -> TLResult<Ptr<Object>> {
        match self.lookup("NoneProto")? {
            Value::Object(obj) => Ok(obj.clone()),
            other => Err(type_error("Object", other)),
        }
    }

    pub fn instance_of(&self, value: &Value, type_value: &Value) -> bool {
        let proto = match self.get_proto_value(value) {
            Ok(Some(proto)) => proto,
            _ => return false,
        };

        if self.eval_equals(&proto, type_value) {
            true
        } else {
            self.instance_of(&proto, type_value)
        }
    }

    fn get_fields(&self, value: &Value) -> TLResult<Option<Ptr<Object>>> {
        match value {
            Value::Object(obj) => Ok(Some(obj.clone())),
            Value::None => Err(type_error("!None", &Value::None)),
            other => self.get_proto(other),
        }
    }

    fn get_proto(&self, value: &Value) -> TLResult<Option<Ptr<Object>>> {
        match value {
            Value::Object(obj) => {
                let proto = obj.borrow().proto.clone();
                match proto {
                    Some(proto) => Ok(Some(proto)),
                    None => Ok(None),
                }
            }
            Value::List(..) => self.get_list_proto().map(Some),
            Value::String(..) => self.get_string_proto().map(Some),
            Value::Number(..) => self.get_number_proto().map(Some),
            Value::Boolean(..) => self.get_boolean_proto().map(Some),
            Value::Function(..) | Value::Builtin(..) => self.get_function_proto().map(Some),
            Value::None => self.get_none_proto().map(Some),
        }
    }

    fn get_proto_value(&self, value: &Value) -> TLResult<Option<Value>> {
        let proto_obj = self.get_proto(value)?;
        Ok(proto_obj.map(Value::Object))
    }

    fn eval_method(&mut self, obj: &Value, method: &str, args: &[Value]) -> TLResult<Value> {
        let val = self.eval_field(obj, method)?;
        self.call_internal(&val, args)
    }

    fn eval_field(&mut self, parent: &Value, field: &str) -> TLResult<Value> {
        let obj = self.get_fields(parent)?;

        let val = obj
            .and_then(|obj| obj.borrow().get(field))
            .unwrap_or_else(|| Value::None);
        match val {
            Value::Function(f) => {
                let mut new_f = f.borrow().clone();
                new_f.self_value = Some(parent.clone());
                Ok(Value::Function(ptr(new_f)))
            }
            Value::Builtin(f) => {
                let mut new_f = f.as_ref().clone();
                new_f.self_value = Some(parent.clone());
                Ok(Value::Builtin(Rc::new(new_f)))
            }
            other => Ok(other),
        }
    }

    fn eval_member(&mut self, obj: &DExpression, field: &str, is_try: bool) -> TLResult<Value> {
        let parent = self.eval_expr(obj)?;
        if is_try {
            if let Value::None = parent {
                return Ok(parent);
            }
        }
        self.eval_field(&parent, field)
    }

    fn iterate_spread_list(
        &mut self,
        list: &[Spread<DExpression>],
        mut f: impl FnMut(&Value) -> EvalResult<()>,
    ) -> EvalResult<()> {
        for Spread { is_spread, value } in list {
            let value = self.eval_expr(value)?;
            match (is_spread, value) {
                // Optimized form for lists
                (true, Value::List(xs)) => {
                    for x in xs.borrow().iter() {
                        f(x)?;
                    }
                }
                (true, val) => self.iterate_value(&val, &mut f)?,
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

    fn call_internal(&mut self, f: &Value, args: &[Value]) -> EvalResult<Value> {
        match f {
            Value::Function(f) => self.call_function(&f.borrow(), args),
            Value::Builtin(f) => self.call_builtin(f.as_ref(), args),
            other => {
                println!("function: {:?}", other);
                println!("{:?}", self.module_stack);
                Err(type_error("Function", &other))
            }
        }
    }

    fn call(&mut self, f: &Value, args: &[Spread<DExpression>]) -> EvalResult<Value> {
        let args = self.eval_spread_list(args)?;
        self.call_internal(f, &args)
    }

    fn call_builtin(&mut self, f: &BuiltinFn, args: &[Value]) -> EvalResult<Value> {
        self.env.push();
        let BuiltinFn { self_value, func } = f;
        let self_value = self_value.as_ref();
        let val = func(&args, self, self_value)?;
        self.env.pop();
        self.ret_val = val.clone();
        Ok(val)
    }

    fn call_function(&mut self, f: &Function, args: &[Value]) -> EvalResult<Value> {
        let Function {
            self_value,
            params,
            last,
            body,
            closure,
        } = f;
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
            DExpression::Lambda(params, last, body) => Ok(Value::Function(ptr(
                self.create_closure(params, last, body)
            ))),
            DExpression::String(s) => Ok(Value::String(s.to_string().into())),
            DExpression::Call(is_try, f, args) => {
                let f = self.eval_expr(f)?;
                match f {
                    Value::None if *is_try => Ok(Value::None),
                    other => self.call(&other, args),
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
            DExpression::Member(is_try, obj, field) => self.eval_member(obj, field, *is_try),
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
                        obj.mutate_field(field, f)
                    }
                    other => Err(type_error("Object", &other)),
                }
            }
        }
    }

    fn eval_block(&mut self, state: EngineState, block: &[DStatement]) -> EvalResult<EngineState> {
        let cur_state = EngineState::Block;
        for stmt in block {
            let next_state = self.eval_stmt(cur_state, stmt, &mut |_, _| {})?;
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

    pub fn eval_stmt(
        &mut self,
        state: EngineState,
        stmt: &DStatement,
        define: &mut dyn FnMut(&str, Value) -> (),
    ) -> EvalResult<EngineState> {
        // println!("exe {:?}", stmt);
        match stmt {
            DStatement::Block(body) => {
                self.env.push();
                let next_state = self.eval_block(state, body.as_ref())?;
                self.env.pop();
                Ok(next_state)
            }
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
                            let state = self.eval_stmt(state, then.as_ref(), &mut |_, _| {})?;
                            self.env.pop();
                            Ok(state)
                        } else {
                            self.env.push();
                            let state =
                                self.eval_stmt(state, otherwise.as_ref(), &mut |_, _| {})?;
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
                let mut value = self.eval_expr(value)?;

                let rec_value = value.clone();
                value.insert_recursive_reference(name, &rec_value);
                if *is_pub {
                    define(name, value.clone());
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
                    let next_state = self.eval_stmt(state, body.as_ref(), &mut |_, _| {})?;
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

    pub fn print_error(&self, err: &TLError) {
        eprintln!("{}", err);
        println!("{:#?}", self.module_stack);
    }
}
