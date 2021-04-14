use crate::{
    common::{TLError, TLResult},
    parser::{
        grammar::{BinaryOp, Expression, Field, LambdaBody, Spread, Statement, UnaryOp},
        identifier::{Identifier, IdentifierEncoder},
    },
};

#[derive(Debug, Clone)]
pub struct DField {
    pub name: Identifier,
    pub value: DExpression,
}

#[derive(Debug, Clone)]
pub enum DExpression {
    Number(f64),
    Boolean(bool),
    String(String),
    Identifier(Identifier),
    List(Vec<Spread<DExpression>>),
    Call(bool, Box<DExpression>, Vec<Spread<DExpression>>),
    Member(bool, Box<DExpression>, Identifier),
    Binary(BinaryOp, Box<DExpression>, Box<DExpression>),
    Unary(UnaryOp, Box<DExpression>),
    Lambda(Vec<Identifier>, Option<Identifier>, Vec<DStatement>),
    Object(Vec<DField>),
    None,
}

impl AsRef<Expression> for Expression {
    fn as_ref(&self) -> &Self {
        self
    }
}

impl AsRef<Statement> for Statement {
    fn as_ref(&self) -> &Self {
        self
    }
}

impl<T> AsRef<Spread<T>> for Spread<T> {
    fn as_ref(&self) -> &Self {
        self
    }
}

impl AsRef<Field> for Field {
    fn as_ref(&self) -> &Self {
        self
    }
}

impl AsRef<LambdaBody> for LambdaBody {
    fn as_ref(&self) -> &Self {
        self
    }
}

#[derive(Debug, Clone)]
pub enum DStatement {
    Assignment(LValue, DExpression),
    Definition(bool, Identifier, DExpression),
    Break,
    Return(DExpression),
    Loop(Box<DStatement>),
    Conditional(DExpression, Box<DStatement>, Box<DStatement>),
    Expression(DExpression),
    Block(Vec<DStatement>),
}

#[derive(Debug, Clone)]
pub enum LValue {
    Member(DExpression, Identifier),
    Index(DExpression, DExpression),
    Identifier(Identifier),
}

#[derive(Debug)]
pub struct Desugarer {
    ctx: IdentifierEncoder,
}

impl Desugarer {
    pub fn new() -> Self {
        Self {
            ctx: IdentifierEncoder::new(),
        }
    }

    pub fn identifier<T: Into<String>>(&mut self, ident: T) -> Identifier {
        self.ctx.encode(ident)
    }

    pub fn reverse_identifier(&self, ident: Identifier) -> Option<&String> {
        self.ctx.decode(ident)
    }

    fn lvalue(&mut self, expr: impl AsRef<Expression>) -> TLResult<LValue> {
        match expr.as_ref() {
            Expression::Member(false, item, field) => {
                let item = self.expression(item)?;
                Ok(LValue::Member(item, self.identifier(field)))
            }
            Expression::Binary(BinaryOp::Index, lhs, rhs) => {
                let lhs = self.expression(lhs)?;
                let rhs = self.expression(rhs)?;
                Ok(LValue::Index(lhs, rhs))
            }
            Expression::Identifier(i) => Ok(LValue::Identifier(self.identifier(i))),
            _ => Err(TLError::Syntax(
                "lvalue must be a member, index, or identifier".to_string(),
            )),
        }
    }

    pub fn statement(&mut self, stmt: impl AsRef<Statement>) -> TLResult<DStatement> {
        match stmt.as_ref() {
            Statement::For(name, expr, body) => {
                let expr = self.expression(expr)?;
                let body = self.statement(body)?;
                let name = self.identifier(name);

                Ok(DStatement::Block(vec![
                    DStatement::Definition(
                        false,
                        self.identifier("__iter__"),
                        DExpression::Call(
                            false,
                            Box::new(DExpression::Member(
                                false,
                                Box::new(expr),
                                self.identifier("iter"),
                            )),
                            Vec::new(),
                        ),
                    ),
                    DStatement::Loop(Box::new(DStatement::Block(vec![
                        DStatement::Definition(
                            false,
                            name,
                            DExpression::Call(
                                false,
                                Box::new(DExpression::Member(
                                    false,
                                    Box::new(DExpression::Identifier(self.identifier("__iter__"))),
                                    self.identifier("next"),
                                )),
                                Vec::new(),
                            ),
                        ),
                        DStatement::Conditional(
                            DExpression::Binary(
                                BinaryOp::Equals,
                                Box::new(DExpression::Identifier(name)),
                                Box::new(DExpression::None),
                            ),
                            Box::new(DStatement::Break),
                            Box::new(body),
                        ),
                    ]))),
                ]))
            }
            Statement::Block(body) => {
                let body = self.statement_list(body)?;
                Ok(DStatement::Block(body))
            }
            Statement::Assignment(lhs, rhs) => {
                let lhs = self.lvalue(lhs)?;
                let rhs = self.expression(rhs)?;
                Ok(DStatement::Assignment(lhs, rhs))
            }
            Statement::BinaryAssignment(op, x, y) => {
                let lhs = self.lvalue(x)?;
                let x = self.expression(x)?;
                let y = self.expression(y)?;
                let value = DExpression::Binary(*op, Box::new(x), Box::new(y));
                Ok(DStatement::Assignment(lhs, value))
            }
            Statement::While(cond, body) => {
                let cond = self.expression(cond)?;
                let body = self.statement(body)?;
                let new_body = vec![DStatement::Conditional(
                    cond,
                    Box::new(body),
                    Box::new(DStatement::Break),
                )];
                Ok(DStatement::Loop(Box::new(DStatement::Block(new_body))))
            }
            Statement::Loop(body) => {
                let body = self.statement(body.as_ref())?;
                Ok(DStatement::Loop(Box::new(body)))
            }
            Statement::Break => Ok(DStatement::Break),
            Statement::Return(expr) => {
                let expr = self.expression(expr)?;
                Ok(DStatement::Return(expr))
            }
            Statement::Definition(is_pub, name, value) => {
                let value = self.expression(value)?;
                Ok(DStatement::Definition(
                    *is_pub,
                    self.identifier(name),
                    value,
                ))
            }
            Statement::Function(is_pub, name, params, last, body) => {
                let body = self.lambda_body(body)?;
                let lambda = DExpression::Lambda(
                    self.identifier_list(params),
                    self.identifier_option(last.as_ref()),
                    body,
                );
                Ok(DStatement::Definition(
                    *is_pub,
                    self.identifier(name),
                    lambda,
                ))
            }
            Statement::Conditional(cond, then, otherwise) => {
                let cond = self.expression(cond)?;
                let then = self.statement(then)?;
                let otherwise = match otherwise {
                    Some(clause) => self.statement(clause),
                    None => Ok(DStatement::Expression(DExpression::None)),
                }?;
                Ok(DStatement::Conditional(
                    cond,
                    Box::new(then),
                    Box::new(otherwise),
                ))
            }
            Statement::Expression(expr) => {
                let expr = self.expression(expr)?;
                Ok(DStatement::Expression(expr))
            }
            Statement::Class(is_pub, name, parent, methods) => {
                let declarations = self.field_list(methods)?;
                let obj = Spread {
                    value: DExpression::Object(declarations),
                    is_spread: false,
                };

                // Create class definition call
                let class = if let Some(parent) = parent {
                    let parent = self.expression(parent)?;
                    let parent = Spread {
                        value: parent,
                        is_spread: false,
                    };
                    let extend = DExpression::Identifier(self.identifier("__extend_class__"));
                    DExpression::Call(false, Box::new(extend), vec![parent, obj])
                } else {
                    let define = DExpression::Identifier(self.identifier("__define_class__"));
                    DExpression::Call(false, Box::new(define), vec![obj])
                };

                Ok(DStatement::Definition(
                    *is_pub,
                    self.identifier(name),
                    class,
                ))
            }
        }
    }

    fn identifier_option<T: Into<String>>(&mut self, identifier: Option<T>) -> Option<Identifier> {
        identifier.map(|ident| self.identifier(ident))
    }

    fn identifier_list<T: ToString>(&mut self, identifiers: impl AsRef<[T]>) -> Vec<Identifier> {
        identifiers
            .as_ref()
            .iter()
            .map(|i| self.identifier(i.to_string()))
            .collect()
    }

    pub fn expression(&mut self, expr: impl AsRef<Expression>) -> TLResult<DExpression> {
        match expr.as_ref() {
            Expression::None => Ok(DExpression::None),
            Expression::Int(i) => Ok(DExpression::Number(*i as f64)),
            Expression::Float(f) => Ok(DExpression::Number(*f)),
            Expression::Boolean(b) => Ok(DExpression::Boolean(*b)),
            Expression::Call(is_try, function, args) => {
                let function = self.expression(function.as_ref())?;
                let args = self.spread_expression_list(args)?;
                Ok(DExpression::Call(*is_try, Box::new(function), args))
            }
            Expression::New(class, args) => {
                let class = self.expression(class)?;
                let args = self.spread_expression_list(args)?;
                Ok(DExpression::Call(
                    false,
                    Box::new(DExpression::Member(
                        false,
                        Box::new(class),
                        self.identifier("new"),
                    )),
                    args,
                ))
            }
            Expression::List(list) => {
                let list = self.spread_expression_list(list)?;
                Ok(DExpression::List(list))
            }
            Expression::Identifier(i) => Ok(DExpression::Identifier(self.identifier(i))),
            Expression::String(s) => Ok(DExpression::String(desugar_string(s))),
            Expression::Member(is_try, item, field) => {
                let item = self.expression(item.as_ref())?;
                Ok(DExpression::Member(
                    *is_try,
                    Box::new(item),
                    self.identifier(field),
                ))
            }
            Expression::Binary(op, lhs, rhs) => {
                let lhs = self.expression(lhs)?;
                let rhs = self.expression(rhs)?;
                Ok(DExpression::Binary(
                    op.clone(),
                    Box::new(lhs),
                    Box::new(rhs),
                ))
            }
            Expression::Unary(op, operand) => {
                let operand = self.expression(operand)?;
                Ok(DExpression::Unary(op.clone(), Box::new(operand)))
            }
            Expression::Lambda(params, last, body) => {
                // Desugar lambda body
                let body = self.lambda_body(body)?;
                Ok(DExpression::Lambda(
                    self.identifier_list(params),
                    self.identifier_option(last.as_ref()),
                    body,
                ))
            }
            Expression::Object(fields) => {
                let declarations = self.field_list(fields)?;
                Ok(DExpression::Object(declarations))
            }
            Expression::Pipe(lhs, rhs) => {
                let lhs = self.expression(lhs)?;
                let rhs = self.expression(rhs)?;

                match rhs {
                    DExpression::Call(is_try, f, mut args) => {
                        let mut new_args = vec![Spread {
                            is_spread: false,
                            value: lhs,
                        }];
                        new_args.append(&mut args);
                        Ok(DExpression::Call(is_try, f, new_args))
                    }
                    other => Err(TLError::Syntax(format!(
                        "expected function call, found {:?}",
                        other
                    ))),
                }
            }
        }
    }

    fn spread_expression(
        &mut self,
        spread: impl AsRef<Spread<Expression>>,
    ) -> TLResult<Spread<DExpression>> {
        let Spread { is_spread, value } = spread.as_ref();
        let value = self.expression(value)?;
        Ok(Spread {
            is_spread: *is_spread,
            value,
        })
    }

    fn field_list(&mut self, fields: impl AsRef<[Field]>) -> TLResult<Vec<DField>> {
        fields
            .as_ref()
            .iter()
            .map(|field| self.field(field))
            .collect()
    }

    pub fn statement_list(&mut self, stmts: impl AsRef<[Statement]>) -> TLResult<Vec<DStatement>> {
        stmts
            .as_ref()
            .iter()
            .map(|stmt| self.statement(stmt))
            .collect()
    }

    fn spread_expression_list(
        &mut self,
        exprs: impl AsRef<[Spread<Expression>]>,
    ) -> TLResult<Vec<Spread<DExpression>>> {
        exprs
            .as_ref()
            .iter()
            .map(|expr| self.spread_expression(expr))
            .collect()
    }

    fn lambda_body(&mut self, body: impl AsRef<LambdaBody>) -> TLResult<Vec<DStatement>> {
        match body.as_ref() {
            LambdaBody::Block(block) => {
                let mut body = self.statement_list(block)?;
                body.push(DStatement::Return(DExpression::None));
                Ok(body)
            }
            LambdaBody::Expression(expr) => {
                let expr = self.expression(expr.as_ref())?;
                Ok(vec![DStatement::Return(expr)])
            }
        }
    }

    fn field(&mut self, field: impl AsRef<Field>) -> TLResult<DField> {
        let field = field.as_ref();
        let value = self.expression(&field.value)?;
        Ok(DField {
            name: self.identifier(&field.name),
            value,
        })
    }
}

fn desugar_string(s: &str) -> String {
    let mut out = s.replace("\\n", "\n");
    out = out.replace("\\t", "\t");
    out = out.replace("\\r", "\r");
    out
}
