use crate::{
    common::TLError,
    parser::grammar::{BinaryOp, Expression, Field, LambdaBody, Spread, Statement, UnaryOp},
};
use std::convert::TryFrom;

#[derive(Debug, Clone)]
pub struct DField {
    pub name: String,
    pub value: DExpression,
}

#[derive(Debug, Clone)]
pub enum DExpression {
    Number(f64),
    Boolean(bool),
    String(String),
    Identifier(String),
    List(Vec<Spread<DExpression>>),
    Call(Box<DExpression>, Vec<Spread<DExpression>>),
    Member(Box<DExpression>, String),
    Binary(BinaryOp, Box<DExpression>, Box<DExpression>),
    Unary(UnaryOp, Box<DExpression>),
    Lambda(Vec<String>, Option<String>, Vec<DStatement>),
    Object(Vec<DField>),
    None,
}

#[derive(Debug, Clone)]
pub enum DStatement {
    Assignment(LValue, DExpression),
    Definition(bool, String, DExpression),
    Break,
    Return(DExpression),
    Loop(Box<DStatement>),
    Conditional(DExpression, Box<DStatement>, Box<DStatement>),
    Expression(DExpression),
    Block(Vec<DStatement>),
}

#[derive(Debug, Clone)]
pub enum LValue {
    Member(DExpression, String),
    Index(DExpression, DExpression),
    Identifier(String),
}

fn try_from_list<'a, E, T, U: TryFrom<&'a T, Error = E>>(items: &'a [T]) -> Result<Vec<U>, E> {
    items.iter().map(|item| U::try_from(item)).collect()
}

fn try_from_lambda_body(body: &LambdaBody) -> Result<Vec<DStatement>, TLError> {
    match body {
        LambdaBody::Block(block) => {
            let mut body = try_from_list::<_, _, DStatement>(block)?;
            body.push(DStatement::Return(DExpression::None));
            Ok(body)
        }
        LambdaBody::Expression(expr) => {
            let expr = DExpression::try_from(expr.as_ref())?;
            Ok(vec![DStatement::Return(expr)])
        }
    }
}

impl<'a, E, T, U: TryFrom<&'a T, Error = E>> TryFrom<&'a Spread<T>> for Spread<U> {
    type Error = E;

    fn try_from(from: &'a Spread<T>) -> Result<Self, Self::Error> {
        let Spread { is_spread, value } = from;
        let value = U::try_from(value)?;
        Ok(Spread {
            is_spread: *is_spread,
            value,
        })
    }
}

impl TryFrom<&Field> for DField {
    type Error = TLError;

    fn try_from(field: &Field) -> Result<Self, Self::Error> {
        let value = DExpression::try_from(&field.value)?;
        Ok(DField {
            name: field.name.clone(),
            value,
        })
    }
}

impl TryFrom<&Expression> for DExpression {
    type Error = TLError;

    fn try_from(expr: &Expression) -> Result<Self, Self::Error> {
        match expr {
            Expression::None => Ok(DExpression::None),
            Expression::Int(i) => Ok(DExpression::Number(*i as f64)),
            Expression::Float(f) => Ok(DExpression::Number(*f)),
            Expression::Boolean(b) => Ok(DExpression::Boolean(*b)),
            Expression::Call(function, args) => {
                let function = Self::try_from(function.as_ref())?;
                let args = try_from_list(args)?;
                Ok(DExpression::Call(Box::new(function), args))
            }
            Expression::List(list) => {
                let list = try_from_list::<_, _, Spread<DExpression>>(list)?;
                Ok(DExpression::List(list))
            }
            Expression::Identifier(i) => Ok(DExpression::Identifier(i.clone())),
            Expression::String(s) => Ok(DExpression::String(s.clone())),
            Expression::Member(item, field) => {
                let item = Self::try_from(item.as_ref())?;
                Ok(DExpression::Member(Box::new(item), field.clone()))
            }
            Expression::Binary(op, lhs, rhs) => {
                let lhs = Self::try_from(lhs.as_ref())?;
                let rhs = Self::try_from(rhs.as_ref())?;
                Ok(DExpression::Binary(
                    op.clone(),
                    Box::new(lhs),
                    Box::new(rhs),
                ))
            }
            Expression::Unary(op, operand) => {
                let operand = Self::try_from(operand.as_ref())?;
                Ok(DExpression::Unary(op.clone(), Box::new(operand)))
            }
            Expression::Lambda(params, last, body) => {
                // Desugar lambda body
                let body = try_from_lambda_body(body)?;
                Ok(DExpression::Lambda(params.clone(), last.clone(), body))
            }
            Expression::Object(fields) => {
                let declarations = try_from_list::<_, _, DField>(fields)?;
                Ok(DExpression::Object(declarations))
            }
            Expression::Pipe(lhs, rhs) => {
                let lhs = Self::try_from(lhs.as_ref())?;
                let rhs = Self::try_from(rhs.as_ref())?;

                match rhs {
                    DExpression::Call(f, mut args) => {
                        let mut new_args = vec![Spread {
                            is_spread: false,
                            value: lhs,
                        }];
                        new_args.append(&mut args);
                        Ok(DExpression::Call(f, new_args))
                    }
                    other => Err(TLError::Syntax(format!(
                        "expected function call, found {:?}",
                        other
                    ))),
                }
            }
            Expression::Link(parts) => Ok(DExpression::Identifier(parts.join("::"))),
        }
    }
}

impl TryFrom<&Expression> for LValue {
    type Error = TLError;

    fn try_from(expr: &Expression) -> Result<Self, Self::Error> {
        match expr {
            Expression::Member(item, field) => {
                let item = DExpression::try_from(item.as_ref())?;
                Ok(LValue::Member(item, field.clone()))
            }
            Expression::Binary(BinaryOp::Index, lhs, rhs) => {
                let lhs = DExpression::try_from(lhs.as_ref())?;
                let rhs = DExpression::try_from(rhs.as_ref())?;
                Ok(LValue::Index(lhs, rhs))
            }
            Expression::Identifier(i) => Ok(LValue::Identifier(i.clone())),
            _ => Err(TLError::Syntax(
                "lvalue must be a member, index, or identifier".to_string(),
            )),
        }
    }
}

impl TryFrom<&Statement> for DStatement {
    type Error = TLError;

    fn try_from(stmt: &Statement) -> Result<Self, Self::Error> {
        match stmt {
            Statement::For(name, expr, body) => {
                let expr = DExpression::try_from(expr)?;
                let body = DStatement::try_from(body.as_ref())?;

                Ok(DStatement::Block(vec![
                    DStatement::Definition(
                        false,
                        "__iter__".to_string(),
                        DExpression::Call(
                            Box::new(DExpression::Member(Box::new(expr), "iter".to_string())),
                            Vec::new(),
                        ),
                    ),
                    DStatement::Loop(Box::new(DStatement::Block(vec![
                        DStatement::Definition(
                            false,
                            name.clone(),
                            DExpression::Call(
                                Box::new(DExpression::Member(
                                    Box::new(DExpression::Identifier("__iter__".to_string())),
                                    "next".to_string(),
                                )),
                                Vec::new(),
                            ),
                        ),
                        DStatement::Conditional(
                            DExpression::Binary(
                                BinaryOp::Equals,
                                Box::new(DExpression::Identifier(name.clone())),
                                Box::new(DExpression::None),
                            ),
                            Box::new(DStatement::Break),
                            Box::new(body),
                        ),
                    ]))),
                ]))
            }
            Statement::Block(body) => {
                let body = try_from_list::<_, _, DStatement>(body)?;
                Ok(DStatement::Block(body))
            }
            Statement::Assignment(lhs, rhs) => {
                let lhs = LValue::try_from(lhs)?;
                let rhs = DExpression::try_from(rhs)?;
                Ok(DStatement::Assignment(lhs, rhs))
            }
            Statement::BinaryAssignment(op, x, y) => {
                let lhs = LValue::try_from(x)?;
                let x = DExpression::try_from(x)?;
                let y = DExpression::try_from(y)?;
                let value = DExpression::Binary(*op, Box::new(x), Box::new(y));
                Ok(DStatement::Assignment(lhs, value))
            }
            Statement::While(cond, body) => {
                let cond = DExpression::try_from(cond)?;
                let body = DStatement::try_from(body.as_ref())?;
                let new_body = vec![DStatement::Conditional(
                    cond,
                    Box::new(body),
                    Box::new(DStatement::Break),
                )];
                Ok(DStatement::Loop(Box::new(DStatement::Block(new_body))))
            }
            Statement::Loop(body) => {
                let body = DStatement::try_from(body.as_ref())?;
                Ok(DStatement::Loop(Box::new(body)))
            }
            Statement::Break => Ok(DStatement::Break),
            Statement::Return(expr) => {
                let expr = DExpression::try_from(expr)?;
                Ok(DStatement::Return(expr))
            }
            Statement::Definition(is_pub, name, value) => {
                let value = DExpression::try_from(value)?;
                Ok(DStatement::Definition(*is_pub, name.clone(), value))
            }
            Statement::Function(is_pub, name, params, last, body) => {
                let body = try_from_lambda_body(body)?;
                let lambda = DExpression::Lambda(params.clone(), last.clone(), body);
                Ok(DStatement::Definition(*is_pub, name.clone(), lambda))
            }
            Statement::Conditional(cond, then, otherwise) => {
                let cond = DExpression::try_from(cond)?;
                let then = DStatement::try_from(then.as_ref())?;
                let otherwise = match otherwise {
                    Some(clause) => DStatement::try_from(clause.as_ref()),
                    None => Ok(DStatement::Expression(DExpression::None)),
                }?;
                Ok(DStatement::Conditional(
                    cond,
                    Box::new(then),
                    Box::new(otherwise),
                ))
            }
            Statement::Expression(expr) => {
                let expr = DExpression::try_from(expr)?;
                Ok(DStatement::Expression(expr))
            }
            Statement::Class(is_pub, name, parent, methods) => {
                let declarations = try_from_list::<_, _, DField>(methods)?;
                let obj = Spread {
                    value: DExpression::Object(declarations),
                    is_spread: false,
                };

                // Create class definition call
                let class = if let Some(parent) = parent {
                    let parent = DExpression::try_from(parent)?;
                    let parent = Spread {
                        value: parent,
                        is_spread: false,
                    };
                    let extend = DExpression::Member(
                        Box::new(DExpression::Identifier("class".to_string())),
                        "extend".to_string(),
                    );
                    DExpression::Call(Box::new(extend), vec![parent, obj])
                } else {
                    let define = DExpression::Member(
                        Box::new(DExpression::Identifier("class".to_string())),
                        "define".to_string(),
                    );
                    DExpression::Call(Box::new(define), vec![obj])
                };

                Ok(DStatement::Definition(*is_pub, name.clone(), class))
            }
        }
    }
}

pub fn desugar_expression(expr: &Expression) -> Result<DExpression, TLError> {
    DExpression::try_from(expr)
}

pub fn desugar_statements(statements: &[Statement]) -> Result<Vec<DStatement>, TLError> {
    try_from_list(statements)
}
