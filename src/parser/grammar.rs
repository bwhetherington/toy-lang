use peg::parser;

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Power,
    Mod,
    Equals,
    NotEquals,
    GT,
    GTE,
    LT,
    LTE,
    LogicAnd,
    LogicOr,
    Index,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Negative,
    Not,
}

#[derive(Debug, Clone)]
pub enum LambdaBody {
    Block(Vec<Statement>),
    Expression(Box<Expression>),
}

#[derive(Debug, Clone)]
pub struct Field {
    pub name: String,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub struct Spread<T> {
    pub value: T,
    pub is_spread: bool,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Int(i32),
    Float(f64),
    Boolean(bool),
    None,
    Identifier(String),
    String(String),
    List(Vec<Spread<Expression>>),
    Call(Box<Expression>, Vec<Spread<Expression>>),
    Member(Box<Expression>, String),
    Binary(BinaryOp, Box<Expression>, Box<Expression>),
    Unary(UnaryOp, Box<Expression>),
    Lambda(Vec<String>, Option<String>, LambdaBody),
    Pipe(Box<Expression>, Box<Expression>),
    Object(Vec<Field>),
    Link(Vec<String>),
}

fn binary(op: BinaryOp, lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary(op, Box::new(lhs), Box::new(rhs))
}

fn unary(op: UnaryOp, x: Expression) -> Expression {
    Expression::Unary(op, Box::new(x))
}

#[derive(Debug, Clone)]
pub enum Statement {
    Function(bool, String, Vec<String>, Option<String>, LambdaBody),
    Definition(bool, String, Expression),
    Assignment(Expression, Expression),
    BinaryAssignment(BinaryOp, Expression, Expression),
    Conditional(Expression, Vec<Statement>, Option<Vec<Statement>>),
    Return(Expression),
    Expression(Expression),
    Break,
    Loop(Vec<Statement>),
    While(Expression, Vec<Statement>),
}

parser!(pub grammar parser() for str {
    use peg::ParseLiteral;

    rule whitespace()
        = quiet! { [' ' | '\t' | '\r' | '\n'] }

    rule _()
        = whitespace()*

    rule __()
        = whitespace()+

    rule string_content() -> &'input str
        = $((!['"'][_])*)

    pub rule string() -> &'input str
        = ['"'] s:string_content() ['"'] { s }
        / expected!("string")

    rule digit() -> &'input str
        = n:$(['0'..='9']) { n }

    rule int() -> i32
        = n:$(['+' | '-']? digit()+) { n.parse().unwrap() }
        / expected!("Int")

    rule float() -> f64
        = n:$(int() ("." digit()*) ("e" int())?) { n.parse().unwrap() }
        / expected!("Float")

    rule boolean() -> bool
        = "True" { true }
        / "False" { false }
        / expected!("Boolean")

    rule identifier_char() -> &'input str
        = s:$(['a'..='z' | 'A'..='Z' | '_']) { s }

    rule identifier() -> &'input str
        = chars:$(identifier_char() (identifier_char() / digit())*) { chars }
        / expected!("identifier")

    rule link() -> Vec<String>
        = first:to_string(<identifier()>) "::" i:to_string(<identifier()>) ** "::" {
            let mut list = Vec::with_capacity(1 + i.len());
            list.push(first);
            for item in i {
                list.push(item);
            }
            list
        }

    rule comma_separated<T>(r: rule<T>) -> Vec<T>
        = l:r() ** (_ "," _) _ ","? { l }

    rule enclosed<T>(start: &str, end: &str, r: rule<T>) -> T
        = ##parse_string_literal(start) _ inner:r() _ ##parse_string_literal(end) { inner }

    rule enclosed_list<T>(start: &str, end: &str, inner: rule<T>) -> Vec<T>
        = enclosed(start, end, <comma_separated(<inner()>)>)

    rule list_expr() -> Vec<Spread<Expression>>
        = enclosed_list("[", "]", <spread(<expr()>)>)
        / expected!("list")

    rule spread<T>(item: rule<T>) -> Spread<T>
        = "..." _ i:item() { Spread { is_spread: true, value: i } }
        / i:item() { Spread { is_spread: false, value: i } }

    rule arg_list() -> Vec<Spread<Expression>>
        = enclosed_list("(", ")", <spread(<expr()>)>)
        / expected!("argument list")

    rule to_string<T>(r: rule<T>) -> String
        = s:$(r()) { s.to_string() }

    rule param_list() -> (Vec<String>, Option<String>)
        = "(" _ "..." _ last:to_string(<identifier()>) _ ","? _ ")" { (Vec::new(), Some(last)) }
        / "(" _ i:to_string(<identifier()>) ** (_ "," _) "," _ "..." _ last:to_string(<identifier()>) _ ","? _ ")" { (i, Some(last)) }
        / "(" _ i:to_string(<identifier()>) ** (_ "," _) ","? _ ")" { (i, None) }
        / expected!("parameters")

    rule lambda_body() -> LambdaBody
        = b:body() { LambdaBody::Block(b) }
        / e:expr() { LambdaBody::Expression(Box::new(e)) }

    rule lambda() -> Expression
        = p:param_list() _ "=>" _ b:lambda_body() {
            let (params, last) = p;
            Expression::Lambda(params, last, b)
        }
        / expected!("lambda")

    rule field_decl() -> Field
        = f:identifier() _ ":" _ v:expr() {
            Field {
                name: f.to_string(),
                value: v,
            }
        }

    rule object() -> Expression
        = f:enclosed_list("{", "}", <field_decl()>) { Expression::Object(f) }

    rule atom() -> Expression
        = n:float() { Expression::Float(n) }
        / n:int() { Expression::Int(n) }
        / b:boolean() { Expression::Boolean(b) }
        / "None" { Expression::None }
        / s:string() { Expression::String(s.to_string()) }
        / i:identifier() { Expression::Identifier(i.to_string()) }
        / l:link() { Expression::Link(l) }
        / l:list_expr() { Expression::List(l) }
        / object()
        / "(" _ x:expr() _ ")" { x }

    rule arithmetic() -> Expression = precedence! {
        e:lambda() { e }
        --
        x:(@) _ "+" _ y:@ { binary(BinaryOp::Add, x, y) }
        x:(@) _ "-" _ y:@ { binary(BinaryOp::Subtract, x, y) }
        --
        x:(@) _ "*" _ y:@ { binary(BinaryOp::Multiply, x, y) }
        x:(@) _ "/" _ y:@ { binary(BinaryOp::Divide, x, y) }
        x:(@) _ "%" _ y:@ { binary(BinaryOp::Mod, x, y) }
        --
        x:@ _ "^" _ y:(@) { binary(BinaryOp::Power, x, y) }
        --
        x:(@) _ "==" _ y:@ { binary(BinaryOp::Equals, x, y) }
        x:(@) _ "!=" _ y:@ { binary(BinaryOp::NotEquals, x, y) }
        x:(@) _ ">" _ y:@ { binary(BinaryOp::GT, x, y) }
        x:(@) _ ">=" _ y:@ { binary(BinaryOp::GTE, x, y) }
        x:(@) _ "<" _ y:@ { binary(BinaryOp::LT, x, y) }
        x:(@) _ "<=" _ y:@ { binary(BinaryOp::LTE, x, y) }
        --
        x:(@) _ "&&" _ y:@ { binary(BinaryOp::LogicAnd, x, y) }
        --
        x:(@) _ "||" _ y:@ { binary(BinaryOp::LogicOr, x, y) }
        --
        "!" _ x:atom() { unary(UnaryOp::Not, x) }
        "-" _ x:atom() { unary(UnaryOp::Negative, x) }
        --
        x:@ _ "[" _ y:(expr()) "]" { binary(BinaryOp::Index, x, y) }
        x:@ _ "." _ y:(identifier()) { Expression::Member(Box::new(x), y.to_string()) }
        x:@ _ l:arg_list() { Expression::Call(Box::new(x), l) }
        --
        e:atom() { e }
    }

    pub rule expr() -> Expression = precedence! {
        x:(@) _ "|>" _ y:@ { Expression::Pipe(Box::new(x), Box::new(y)) }
        --
        e:arithmetic() { e }
    }

    rule bin_assignment() -> Statement
        = lhs:expr() _ "+=" _ rhs:expr() _ ";" { Statement::BinaryAssignment(BinaryOp::Add, lhs, rhs) }
        / lhs:expr() _ "-=" _ rhs:expr() _ ";" { Statement::BinaryAssignment(BinaryOp::Subtract, lhs, rhs) }
        / lhs:expr() _ "*=" _ rhs:expr() _ ";" { Statement::BinaryAssignment(BinaryOp::Multiply, lhs, rhs) }
        / lhs:expr() _ "/=" _ rhs:expr() _ ";" { Statement::BinaryAssignment(BinaryOp::Divide, lhs, rhs) }
        / lhs:expr() _ "%=" _ rhs:expr() _ ";" { Statement::BinaryAssignment(BinaryOp::Mod, lhs, rhs) }
        / lhs:expr() _ "&&=" _ rhs:expr() _ ";" { Statement::BinaryAssignment(BinaryOp::LogicAnd, lhs, rhs) }
        / lhs:expr() _ "||=" _ rhs:expr() _ ";" { Statement::BinaryAssignment(BinaryOp::LogicOr, lhs, rhs) }
        / lhs:expr() _ "===" _ rhs:expr() _ ";" { Statement::BinaryAssignment(BinaryOp::Equals, lhs, rhs) }
        / lhs:expr() _ "!==" _ rhs:expr() _ ";" { Statement::BinaryAssignment(BinaryOp::NotEquals, lhs, rhs) }
        / lhs:expr() _ "[]=" _ rhs:expr() _ ";" { Statement::BinaryAssignment(BinaryOp::Index, lhs, rhs) }


    rule assignment() -> Statement
        = lhs:expr() _ "=" _ rhs:expr() _ ";" { Statement::Assignment(lhs, rhs) }

    rule statements() -> Vec<Statement>
        = s:statement() ** _ { s }

    pub rule body() -> Vec<Statement>
        = "{" _ s:statements() _ "}" { s }

    rule definition() -> Statement
        = "let" __ i:identifier() _ "=" _ e:expr() _ ";" {
            Statement::Definition(
                false,
                i.to_string(),
                e,
            )
        }

    rule return_statement() -> Statement
        = "return" __ e:expr() _ ";" { Statement::Return(e) }
        / "return" _ ";" { Statement::Return(Expression::None) }

    rule expression_statement() -> Statement
        = e:expr() _ ";" { Statement::Expression(e) }

    rule conditional() -> Statement
        = "if" __ c:expr() __ then:body() _ "else" _ otherwise:body() {
            Statement::Conditional(
                c,
                then,
                Some(otherwise),
            )
        }
        / "if" __ c:expr() __ then:body() {
            Statement::Conditional(
                c, then, None
            )
        }

    rule function() -> Statement
        = "func" __ i:to_string(<identifier()>) _ p:param_list() _ b:body() {
            let (params, last) = p;
            Statement::Function(false, i, params, last, LambdaBody::Block(b))
        }

    rule loop_statement() -> Statement
        = "loop" _ b:body() { Statement::Loop(b) }

    rule while_statement() -> Statement
        = "while" __  c:expr() __ b:body() { Statement::While(c, b) }

    rule statement_content() -> Statement
        = function()
        / conditional()
        / definition()
        / bin_assignment()
        / assignment()
        / return_statement()
        / loop_statement()
        / while_statement()
        / "break" _ ";" { Statement::Break }
        / expression_statement()
        / expected!("statement")

    rule module_decl() -> Statement
        = "pub" __ f:function() {
            match f {
                Statement::Function(_, name, params, last, body) => Statement::Function(true, name, params, last, body),
                _ => unreachable!()
            }
        }
        / function()
        / "pub" __ d:definition() {
            match d {
                Statement::Definition(_, name, value) => Statement::Definition(true, name, value),
                _ => unreachable!()
            }
        }
        / definition()
        / e:expr() _ ";" { Statement::Expression(e) }

    pub rule module() -> Vec<Statement>
        = _ s:module_decl() ** _ _ { s }

    rule statement() -> Statement
        = ";"* _ s:statement_content() _ ";"* { s }
});
