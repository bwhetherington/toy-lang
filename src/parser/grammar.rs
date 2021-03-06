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
    Elvis,
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
    Int(i64),
    Float(f64),
    Boolean(bool),
    None,
    Identifier(String),
    String(String),
    List(Vec<Spread<Expression>>),
    Call(bool, Box<Expression>, Vec<Spread<Expression>>),
    Member(bool, Box<Expression>, String),
    Binary(BinaryOp, Box<Expression>, Box<Expression>),
    Unary(UnaryOp, Box<Expression>),
    Lambda(Vec<String>, Option<String>, LambdaBody),
    Pipe(Box<Expression>, Box<Expression>),
    Object(Vec<Field>),
    New(Box<Expression>, Vec<Spread<Expression>>),
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
    Class(bool, String, Option<Expression>, Vec<Field>),
    Assignment(Expression, Expression),
    BinaryAssignment(BinaryOp, Expression, Expression),
    Conditional(Expression, Box<Statement>, Option<Box<Statement>>),
    Return(Expression),
    Expression(Expression),
    Break,
    Block(Vec<Statement>),
    Loop(Box<Statement>),
    While(Expression, Box<Statement>),
    For(String, Expression, Box<Statement>),
}

parser!(pub grammar parser() for str {
    use peg::ParseLiteral;

    rule linebreak()
        = quiet! { "\r\n" }
        / quiet! { "\n" }

    rule non_linebreak_whitespace()
        = quiet! { [' ' | '\t' ] }

    rule whitespace()
        = non_linebreak_whitespace()
        / linebreak()
        / comment()

    rule comment()
        = "/*" (!("*/")[_])* "*/"
        / "//" (!['\n'][_])*

    rule _()
        = whitespace()*

    rule __()
        = whitespace()+

    rule line_end()
        = non_linebreak_whitespace()* linebreak()
        / _ ";"

    rule string_content() -> &'input str
        = $((!['"'][_])*)

    pub rule string() -> &'input str
        = ['"'] s:string_content() ['"'] { s }
        / expected!("string")

    rule digit() -> &'input str
        = n:$(['0'..='9']) { n }
        / expected!("digit")

    rule int() -> i64
        = n:$(['+' | '-']? digit()+) { n.parse().unwrap() }
        / expected!("Int")

    rule float() -> f64
        = n:$(int() ("." digit()*) ("e" int())?) { n.parse().unwrap() }
        / "NaN" { std::f64::NAN }
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
        = b:body_content() { LambdaBody::Block(b) }
        / e:expr() { LambdaBody::Expression(Box::new(e)) }

    rule lambda() -> Expression
        = p:param_list() _ "=>" _ b:lambda_body() {
            let (params, last) = p;
            Expression::Lambda(params, last, b)
        }
        / expected!("lambda")

    rule method_field() -> Field
        = f:identifier() _ p:param_list() _ b:body_content() {
            let (params, last) = p;
            Field {
                name: f.to_string(),
                value: Expression::Lambda(params, last, LambdaBody::Block(b))
            }
        }

    rule expr_method_field() -> Field
        = f:identifier() _ p:param_list() _ "=>" _ e:expr() {
            let (params, last) = p;
            Field {
                name: f.to_string(),
                value: Expression::Lambda(params, last, LambdaBody::Expression(Box::new(e)))
            }
        }

    rule field_decl() -> Field
        = f:identifier() _ ":" _ v:expr() {
            Field {
                name: f.to_string(),
                value: v,
            }
        }
        / method_field()
        / expr_method_field()

    rule object() -> Expression
        = f:enclosed_list("{", "}", <field_decl()>) { Expression::Object(f) }

    rule atom() -> Expression
        = n:float() { Expression::Float(n) }
        / n:int() { Expression::Int(n) }
        / b:boolean() { Expression::Boolean(b) }
        / "None" { Expression::None }
        / s:string() { Expression::String(s.to_string()) }
        / i:identifier() { Expression::Identifier(i.to_string()) }
        / l:list_expr() { Expression::List(l) }
        / object()
        / "(" _ x:expr() _ ")" { x }

    rule constructor() -> Expression = precedence! {
        x:@ _ "[" _ y:expr() _ "]" { binary(BinaryOp::Index, x, y) }
        x:@ _ "." _ y:identifier() { Expression::Member(false, Box::new(x), y.to_string()) }
        --
        x:atom() { x }
    }

    rule new() -> Expression
        = "new" __ e:constructor() _ l:arg_list() { Expression::New(Box::new(e), l) }

    rule primary_expr() -> Expression = precedence! {
        x:@ _ "[" _ y:expr() _ "]" { binary(BinaryOp::Index, x, y) }
        x:@ _ "?." _ y:identifier() { Expression::Member(true, Box::new(x), y.to_string()) }
        x:@ _ "." _ y:identifier() { Expression::Member(false, Box::new(x), y.to_string()) }
        --
        x:@ _ "?." _ l:arg_list() { Expression::Call(true, Box::new(x), l) }
        x:@ _ l:arg_list() { Expression::Call(false, Box::new(x), l) }
        --
        x:new() { x }
        x:atom() { x }
    }

    rule unary() -> Expression
        = "!" _ x:primary_expr() { unary(UnaryOp::Not, x) }
        / "-" _ x:primary_expr() { unary(UnaryOp::Negative, x) }

    rule arithmetic() -> Expression = precedence! {
        e:lambda() { e }
        --
        x:(@) _ "||" _ y:@ { binary(BinaryOp::LogicOr, x, y) }
        --
        x:(@) _ "&&" _ y:@ { binary(BinaryOp::LogicAnd, x, y) }
        --
        x:(@) _ "==" _ y:@ { binary(BinaryOp::Equals, x, y) }
        x:(@) _ "!=" _ y:@ { binary(BinaryOp::NotEquals, x, y) }
        x:(@) _ ">" _ y:@ { binary(BinaryOp::GT, x, y) }
        x:(@) _ ">=" _ y:@ { binary(BinaryOp::GTE, x, y) }
        x:(@) _ "<" _ y:@ { binary(BinaryOp::LT, x, y) }
        x:(@) _ "<=" _ y:@ { binary(BinaryOp::LTE, x, y) }
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
        x:(@) _ "??" _ y:@ { binary(BinaryOp::Elvis, x, y) }
        --
        e:primary_expr() { e }
        --
        e:unary() { e }
    }
    / expected!("operator")

    pub rule expr() -> Expression = precedence! {
        x:(@) _ "|>" _ y:@ { Expression::Pipe(Box::new(x), Box::new(y)) }
        --
        e:arithmetic() { e }
    }

    rule bin_assignment() -> Statement
        = lhs:expr() _ "+=" _ rhs:expr() line_end() { Statement::BinaryAssignment(BinaryOp::Add, lhs, rhs) }
        / lhs:expr() _ "-=" _ rhs:expr() line_end() { Statement::BinaryAssignment(BinaryOp::Subtract, lhs, rhs) }
        / lhs:expr() _ "*=" _ rhs:expr() line_end() { Statement::BinaryAssignment(BinaryOp::Multiply, lhs, rhs) }
        / lhs:expr() _ "/=" _ rhs:expr() line_end() { Statement::BinaryAssignment(BinaryOp::Divide, lhs, rhs) }
        / lhs:expr() _ "%=" _ rhs:expr() line_end() { Statement::BinaryAssignment(BinaryOp::Mod, lhs, rhs) }
        / lhs:expr() _ "&&=" _ rhs:expr() line_end() { Statement::BinaryAssignment(BinaryOp::LogicAnd, lhs, rhs) }
        / lhs:expr() _ "||=" _ rhs:expr() line_end() { Statement::BinaryAssignment(BinaryOp::LogicOr, lhs, rhs) }
        / lhs:expr() _ "===" _ rhs:expr() line_end() { Statement::BinaryAssignment(BinaryOp::Equals, lhs, rhs) }
        / lhs:expr() _ "!==" _ rhs:expr() line_end() { Statement::BinaryAssignment(BinaryOp::NotEquals, lhs, rhs) }
        / lhs:expr() _ "[]=" _ rhs:expr() line_end() { Statement::BinaryAssignment(BinaryOp::Index, lhs, rhs) }


    rule assignment() -> Statement
        = lhs:expr() _ "=" _ rhs:expr() line_end() { Statement::Assignment(lhs, rhs) }

    rule statements() -> Vec<Statement>
        = s:statement() ** _ { s }

    rule body_content() -> Vec<Statement>
        = "{" _ s:statements() _ "}" { s }

    pub rule body() -> Statement
        = s:body_content() { Statement::Block(s) }

    rule definition() -> Statement
        = "let" __ name:to_string(<identifier()>) _ "=" _ value:expr() line_end() {
            Statement::Definition(
                false,
                name,
                value,
            )
        }

    rule class_method_field() -> Field
        = method_field()
        / m:expr_method_field() line_end() { m }

    rule class_fields() -> Vec<Field>
        = fields:class_method_field() ** _ { fields }

    rule class_body() -> Vec<Field>
        = "{" _ fields:class_fields() _ "}" { fields }

    rule class_definition() -> Statement
        = "class" __ name:to_string(<identifier()>) _ ":" _ parent:expr() _ fields:class_body() {
            Statement::Class(false, name, Some(parent), fields)
        }
        / "class" __ name:to_string(<identifier()>) _ fields:class_body() {
            Statement::Class(false, name, None, fields)
        }

    rule return_statement() -> Statement
        = "return" __ e:expr() line_end() { Statement::Return(e) }
        / "return" line_end() { Statement::Return(Expression::None) }

    rule expression_statement() -> Statement
        = e:expr() line_end() { Statement::Expression(e) }

    rule conditional() -> Statement
        = "if" __ c:expr() __ then:statement() _ "else" _ otherwise:statement() {
            Statement::Conditional(
                c,
                Box::new(then),
                Some(Box::new(otherwise)),
            )
        }
        / "if" __ c:expr() __ then:statement() {
            Statement::Conditional(
                c, Box::new(then), None
            )
        }

    rule function() -> Statement
        = "fn" __ i:to_string(<identifier()>) _ p:param_list() _ b:body_content() {
            let (params, last) = p;
            Statement::Function(false, i, params, last, LambdaBody::Block(b))
        }

    rule loop_statement() -> Statement
        = "loop" _ b:statement() { Statement::Loop(Box::new(b)) }

    rule while_statement() -> Statement
        = "while" __  c:expr() __ b:statement() { Statement::While(c, Box::new(b)) }

    rule for_statement() -> Statement
        = "for" __ i:to_string(<identifier()>) __ "in" __ e:expr() _ s:statement() {
            Statement::For(i, e, Box::new(s))
        }

    rule statement_content() -> Statement
        = function()
        / b:body()
        / conditional()
        / definition()
        / bin_assignment()
        / assignment()
        / return_statement()
        / loop_statement()
        / while_statement()
        / for_statement()
        / "break" line_end() { Statement::Break }
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
        / class_definition()
        / "pub" __ c:class_definition() {
            match c {
                Statement::Class(_, name, parent, value) => Statement::Class(true, name, parent, value),
                _ => unreachable!()
            }
        }
        / definition()
        / bin_assignment()
        / assignment()
        / loop_statement()
        / while_statement()
        / for_statement()
        / conditional()
        / e:expr() line_end() { Statement::Expression(e) }

    pub rule module() -> Vec<Statement>
        = _ s:module_decl() ** _ _ { s }

    pub rule statement() -> Statement
        = line_end()* _ s:statement_content() line_end()* { s }
});
