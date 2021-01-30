use peg::parser;

#[derive(Debug)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Power,
    Equals,
    NotEquals,
    GT,
    GTE,
    LT,
    LTE,
    LogicAnd,
    LogicOr,
}

#[derive(Debug)]
pub enum UnaryOp {
    Negative,
    Not,
}

#[derive(Debug)]
pub enum Expression {
    Int(i32),
    Float(f64),
    Boolean(bool),
    None,
    Identifier(String),
    String(String),
    List(Vec<Expression>),
    Call(Box<Expression>, Vec<Expression>),
    Binary(BinaryOp, Box<Expression>, Box<Expression>),
    Unary(UnaryOp, Box<Expression>),
}

fn binary(op: BinaryOp, lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary(op, Box::new(lhs), Box::new(rhs))
}

fn unary(op: UnaryOp, x: Expression) -> Expression {
    Expression::Unary(op, Box::new(x))
}

#[derive(Debug)]
pub enum Statement {
    Function(String, Vec<String>, Vec<Statement>),
    Assignment(String, Expression),
    Conditional(Expression, Vec<Statement>, Vec<Statement>),
    Return(Expression),
    Expression(Expression),
}

parser!(pub grammar parser() for str {
    use peg::ParseLiteral;

    rule whitespace()
        = quiet! { [' ' | '\t' | '\n'] }

    rule _()
        = whitespace()*

    rule __()
        = whitespace()+

    rule string() -> &'input str
        = "\"" s:$(!['"']) "\"" { s }

    rule int() -> i32
        = n:$(['+' | '-']? ['0'..='9']+) { n.parse().unwrap() }
        / expected!("Int")

    rule float() -> f64
        = n:$(int() ("." ['0'..='9']*) ("e" int())?) { n.parse().unwrap() }
        / expected!("Float")

    rule boolean() -> bool
        = "True" { true }
        / "False" { false }
        / expected!("Boolean")

    rule identifier() -> &'input str
        = chars:$(['a'..='z' | 'A'..='Z' | '_']+) { chars }
        / expected!("identifier")

    rule comma_separated<T>(r: rule<T>) -> Vec<T>
        = l:r() ** (_ "," _) { l }

    rule enclosed<T>(start: &str, end: &str, r: rule<T>) -> T
        = ##parse_string_literal(start) _ inner:r() _ ##parse_string_literal(end) { inner }

    rule enclosed_list<T>(start: &str, end: &str, inner: rule<T>) -> Vec<T>
        = enclosed(start, end, <comma_separated(<inner()>)>)

    rule list_expr() -> Vec<Expression>
        = enclosed_list("[", "]", <expr()>)
        / expected!("list")

    rule arg_list() -> Vec<Expression>
        = enclosed_list("(", ")", <expr()>)

    rule call() -> Expression
        = i:identifier() _ l:arg_list() {
            Expression::Call(
                Box::new(Expression::Identifier(i.to_string())),
                l,
            )
        }

    rule atom() -> Expression
        = c:call() { c }
        / n:int() { Expression::Int(n) }
        / n:float() { Expression::Float(n) }
        / b:boolean() { Expression::Boolean(b) }
        / "None" { Expression::None }
        / s:string() { Expression::String(s.to_string()) }
        / i:identifier() { Expression::Identifier(i.to_string()) }
        / l:list_expr() { Expression::List(l) }

    pub rule expr() -> Expression = precedence! {
        x:(@) _ "+" _ y:@ { binary(BinaryOp::Add, x, y) }
        x:(@) _ "-" _ y:@ { binary(BinaryOp::Subtract, x, y) }
        --
        x:(@) _ "*" _ y:@ { binary(BinaryOp::Multiply, x, y) }
        x:(@) _ "/" _ y:@ { binary(BinaryOp::Divide, x, y) }
        --
        x:@ _ "**" _ y:(@) { binary(BinaryOp::Power, x, y) }
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
        "!" _ x:expr() { unary(UnaryOp::Not, x) }
        "-" _ x:expr() { unary(UnaryOp::Negative, x) }
        --
        e:atom() { e }
        "(" _ e:expr() _ ")" { e }
    }

    pub rule statements() -> Vec<Statement>
        = s:statement() ** _ { s }

    pub rule body() -> Vec<Statement>
        = "{" _ s:statements() _ "}" { s }

    rule assignment() -> Statement
        = "let" __ i:identifier() _ "=" _ e:expr() _ ";" {
            Statement::Assignment(
                i.to_string(),
                e,
            )
        }

    rule return_statement() -> Statement
        = "return" __ e:expr() _ ";" { Statement::Return(e) }

    rule expression_statement() -> Statement
        = e:expr() _ ";" { Statement::Expression(e) }

    rule conditional() -> Statement
        = "if" __ c:expr() _ then:body() _ "else" _ otherwise:body() {
            Statement::Conditional(
                c,
                then,
                otherwise,
            )
        }

    rule param_list() -> Vec<&'input str>
        = enclosed_list("(", ")", <identifier()>)

    rule function() -> Statement
        = "fn" __ i:identifier() _ p:param_list() _ b:body() {
            Statement::Function(
                i.to_string(),
                p.into_iter().map(|s| s.to_string()).collect(),
                b,
            )
        }

    pub rule statement() -> Statement
        = s:function() { s }
        / s:conditional() { s }
        / s:assignment() { s }
        / s:return_statement() { s }
        / s:expression_statement() { s }
});
