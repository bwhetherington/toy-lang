use peg::{error::ParseError as PegError, str::LineCol};
use std::{error, fmt, rc::Rc};

impl From<PegError<LineCol>> for TLError {
    fn from(err: PegError<LineCol>) -> TLError {
        TLError::Parse {
            desc: err.to_string(),
            line: err.location.line,
            column: err.location.column,
        }
    }
}

pub type TLResult<T> = Result<T, TLError>;

#[derive(Debug)]
pub enum TLError {
    Syntax(String),
    Undefined(String),
    TypeMismatch(String, String),
    OutOfBounds(usize),
    ModuleNotFound(String),
    CircularDependency,
    Parse {
        desc: String,
        line: usize,
        column: usize,
    },
}

impl fmt::Display for TLError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl error::Error for TLError {}

pub type Str = Rc<str>;
