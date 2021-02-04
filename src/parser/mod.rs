pub mod desugar;
pub mod grammar;

pub use desugar::{desugar_statements, DExpression, DField, DStatement, LValue};
pub use grammar::{
  parser::{expr, module},
  BinaryOp, Spread, UnaryOp,
};

pub type Program = Vec<DStatement>;
