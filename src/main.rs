mod common;
mod module;
mod parser;
mod runtime;

use std::error;
use std::io::{self, prelude::*};

use crate::{
    common::{TLError, TLResult},
    module::FileLoader,
    parser::DStatement,
    runtime::{Engine, EngineState},
};

fn eval_line(line: &str, engine: &mut Engine) -> Result<(), TLError> {
    let parsed = crate::parser::grammar::parser::expr(&line)?;
    let body = crate::parser::desugar::desugar_expression(&parsed)?;
    let res = engine.eval_expr(&body)?;
    println!("{:?}", res);
    Ok(())
}

fn execute_line(line: &str, engine: &mut Engine) -> Result<(), TLError> {
    use std::convert::TryFrom;
    let parsed = crate::parser::grammar::parser::statement(&format!("{};", line))?;
    let body = DStatement::try_from(&parsed)?;
    engine.eval_stmt(EngineState::Run, &body, None)?;
    Ok(())
}

fn process_line(line: &str, engine: &mut Engine) -> Result<(), TLError> {
    let try_expr = eval_line(line, engine);
    if try_expr.is_err() {
        execute_line(line, engine)?;
    }
    Ok(())
}

fn repl(engine: &mut Engine) -> TLResult<()> {
    println!("TL REPL v0.1");
    let stdin = io::stdin();
    print!("> ");
    io::stdout().flush().unwrap();
    for line in stdin.lock().lines() {
        let line = line.unwrap();
        if let Err(e) = process_line(line.trim(), engine) {
            println!("{}", e);
        }
        print!("> ");
        io::stdout().flush().unwrap();
    }
    Ok(())
}

fn run(engine: &mut Engine, src: &str) -> TLResult<()> {
    engine.run_module(src)?;
    repl(engine)?;

    Ok(())
}

fn main() -> Result<(), Box<dyn error::Error>> {
    let file = std::env::args()
        .skip(1)
        .next()
        .unwrap_or_else(|| "test.txt".to_string());

    let mut engine = crate::runtime::init_engine(FileLoader::new());

    match run(&mut engine, &file) {
        Err(e) => engine.print_error(&e),
        _ => (),
    }

    // match run(&input) {
    //     Err(err @ TLError::Parse { .. }) => crate::util::print_with_error(&input, &err),
    //     Err(other) => println!("{:?}", other),
    //     _ => (),
    // }

    Ok(())
}
