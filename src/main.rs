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
    if let crate::runtime::Value::None = res {
    } else {
        println!("{:?}", res);
    }
    Ok(())
}

fn execute_line(line: &str, engine: &mut Engine) -> Result<(), TLError> {
    use std::convert::TryFrom;
    let parsed = crate::parser::grammar::parser::statement(&format!("{}\n", line))?;
    let body = DStatement::try_from(&parsed)?;
    engine.eval_stmt(EngineState::Run, &body, &mut |_, _| {})?;
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
        let line = line.trim();
        if line.len() > 0 {
            if let Err(e) = process_line(line.trim(), engine) {
                println!("{}", e);
            }
        }
        print!("> ");
        io::stdout().flush().unwrap();
    }
    Ok(())
}

fn run(engine: &mut Engine, src: &Option<String>, interactive: bool) -> TLResult<()> {
    let mut has_run = false;
    if let Some(src) = src {
        engine.entry_point(src);
        let src = std::fs::read_to_string(src).expect("file could not be read");
        engine.run_src(&src)?;
        has_run = true;
    }
    if interactive {
        repl(engine)?;
        has_run = true;
    }

    if !has_run {
        println!("No program specified");
    }

    Ok(())
}

fn main() -> Result<(), Box<dyn error::Error>> {
    // Check for interactive flag
    let mut interactive = false;
    let mut file: Option<String> = None;

    for arg in std::env::args().skip(1) {
        if arg == "-i" {
            interactive = true;
        } else {
            file = Some(arg);
            break;
        }
    }

    let mut engine = crate::runtime::init_engine(FileLoader::new());

    match run(&mut engine, &file, interactive) {
        Err(e) => engine.print_error(&e),
        _ => (),
    }

    Ok(())
}
