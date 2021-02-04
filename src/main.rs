mod common;
mod module;
mod parser;
mod runtime;
mod util;

use std::error;
use std::fs;
use std::io::{self, prelude::*};

use crate::{
    common::{TLError, TLResult},
    module::{CacheLoader, FileLoader},
    runtime::{ptr, Engine},
};

fn eval_line(line: &str, engine: &mut Engine) -> Result<(), TLError> {
    let parsed = crate::parser::grammar::parser::expr(&line)?;
    let body = crate::parser::desugar::desugar_expression(&parsed)?;
    let res = engine.eval_expr(&body)?;
    println!("<- {:?}", res);
    Ok(())
}

fn repl(engine: &mut Engine) -> Result<(), Box<dyn error::Error>> {
    println!("TL REPL v0.1");
    let stdin = io::stdin();
    print!("=> ");
    io::stdout().flush()?;
    for line in stdin.lock().lines() {
        let line = line?;
        if let Err(e) = eval_line(line.trim(), engine) {
            println!("{}", e);
        }
        print!("=> ");
        io::stdout().flush()?;
        io::stdout().flush()?;
    }
    Ok(())
}

fn run(input: &str) -> Result<(), Box<dyn error::Error>> {
    let res = crate::parser::grammar::parser::module(&input)?;
    let body = crate::parser::desugar::desugar_statements(&res)?;
    // println!("{:#?}", body);

    let mut engine = crate::runtime::init_engine();
    engine.set_loader(Box::new(CacheLoader::new(FileLoader)));

    engine.eval_program(&body)?;
    repl(&mut engine)?;

    Ok(())
}

fn main() -> Result<(), Box<dyn error::Error>> {
    let file = std::env::args()
        .skip(1)
        .next()
        .unwrap_or_else(|| "test.txt".to_string());

    let input = fs::read_to_string(file)?;

    run(&input)?;

    // match run(&input) {
    //     Err(err @ TLError::Parse { .. }) => crate::util::print_with_error(&input, &err),
    //     Err(other) => println!("{:?}", other),
    //     _ => (),
    // }

    Ok(())
}
