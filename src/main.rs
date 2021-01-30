mod parser;
mod runtime;

use std::fs;

fn main() {
    let input = fs::read_to_string("test.txt").unwrap();
    println!("input:\n--\n{}\n--", input);
    let res = crate::parser::grammar::parser::statements(&input);
    match res {
        Ok(stmt) => println!("{:#?}", stmt),
        Err(err) => println!("{}", err),
    }
}
