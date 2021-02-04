use crate::common::TLError;

pub fn print_with_error(s: &str, err: &TLError) {
    match err {
        TLError::Parse { desc, line, column } => {
            for (row, text) in s.lines().enumerate() {
                println!("| {}", text);
                if row + 1 == *line {
                    print!("> ");
                    for _ in 1..*column {
                        print!(" ");
                    }
                    println!("^");
                    println!("> {}", desc);
                }
            }
        }
        _ => (),
    }
}
