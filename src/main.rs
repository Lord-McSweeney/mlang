pub(crate) mod read;

use read::statement;
use read::tokenize;

use std::env;
use std::fs;

fn main() {
    let args = env::args().collect::<Vec<_>>();

    let Some(file_name) = args.get(1) else {
        eprintln!("Use as {} [filename]", args[0]);
        return;
    };

    let data = match fs::read(&file_name) {
        Ok(data) => data,
        Err(e) => {
            eprintln!("Failed to read file: {}", e);
            return;
        }
    };

    // Ensure text is ASCII, then tokenize it
    for c in &data {
        if *c >= 128 {
            eprintln!("Expecting ASCII text");
            return;
        }
    }

    // SAFETY: The data is ASCII
    let text = unsafe { std::str::from_utf8_unchecked(&data) };
    let tokens = tokenize::tokenize(&text).unwrap();

    println!("{:?}", tokens);

    let statements = statement::parse_statements(&tokens);

    println!("{:?}", statements);
}
