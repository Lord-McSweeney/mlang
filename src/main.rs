pub(crate) mod read;
pub(crate) mod write;

use read::statement;
use read::tokenize;
use write::latex;

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
    let tokens = tokenize::tokenize(&text);

    println!("{:?}", tokens);

    let Ok(tokens) = tokens else {
        return;
    };

    let statements = statement::parse_statements(&tokens);

    println!("{:?}", statements);

    let Ok(statements) = statements else {
        return;
    };

    println!("{}", latex::emit_latex(&statements));
}
