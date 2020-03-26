use std::error::Error;
use std::fs;
use std::path::Path;
use std::io;

use self::scanning::scan_tokens;
use self::parser::parse;
use self::eval::evaluate;

mod n_peekable;
mod scanning;
mod expr;
mod parser;
mod eval;

pub fn run_file(path: &Path) {
    let file_content = match fs::read_to_string(path) {
        Err(why) => panic!("can not read file conents: {}", why.description()),
        Ok(s) => s
    };
    match run(&file_content) {
        Ok(v) => v,
        Err(_) => std::process::exit(65)
    }
}

pub fn run_prompt() {
    loop {
        let mut line = String::new();
        match io::stdin().read_line(&mut line) {
            Err(why) => panic!("can not read from stdin"),
            Ok(s) => s
        };
        // ignore error while running script
        match run(&line) {
            Ok(v) => v,
            Err(_) => ()
        };
    }
}

fn run(s: &str) -> Result<(),String> {
    let mut tokens = scan_tokens(s);
    let ast = parse(tokens);
    if let Some(expr) = ast {
        evaluate(&expr);
    }
    Ok(())
}

