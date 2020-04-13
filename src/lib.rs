use std::fs;
use std::path::Path;
use std::io;

use self::scanning::scan_tokens;
use self::parser::parse;
use self::eval::LoxValue;
use self::interpreter::Interpreter;

mod n_peekable;
mod scanning;
mod expr;
mod parser;
mod eval;
mod interpreter;

pub fn run_file(path: &Path) {
    let file_content = match fs::read_to_string(path) {
        Err(why) => panic!("can not read file conents: {:?}", why),
        Ok(s) => s
    };
    if let Err(_) = run(&file_content) {
        std::process::exit(65);
    }
}

pub fn run_prompt() {
    loop {
        let mut line = String::new();
        if let Err(why) = io::stdin().read_line(&mut line) {
            panic!("can not read from stdin: {:?}", why);
        }
        match run(&line) {
            Ok(value) => println!("{}", value),
            Err(s) => println!("{}", s)
        };
    }
}

fn run(s: &str) -> Result<LoxValue,String> {
    let tokens = scan_tokens(s);
    if let Ok(stmts) = parse(tokens) {
        let interpreter = Interpreter::new();
        interpreter.interpret(&stmts);
    }
    Err("sad panda".to_string())
}

