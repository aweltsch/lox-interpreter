use std::fs;
use std::path::Path;
use std::io;

use self::scanning::scan_tokens;
use self::parser::Parser;
use self::interpreter::LoxValue;
use self::interpreter::Interpreter;

mod n_peekable;
mod scanning;
mod expr;
mod parser;
mod interpreter;
mod native_functions;

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
    let mut parser = Parser::new(tokens);
    let mut interpreter = Interpreter::new();
    match parser.parse() {
        Ok(stmts) => interpreter.interpret(&stmts),
        Err(s) => println!("{}", s)
    };
    Err("sad panda".to_string())
}

