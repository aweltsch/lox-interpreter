use std::env;
use std::error::Error;
use std::fs;
use std::path::Path;
use std::io;
use std::option::Option;

use self::n_peekable::NPeekable;
use self::scanning::scan_tokens;

mod n_peekable;
mod scanning;

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

fn run(s: &str) -> Result<(), &'static str> {
    let tokens = scan_tokens(s);

    for token in tokens.iter() {
        println!("{:?}", token);
    }

    Ok(())
}

