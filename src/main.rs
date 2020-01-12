use std::env;
use std::error::Error;
use std::fs::File;
use std::fs;
use std::io::prelude::*;
use std::path::Path;
use std::io;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() > 2 {
        println!("Usage: rlox [script]");
        std::process::exit(64);
    } else if args.len() == 2 {
        let path = Path::new(&args[1]);
        runFile(path);
    } else if args.len() == 1 {
        runPrompt();
    } else {
        panic!("Fatal: there needs to be at least one argument!");
    }
}

fn runFile(path: &Path) {
    let mut file = match File::open(path) {
        Err(why) => panic!("can't open {}: {}", path.display(),
                                                why.description()),
        Ok(file) => file
    };

    let fileContent = match fs::read_to_string(path) {
        Err(why) => panic!("can not read file conents: {}", why.description()),
        Ok(s) => s
    };
    run(&fileContent);
}

fn runPrompt() {
    loop {
        let mut line = String::new();
        io::stdin().read_line(&mut line);

        run(&line);
    }
}

fn run(s: &str) {
}
