use std::env;

fn runFile(path: &str) {
}

fn runPrompt() {
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() > 2 {
        println!("Usage: rlox [script]");
        std::process::exit(64);
    } else if args.len() == 2 {
        runFile(&args[1]);
    } else if args.len() == 1 {
        runPrompt();
    } else {
        panic!("Fatal: there needs to be at least one argument!");
    }
}
