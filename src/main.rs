use std::{
    env, fs,
    io::{self, Write},
    process,
};

use error::LoxError;
use interner::Interner;
use typed_arena::Arena;

use crate::vm::Vm;

mod chunk;
mod compiler;
mod error;
mod interner;
mod object;
mod scanner;
mod vm;

fn main() {
    let args: Vec<String> = env::args().collect();
    let arena = Arena::new();
    let interner = Interner::new(&arena);
    let mut vm = Vm::new(interner);

    match args.len() {
        1 => repl(&mut vm),
        2 => run_file(&mut vm, &args[1]),
        _ => {
            eprintln!("Usage: rlox [path]");
            process::exit(64);
        }
    }
}

fn repl(vm: &mut Vm) {
    loop {
        print!("> ");
        io::stdout().flush().unwrap();

        let mut line = String::new();
        io::stdin()
            .read_line(&mut line)
            .expect("Unable to read line from the REPL");

        if line.is_empty() {
            break;
        }

        vm.interpret(&line).ok();
    }
}

fn run_file(vm: &mut Vm, path: &str) {
    let code = match fs::read_to_string(path) {
        Ok(content) => content,
        Err(error) => {
            eprint!("Unable to read file {}: {}", path, error);
            process::exit(74);
        }
    };

    if let Err(error) = vm.interpret(&code) {
        match error {
            LoxError::Compile => process::exit(65),
            LoxError::Runtime => process::exit(70),
        }
    }
}
