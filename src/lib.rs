use std::fs::File;
use std::io::{self, Read};
use std::process;

pub mod tokens;
pub mod lexer;

#[derive(Debug, PartialEq)]
pub struct Config {
    pub filename: String,
}

pub fn run(config: Config) {
    let source_code = read_scheme_file(&config.filename).unwrap_or_else(|err| {
        eprintln!("Problem reading source file: {}", err);
        process::exit(1);
    });

    let tokens = lexer::lex(&source_code);

    for token in tokens.iter() {
        println!("{:?}", token);
    }
}

fn read_scheme_file(path: &str) -> Result<String, io::Error> {
    let mut s = String::new();
    File::open(path)?.read_to_string(&mut s)?;

    Ok(s)
}