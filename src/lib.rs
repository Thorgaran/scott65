use std::fs::File;
use std::error::Error;
use tokens::ParseError;
use std::io::{self, Read, Write};

pub mod tokens;
pub mod lexer;
pub mod parser;
pub mod generator;

#[derive(Debug, PartialEq)]
pub struct Config {
    pub filename: String,
    pub output: String,
}

pub fn run(config: Config) -> Result<(), Box<dyn Error>> {
    let source_code = read_scheme_file(&config.filename)?;

    let assembly = compile(&source_code)?;

    write_asm_file(&config.output, &assembly)?;

    Ok(())
}

pub fn compile(source_code: &str) -> Result<String, ParseError> {
    let tokens = lexer::lex(&source_code);

    #[cfg(any(feature = "test", test))]
   println!("{}", tokens);

    let program = match parser::parse(tokens) {
        Ok(prgm) => prgm,
        Err(err) => {
            eprintln!("Parsing error: {}", err);
            return Err(err);
        },
    };

    #[cfg(any(feature = "test", test))]
    println!("{}", program);

    Ok(generator::Assembly::generate(program))
}

pub fn read_scheme_file(path: &str) -> Result<String, io::Error> {
    let mut s = String::new();
    File::open(path)?.read_to_string(&mut s)?;

    Ok(s)
}

pub fn write_asm_file(path: &str, asm: &str) -> Result<(), io::Error> {
    File::create(path)?.write(asm.as_bytes())?;
    
    Ok(())
}