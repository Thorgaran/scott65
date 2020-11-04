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
    pub source: String,
    pub output: String,
}

pub fn run(config: Config) -> Result<(), Box<dyn Error>> {
    let source_code = read_scheme_file(&config.source)?;

    let tokens = lexer::lex(&source_code);

    #[cfg(any(feature = "test", test))]
    println!("{}", tokens);

    let program = match parser::parse(tokens) {
        Ok(asm) => asm,
        Err(err) => {
            ParseError::err_pretty_print(&err, &config.source, &source_code);
            return Err(Box::new(err));
        },
    };

    #[cfg(any(feature = "test", test))]
    println!("{}", program);

    let assembly = generator::Assembly::generate(program);

    write_asm_file(&config.output, &assembly)?;

    Ok(())
}

fn read_scheme_file(path: &str) -> Result<String, io::Error> {
    let mut s = String::new();
    File::open(path)?.read_to_string(&mut s)?;

    Ok(s)
}

fn write_asm_file(path: &str, asm: &str) -> Result<(), io::Error> {
    File::create(path)?.write(asm.as_bytes())?;
    
    Ok(())
}
