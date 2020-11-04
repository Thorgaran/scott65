use std::fs::File;
use std::error::Error;
use tokens::{ParseError, ErrorKind, Position};
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

    let assembly = match compile(&source_code) {
        Ok(asm) => asm,
        Err(err) => {
            err_pretty_print(&err, &config.filename, &source_code);
            return Err(Box::new(err));
        },
    };

    write_asm_file(&config.output, &assembly)?;

    Ok(())
}

pub fn compile(source_code: &str) -> Result<String, ParseError> {
    let tokens = lexer::lex(&source_code);

    #[cfg(any(feature = "test", test))]
   println!("{}", tokens);

    let program = parser::parse(tokens)?;

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

fn err_pretty_print(err: &ParseError, filename: &str, source_code: &str) {
    let err_pos = match &err.kind {
        ErrorKind::WrongToken(_, actual) => actual.pos.clone(),
        ErrorKind::EndOfFile(_) => {
            let line = source_code.lines().count();
            Position {
                line,
                column: source_code.lines().nth(line).unwrap().chars().count(),
            }
        },
    };

    eprintln!("Parsing error: {}", err);
    eprintln!(" --> {}", filename);
    eprintln!("{} | {}", err_pos.line, source_code.lines().nth(err_pos.line).unwrap());
    eprintln!("    {}^", " ".repeat(err_pos.column));
}