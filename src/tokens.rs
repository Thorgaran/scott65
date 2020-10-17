use std::fmt;

// Used to store the position of a token in the input string
#[derive(Debug, PartialEq)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Ln {}, Col {}", self.line, self.column)
    }
}

#[derive(Debug, PartialEq)]
pub enum Value {
    Int(u8),
}

// Tokens contain their position, to help the parser report meaningful errors
#[derive(Debug, PartialEq)]
pub enum Token {
    OpenParen(Position),
    CloseParen(Position),
    Literal(Value, Position),
}