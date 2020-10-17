use std::fmt;
use std::mem;
use std::iter::Peekable;

// Tokens contain their position, to help the parser report meaningful errors
#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub pos: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    OpenParen,
    CloseParen,
    Literal(Value),
}

// Used to store the position of a token in the input string
#[derive(Debug, PartialEq, Clone)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Int(u8),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ({})", self.kind, self.pos)
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let token_str = match self {
            TokenKind::OpenParen => String::from("("),
            TokenKind::CloseParen => String::from(")"),
            TokenKind::Literal(val) => format!("Literal: {}", val),
        };

        write!(f, "'{}'", token_str)
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Ln {}, Col {}", self.line, self.column)
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            Value::Int(val) => val,
        })
    }
}

/// Returns an error if the next token isn't of a given expected kind.
/// 
/// Note that the values inside the variants are ignored.
/// 
/// # Examples
/// 
/// ```
/// use scott65::tokens;
/// use tokens::{Token, TokenKind, Position, Value, ParseError, ErrorKind};
/// 
/// let expected_token = TokenKind::Literal(Value::Int(123));
/// 
/// let actual_token = Token { 
///     kind: TokenKind::Literal(Value::Int(42)),
///     pos: Position {line: 0, column: 0}
/// };
/// 
/// assert_eq!(
///     tokens::expect_next(&mut vec![actual_token].into_iter().peekable(), expected_token.clone()), 
///     Ok(())
/// );
/// 
/// let actual_token = Token { 
///     kind: TokenKind::OpenParen,
///     pos: Position {line: 0, column: 0}
/// };
/// 
/// assert_eq!(
///     tokens::expect_next(&mut vec![actual_token.clone()].into_iter().peekable(), expected_token.clone()), 
///     Err(ParseError { kind: ErrorKind::WrongToken(expected_token.clone(), actual_token) })
/// );
/// 
/// assert_eq!(
///     tokens::expect_next(&mut vec![].into_iter().peekable(), expected_token.clone()), 
///     Err(ParseError { kind: ErrorKind::EndOfFile(expected_token) })
/// );
/// ```
pub fn expect_next<T>(tokens: &mut Peekable<T>, expected_token: TokenKind) -> Result<(), ParseError>
where
    T: Iterator<Item = Token>
{
    match tokens.peek() {
        Some(tok) => {
            if mem::discriminant(&tok.kind) != mem::discriminant(&expected_token) {
                return Err(ParseError { kind: ErrorKind::WrongToken(expected_token, tok.clone()) })
            }
        },
        None => return Err(ParseError { kind: ErrorKind::EndOfFile(expected_token) }),
    }

    Ok(())
}

/// The error type for parsing operations of Tokens.
#[derive(Debug, PartialEq, Clone)]
pub struct ParseError {
    pub kind: ErrorKind,
}

/// A list specifying general categories of parsing error.
///
/// It is used with the [`tokens::ParseError`] type.
#[derive(Debug, PartialEq, Clone)]
pub enum ErrorKind {
    /// WrongToken(expected_token, actual_token): Expected a token but got a different one.
    WrongToken(TokenKind, Token),
    /// EndOfFile(expected_token): Expected a token but reached the end of the source file.
    EndOfFile(TokenKind),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            ErrorKind::WrongToken(expected, actual) => 
                write!(f, "Expected {}, got {}", expected, actual),
            ErrorKind::EndOfFile(expected) =>
                write!(f, "Expected {} but reached the end of the source file", expected),
        }
    }
}