use std::fmt;
use std::ops::Deref;
use std::error::Error;
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

// Wrapper type around Tokens Vector
#[derive(Debug, PartialEq)]
pub struct TokList(Vec<Token>);

impl TokList {
    pub fn from(tokens: Vec<Token>) -> TokList {
        TokList(tokens)
    }
}

// Implement Deref to be able to use Vector methods
impl Deref for TokList {
    type Target = Vec<Token>;

    fn deref(&self) -> &Vec<Token> {
        &self.0
    }
}

impl fmt::Display for TokList {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Tokens:\n    {}\n", self
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<String>>()
            .join("\n    ")
        )
    }
}

// Wrapper type around Tokens Peekable
pub struct TokPeekable(Peekable<Box<dyn Iterator<Item = Token>>>);

impl TokPeekable {
    pub fn from(tokens: TokList) -> TokPeekable {
        TokPeekable((Box::new(tokens.0.into_iter()) as Box<dyn Iterator<Item = Token>>).peekable())
    }

    /// Returns an error if the next token isn't of a given expected kind.
    /// Otherwise, returns the kind of the next token (thus calling next() on the peekable)
    /// 
    /// Note that the values inside the variants are ignored for the comparison.
    /// 
    /// # Examples
    /// 
    /// ```
    /// use scott65::tokens;
    /// use tokens::*;
    /// 
    /// let wrong_expected_token = TokenKind::OpenParen;
    /// let expected_token = TokenKind::Literal(Value::Int(123));
    /// 
    /// let actual_token = Token { 
    ///     kind: TokenKind::Literal(Value::Int(42)),
    ///     pos: Position {line: 0, column: 0}
    /// };
    /// let mut actual_token_peekable = TokPeekable::from(TokList::from(vec![actual_token.clone()]));
    /// 
    /// // Wrong token
    /// assert_eq!(
    ///     actual_token_peekable.expect_next(wrong_expected_token.clone()), 
    ///     Err(ParseError { kind: ErrorKind::WrongToken(vec![wrong_expected_token.clone()], actual_token.clone()) })
    /// );
    /// 
    /// // Right token
    /// assert_eq!(
    ///     actual_token_peekable.expect_next(expected_token.clone()), 
    ///     Ok(actual_token.kind)
    /// );
    /// 
    /// // End of file
    /// assert_eq!(
    ///     actual_token_peekable.expect_next(expected_token.clone()), 
    ///     Err(ParseError { kind: ErrorKind::EndOfFile(expected_token) })
    /// );
    /// ```
    pub fn expect_next(&mut self, expected: TokenKind) -> Result<TokenKind, ParseError> {
        match self.0.peek() {
            Some(tok) => {
                if mem::discriminant(&tok.kind) == mem::discriminant(&expected) {
                    Ok(self.0.next().unwrap().kind)
                } else {
                    Err(ParseError { kind: ErrorKind::WrongToken(vec![expected], tok.clone()) })
                }
            },
            None => Err(ParseError { kind: ErrorKind::EndOfFile(expected) }),
        }
    }

    /// Compares the token to another one, returns true if they are equal and false otherwise.
    /// 
    /// Returns an error if the end of the token list is reached.
    pub fn is_next(&mut self, other: TokenKind) -> Result<bool, ParseError> {
        match self.expect_next(other) {
            Err(err) => match err.kind {
                ErrorKind::WrongToken(_, _) => Ok(false),
                ErrorKind::EndOfFile(_) => Err(err),
            },
            Ok(_) => Ok(true),
        }
    }
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
    /// WrongToken(expected_tokens, actual_token): Expected a token but got a different one.
    WrongToken(Vec<TokenKind>, Token),
    /// EndOfFile(expected_token): Expected a token but reached the end of the source file.
    EndOfFile(TokenKind),
}

impl Error for ParseError {}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            ErrorKind::WrongToken(expected, actual) => {
                match expected.len() {
                    0 => panic!("Empty list of expected tokens"),
                    1 => write!(f, "Expected {}, got {}", expected[0], actual),
                    _ => write!(f, "Expected one of {}, got {}", 
                        expected.iter().map(|x| x.to_string()).collect::<String>(), 
                        actual)
                }
            },
            ErrorKind::EndOfFile(expected) =>
                write!(f, "Expected {} but reached the end of the source file", expected),
        }
    }
}