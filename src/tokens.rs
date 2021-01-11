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
    Operator(Operator),
    Keyword(Keyword),
    Identifer(String),
}

// Used to store the position of a token in the input string
#[derive(Debug, PartialEq, Clone)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    // The Any variant is used for comparison purposes
    Any,
    Int(Radix, String),
    Bool(bool),
    Char(String),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Radix {
    Unknown(String),
    Bin,
    Oct,
    Dec,
    Hex,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Operator {
    // The Any variant is used for comparison purposes
    Any,
    Add1,
    Sub1,
    Zero,
    Not,
    BitwiseNot,
    Add,
    Sub,
    Multiply,
    Equal,
    And,
    Or,
    Greater,
    GreaterOrEq,
    Less,
    LessOrEq,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Keyword {
    // The Any variant is used for comparison purposes
    Any,
    Begin,
    Let,
    Set,
    If,
    Define,
}

impl Radix {
    /// Test whether or not the radix is Radix::Unknown
    pub fn is_unknown(&self) -> bool {
        mem::discriminant(self) == mem::discriminant(&Radix::Unknown(String::from("")))
    }

    /// Get the base corresponding to a radix.
    /// Function will panic on Radix::Unknown.
    /// 
    /// # Examples
    /// 
    /// ```
    /// use scott65::tokens::Radix;
    /// 
    /// assert_eq!(Radix::Hex.get_base(), 16);
    /// ```
    pub fn get_base(&self) -> u32 {
        match self {
            Radix::Unknown(_) => panic!("An unknown radix doesn't have a corresponding base"),
            Radix::Bin => 2,
            Radix::Oct => 8,
            Radix::Dec => 10,
            Radix::Hex => 16,
        }
    }
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
            TokenKind::Operator(op) => format!("Operator: {}", op),
            TokenKind::Keyword(kw) => format!("Keyword: {}", kw),
            TokenKind::Identifer(s) => format!("Identifier: \"{}\"", s),
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
        match self {
            Value::Any => write!(f, "any"),
            Value::Int(rad, val) => write!(f, "{}{}", rad, val),
            Value::Bool(bol) => write!(f, "{}", bol),
            Value::Char(c_name) => write!(f, "{}", c_name),
        }
    }
}

impl fmt::Display for Radix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            Radix::Unknown(s) => s,
            Radix::Bin => "#b",
            Radix::Oct => "#o",
            Radix::Dec => "",
            Radix::Hex => "#x",
        })
    }
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            Operator::Any => "any",
            Operator::Add1 => "add1",
            Operator::Sub1 => "sub1",
            Operator::Zero => "zero?",
            Operator::Not => "not",
            Operator::BitwiseNot => "bitwise-not",
            Operator::Add => "+",
            Operator::Sub => "-",
            Operator::Multiply => "*",
            Operator::Equal => "=",
            Operator::And => "AND",
            Operator::Or => "OR",
            Operator::Greater => ">",
            Operator::GreaterOrEq => ">=",
            Operator::Less => "<",
            Operator::LessOrEq => "<=",
        })
    }
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            Keyword::Any => "any",
            Keyword::Begin => "begin",
            Keyword::Let => "let",
            Keyword::Set => "set!",
            Keyword::If => "if",
            Keyword::Define => "define",
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

    pub fn next(&mut self) -> Option<Token> {
        self.0.next()
    }

    /// Returns an error if the next token isn't of a given expected kind.
    /// Otherwise, returns the next token (thus calling next() on the peekable)
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
    /// let expected_token = TokenKind::Literal(Value::Int(Radix::Dec, String::from("123")));
    /// 
    /// let actual_token = Token { 
    ///     kind: TokenKind::Literal(Value::Int(Radix::Dec, String::from("42"))),
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
    ///     Ok(actual_token)
    /// );
    /// 
    /// // End of file
    /// assert_eq!(
    ///     actual_token_peekable.expect_next(expected_token.clone()), 
    ///     Err(ParseError { kind: ErrorKind::EndOfFile(expected_token) })
    /// );
    /// ```
    pub fn expect_next(&mut self, expected: TokenKind) -> Result<Token, ParseError> {
        match self.0.peek() {
            Some(tok) => {
                if mem::discriminant(&tok.kind) == mem::discriminant(&expected) {
                    Ok(self.next().unwrap())
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
        match self.0.peek() {
            Some(tok) => {
                if mem::discriminant(&tok.kind) == mem::discriminant(&other) {
                    Ok(true)
                } else {
                    Ok(false)
                }
            },
            None => Err(ParseError { kind: ErrorKind::EndOfFile(other) }),
        }
    }

    /// Returns a ParseError with a WrongToken kind.ParseError
    /// 
    /// Consumes one element from the tokens iterator.
    pub fn raise_wrong_token(&mut self, expected_tokens: &[TokenKind]) -> ParseError {
        ParseError { kind: ErrorKind::WrongToken(
            expected_tokens.to_vec(),
            self.next().expect("No tokens left when one was expected")
        )}
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
    /// UnknownRadix(token): This radix isn't defined in the Scheme programming language. 
    /// Token must ben an Int Literal.
    UnknownRadix(Token),
    /// UIntOverflow(token): The number given isn't between 0 and 255. 
    /// Token must be an Int Literal.
    UIntOverflow(Token),
    /// InvalidChar(token): Invalid character name (missing or unknown).
    /// The token must be a char Literal.
    InvalidChar(Token),
}

impl ParseError {
    pub fn err_pretty_print(&self, filename: &str, source_code: &str) {
        let err_pos = match &self.kind {
            ErrorKind::WrongToken(_, actual) => actual.pos.clone(),
            ErrorKind::EndOfFile(_) => {
                let line = source_code.lines().count() - 1;
                Position {
                    line,
                    column: source_code.lines().nth(line).unwrap().chars().count(),
                }
            },
            ErrorKind::UnknownRadix(token) => token.pos.clone(),
            ErrorKind::UIntOverflow(token) => token.pos.clone(),
            ErrorKind::InvalidChar(token) => token.pos.clone(),
        };
    
        eprintln!("Parsing error: {}", self);
        eprintln!(" --> {}", filename);
        eprintln!("{} | {}", err_pos.line, source_code.lines().nth(err_pos.line).unwrap());
        eprintln!("    {}^", " ".repeat(err_pos.column));
    }
}

impl Error for ParseError {}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            ErrorKind::WrongToken(expected, actual) => match expected.len() {
                0 => panic!("Empty list of expected tokens"),
                1 => write!(f, "Expected {}, got {}", expected[0], actual),
                _ => write!(f, "Expected one of [{}], got {}", 
                    expected.iter()
                        .map(|x| x.to_string())
                        .collect::<Vec<String>>()
                        .join(", "), 
                    actual)
            },
            ErrorKind::EndOfFile(expected) =>
                write!(f, "Expected {} but reached the end of the source file", expected),
            ErrorKind::UnknownRadix(token) => {
                if let TokenKind::Literal(Value::Int(rad, _)) = &token.kind {
                    write!(f, "Unknown radix: '{}'. The supported radix are '#b', '#o', '#d' and '#x'", rad)
                }
                else {
                    panic!("The token isn't an Int Literal")
                }
            },
            ErrorKind::UIntOverflow(token) => {
                if let TokenKind::Literal(value) = &token.kind {
                    write!(f, "Invalid number: '{}'. The 6502 only supports numbers between 0 and 255", value)
                }
                else {
                    panic!("The token isn't a Literal")
                }
            },
            ErrorKind::InvalidChar(token) => {
                if let TokenKind::Literal(Value::Char(c_name)) = &token.kind {
                    write!(f, "Invalid character name: '{}'. Only simple ASCII is supported", c_name)
                }
                else {
                    panic!("The token isn't a char Literal")
                }
            },
        }
    }
}
