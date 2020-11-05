use super::tokens::*;
use std::fmt;

#[derive(Debug, PartialEq)]
pub struct Program(pub Exp);

#[derive(Debug, PartialEq)]
pub enum Exp {
    Constant(Const),
}

#[derive(Debug, PartialEq)]
pub enum Const {
    UInt(u8),
    Bool(bool),
    Char(char),
}

pub fn parse(tokens: TokList) -> Result<Program, ParseError> {
    let mut tokens = TokPeekable::from(tokens);
    Ok(Program(Exp::from_parse(&mut tokens)?))
}

impl Exp {
    fn from_parse(tokens: &mut TokPeekable) -> Result<Exp, ParseError> {
        if tokens.is_next(TokenKind::OpenParen)? {
            tokens.expect_next(TokenKind::OpenParen)?;

            let exp = Exp::from_parse(tokens)?;

            tokens.expect_next(TokenKind::CloseParen)?;

            Ok(exp)
        } 
        else if tokens.is_next(TokenKind::Literal(Value::Any))? {
            let constant = Const::from_parse(tokens)?;

            Ok(Exp::Constant(constant))
        }
        else {
            Err(tokens.raise_wrong_token(&[
                TokenKind::OpenParen,
                TokenKind::Literal(Value::Any),
            ]))
        }
    }
}

impl Const {
    fn from_parse(tokens: &mut TokPeekable) -> Result<Const, ParseError> { 
        let next_tok = tokens.expect_next(TokenKind::Literal(Value::Any))?;

        if let TokenKind::Literal(ref value) = next_tok.kind {
            match value {
                Value::Any => panic!("Any should only be used for comparison purposes"),
                Value::Int(rad, int_str) => {
                    if rad.is_unknown() {
                        Err(ParseError { kind: 
                            ErrorKind::UnknownRadix(next_tok.clone())
                        })
                    }
                    else {
                        match u8::from_str_radix(&int_str, rad.get_base()) {
                            Ok(uint) => Ok(Const::UInt(uint)),
                            Err(_) => Err(ParseError { kind: 
                                ErrorKind::UIntOverflow(next_tok.clone())
                            }),
                        }
                    }
                },
                Value::Bool(bol) => Ok(Const::Bool(*bol)),
                Value::Char(char_str) => {
                    let character = if char_str.len() == 0 {
                        None
                    }
                    else if char_str.len() == 1 {
                        let c = char_str.chars().next().unwrap();
            
                        if c.is_ascii_graphic() { Some(c) } else { None }
                    }
                    else { match &char_str[..] {
                        "altmode" => Some('\u{001b}'),
                        "backnext" => Some('\u{001f}'),
                        "backspace" => Some('\u{0008}'),
                        "call" => Some('\u{001a}'),
                        "linefeed" | "newline" => Some('\u{000a}'),
                        "page" => Some('\u{000c}'),
                        "return" => Some('\u{000d}'),
                        "rubout" => Some('\u{007f}'),
                        "space" => Some(' '),
                        "tab" => Some('\u{0009}'),
                        _ => None,
                    }};
                    
                    match character {
                        Some(c) => Ok(Const::Char(c)),
                        None => Err(ParseError { kind:
                            ErrorKind::InvalidChar(next_tok.clone()),
                        })
                    }
                },
            }
        }
        else { unreachable![] }
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Program:\n    {}\n", self.0)
    }
}

impl fmt::Display for Exp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Exp::Constant(val) => write!(f, "Constant: {}", val),
        }
    }
}

impl fmt::Display for Const {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Const::UInt(uint) => write!(f, "{}", uint),
            Const::Bool(bol) => write!(f, "{}", bol),
            Const::Char(c) => write!(f, "'{}'", c.escape_default()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokens::Position;

    #[test]
    fn int_with_parentheses() {
        let tokens = TokList::from(vec![
            Token {
                kind: TokenKind::OpenParen,
                pos: Position {line: 0, column: 0}
            },
            Token {
                kind: TokenKind::Literal(Value::Int(Radix::Dec, String::from("42"))),
                pos: Position {line: 0, column: 2}
            },
            Token {
                kind: TokenKind::CloseParen,
                pos: Position {line: 1, column: 0}
            },
        ]);

        assert_eq!(Ok(Program(Exp::Constant(Const::UInt(42)))), parse(tokens));
    }
}