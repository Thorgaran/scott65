use super::tokens::*;
use std::fmt;

#[derive(Debug, PartialEq)]
pub struct Program(pub Exp);

#[derive(Debug, PartialEq)]
pub enum Exp {
    Value(Value),
}

pub fn parse(tokens: TokList) -> Result<Program, ParseError> {
    let mut tokens = TokPeekable::from(tokens);
    Ok(Program(Exp::from_parse(&mut tokens)?))
}

impl Exp {
    fn from_parse(tokens: &mut TokPeekable) -> Result<Exp, ParseError> {
        if tokens.is_next(TokenKind::OpenParen)? {
            let exp = Exp::from_parse(tokens)?;
            
            tokens.expect_next(TokenKind::CloseParen)?;

            Ok(exp)
        } 
        else {
            let next_tok = tokens.expect_next(TokenKind::Literal(Value::Int(0)))?;
            
            if let TokenKind::Literal(value) = next_tok {
                Ok(Exp::Value(value))
            }
            else {
                panic!("expect_next didn't return a valid Literal token")
            }
        }
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
            Exp::Value(val) => write!(f, "Value: {}", val),
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
                kind: TokenKind::Literal(Value::Int(42)),
                pos: Position {line: 0, column: 2}
            },
            Token {
                kind: TokenKind::CloseParen,
                pos: Position {line: 1, column: 0}
            },
        ]);

        assert_eq!(Ok(Program(Exp::Value(Value::Int(42)))), parse(tokens));
    }
}