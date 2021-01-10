use super::tokens::*;
use std::fmt;

#[derive(Debug, PartialEq)]
pub struct Program(pub Exp);

#[derive(Debug, PartialEq)]
pub enum Exp {
    ProcCall(Proc),
    Constant(Const),
}

#[derive(Debug, PartialEq)]
pub enum Const {
    UInt(u8),
    Bool(bool),
    Char(char),
}

#[derive(Debug, PartialEq)]
pub struct Proc {
    pub name: ProcName,
    pub operands: Vec<Box<Exp>>,
}

#[derive(Debug, PartialEq)]
pub enum ProcName {
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
    GreaterOrEq,
    Less,
}

pub fn parse(tokens: TokList) -> Result<Program, ParseError> {
    let mut tokens = TokPeekable::from(tokens);
    Ok(Program(Exp::from_parse(&mut tokens)?))
}

impl Exp {
    fn from_parse(tokens: &mut TokPeekable) -> Result<Exp, ParseError> {
        if tokens.is_next(TokenKind::OpenParen)? {
            tokens.expect_next(TokenKind::OpenParen)?;
            
            let procedure = Proc::from_parse(tokens)?;

            tokens.expect_next(TokenKind::CloseParen)?;

            Ok(Exp::ProcCall(procedure))
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

impl Proc {
    fn from_parse(tokens: &mut TokPeekable) -> Result<Proc, ParseError> {
        let next_tok = tokens.expect_next(TokenKind::Operator(Operator::Any))?;

        if let TokenKind::Operator(op) = next_tok.kind {
            match op {
                Operator::Any => panic!("Any should only be used for comparison purposes"),
                Operator::Add1 => Ok(Proc{ 
                    name: ProcName::Add1, 
                    operands: vec![Box::from(Exp::from_parse(tokens)?)] 
                }),
                Operator::Sub1 => Ok(Proc{ 
                    name: ProcName::Sub1, 
                    operands: vec![Box::from(Exp::from_parse(tokens)?)] 
                }),
                Operator::Zero => Ok(Proc{ 
                    name: ProcName::Zero, 
                    operands: vec![Box::from(Exp::from_parse(tokens)?)] 
                }),
                Operator::Not => Ok(Proc{ 
                    name: ProcName::Not, 
                    operands: vec![Box::from(Exp::from_parse(tokens)?)] 
                }),
                Operator::BitwiseNot => Ok(Proc{ 
                    name: ProcName::BitwiseNot, 
                    operands: vec![Box::from(Exp::from_parse(tokens)?)] 
                }),
                Operator::Add => Ok(Proc{ 
                    name: ProcName::Add, 
                    operands: vec![
                        Box::from(Exp::from_parse(tokens)?),
                        Box::from(Exp::from_parse(tokens)?)
                    ] 
                }),
                Operator::Sub => Ok(Proc{ 
                    name: ProcName::Sub, 
                    operands: vec![
                        Box::from(Exp::from_parse(tokens)?),
                        Box::from(Exp::from_parse(tokens)?)
                    ] 
                }),
                Operator::Multiply => Ok(Proc{ 
                    name: ProcName::Multiply, 
                    operands: vec![
                        Box::from(Exp::from_parse(tokens)?),
                        Box::from(Exp::from_parse(tokens)?)
                    ] 
                }),
                Operator::Equal => Ok(Proc{ 
                    name: ProcName::Equal, 
                    operands: vec![
                        Box::from(Exp::from_parse(tokens)?),
                        Box::from(Exp::from_parse(tokens)?)
                    ] 
                }),
                Operator::And => Ok(Proc{ 
                    name: ProcName::And, 
                    operands: vec![
                        Box::from(Exp::from_parse(tokens)?),
                        Box::from(Exp::from_parse(tokens)?)
                    ] 
                }),
                Operator::Or => Ok(Proc{ 
                    name: ProcName::Or, 
                    operands: vec![
                        Box::from(Exp::from_parse(tokens)?),
                        Box::from(Exp::from_parse(tokens)?)
                    ] 
                }),
                Operator::Greater => {
                    let mut operands = vec![
                        Box::from(Exp::from_parse(tokens)?),
                        Box::from(Exp::from_parse(tokens)?)
                    ];
                    operands.reverse();
                    Ok(Proc{ name: ProcName::Less, operands })
                },
                Operator::GreaterOrEq => Ok(Proc{ 
                    name: ProcName::GreaterOrEq, 
                    operands: vec![
                        Box::from(Exp::from_parse(tokens)?),
                        Box::from(Exp::from_parse(tokens)?)
                    ]
                }),
                Operator::Less => Ok(Proc{ 
                    name: ProcName::Less, 
                    operands: vec![
                        Box::from(Exp::from_parse(tokens)?),
                        Box::from(Exp::from_parse(tokens)?)
                    ] 
                }),
                Operator::LessOrEq => {
                    let mut operands = vec![
                        Box::from(Exp::from_parse(tokens)?),
                        Box::from(Exp::from_parse(tokens)?)
                    ];
                    operands.reverse();
                    Ok(Proc{ name: ProcName::GreaterOrEq, operands })
                },
            }
        }
        else { unreachable![] }
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
            Exp::ProcCall(procedure) => write!(f, "Procedure: {}", procedure),
            Exp::Constant(val) => write!(f, "Constant: {}", val),
        }
    }
}

impl fmt::Display for Proc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}({})", self.name, self.operands.iter()
            .map(|x| x.to_string())
            .collect::<Vec<String>>()
            .join(", "))
    }
}

impl fmt::Display for ProcName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ProcName::Add1 => write!(f, "add1"),
            ProcName::Sub1 => write!(f, "sub1"),
            ProcName::Zero => write!(f, "zero?"),
            ProcName::Not => write!(f, "not"),
            ProcName::BitwiseNot => write!(f, "bitwise-not"),
            ProcName::Add => write!(f, "+"),
            ProcName::Sub => write!(f, "-"),
            ProcName::Multiply => write!(f, "*"),
            ProcName::Equal => write!(f, "="),
            ProcName::And => write!(f, "and"),
            ProcName::Or => write!(f, "or"),
            ProcName::GreaterOrEq => write!(f, ">="),
            ProcName::Less => write!(f, "<"),
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
    fn simple_int() {
        let tokens = TokList::from(vec![
            Token {
                kind: TokenKind::Literal(Value::Int(Radix::Dec, String::from("42"))),
                pos: Position {line: 0, column: 2}
            },
        ]);

        assert_eq!(Ok(Program(Exp::Constant(Const::UInt(42)))), parse(tokens));
    }
}