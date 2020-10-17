extern crate itertools;
use super::tokens::{Token, TokenKind, Position, Value};
use itertools::Itertools;
use std::iter::Peekable;
use std::vec::IntoIter;

pub struct Lexer {
    input_chars: Peekable<IntoIter<(Position, char)>>,
    pub tokens: Vec<Token>,
}

impl Lexer {
    pub fn new(input: &str) -> Lexer {
        // Creates a vector of tuples (Position, char)
        let input_chars_vec: Vec<(Position, char)> = input.lines()
            .enumerate()
            .flat_map(|(line, line_contents)| line_contents.chars()
                .enumerate()
                .map(move |(column, c)| (Position { line, column }, c)))
            .collect();
        
        Lexer {
            input_chars: input_chars_vec.into_iter().peekable(),
            tokens: Vec::new(), 
        }
    }

    pub fn push_token(&mut self, token: Token) {
        self.tokens.push(token);
    }

    pub fn next(&mut self) -> Option<(Position, char)> {
        self.input_chars.next()
    }

    pub fn drop(&mut self) {
        self.input_chars.next();
    }

    pub fn peek(&mut self) -> Option<&(Position, char)> {
        self.input_chars.peek()
    }

    pub fn get_string<F>(&mut self, first_char: char, func: F) -> String
    where F : Fn(&char) -> bool {
        let (_, mut string): (Vec<Position>, String) = 
            self.input_chars.peeking_take_while(|(_, c)| func(c)).unzip();
        string.insert(0, first_char);

        string
    }
}

/// Translates a Scheme string into Tokens
pub fn lex(input: &str) -> Vec<Token> {
    let mut lexer = Lexer::new(input);
    

    while let Some((pos, c)) = lexer.next() {
        match c {
            '(' => lexer.push_token(Token { kind: TokenKind::OpenParen, pos }),
            ')' => lexer.push_token(Token { kind: TokenKind::CloseParen, pos }),
            '0'..='9' => {
                let number_str = lexer.get_string(c, |x| x.is_ascii_digit());
                lexer.push_token(Token {
                    kind: TokenKind::Literal(
                        Value::Int(number_str.parse().unwrap())
                    ),
                    pos
                });
            },
            ' ' => continue,
            _ => panic!("Invalid character"),
        }
    }

    lexer.tokens
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty() {
        let expected_tokens: Vec<Token> = vec![];
        assert_eq!(expected_tokens, lex(""));
    }

    #[test]
    fn paren_and_int() {
        let expected_tokens = vec![
            Token { 
                kind: TokenKind::Literal(Value::Int(1)), 
                pos: Position { line: 0, column: 0 }
            },
            Token { 
                kind: TokenKind::OpenParen, 
                pos: Position { line: 0, column: 1 }
            },
            Token { 
                kind: TokenKind::CloseParen, 
                pos: Position { line: 0, column: 4 }
            },
            Token { 
                kind: TokenKind::Literal(Value::Int(255)), 
                pos: Position { line: 0, column: 6 }
            },
            Token { 
                kind: TokenKind::CloseParen, 
                pos: Position { line: 0, column: 10 }
            },
            Token { 
                kind: TokenKind::Literal(Value::Int(0)), 
                pos: Position { line: 0, column: 11 }
            },
        ];

        assert_eq!(expected_tokens, lex("1(  ) 255 )0"));
    }
}