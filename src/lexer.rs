extern crate itertools;
use super::tokens::*;
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

    pub fn get_string<F>(&mut self, func: F) -> String
    where F : Fn(&char) -> bool {
        let (_, string): (Vec<Position>, String) = 
            self.input_chars.peeking_take_while(|(_, c)| func(c)).unzip();

        string
    }

    pub fn lex_number(&mut self, pos: Position, radix: Radix) {
        let number_str = self.get_string(|x| x.is_digit(16));
        self.push_token(Token {
            kind: TokenKind::Literal(
                Value::Int(radix, number_str)
            ),
            pos: pos.clone()
        });
    }

    pub fn lex_char(&mut self, pos: Position) {
        let char_str = self.get_string(|x| x.is_ascii());
        self.push_token(Token {
            kind: TokenKind::Literal(
                Value::Char(char_str)
            ),
            pos: pos.clone()
        });
    }
}

/// Translates a Scheme string into Tokens
pub fn lex(input: &str) -> TokList {
    let mut lexer = Lexer::new(input);
    

    while let Some((pos, c)) = lexer.peek() {
        match c {
            '(' => if let Some((pos, _)) = lexer.next() {
                lexer.push_token(Token { kind: TokenKind::OpenParen, pos });
            },
            ')' => if let Some((pos, _)) = lexer.next() {
                lexer.push_token(Token { kind: TokenKind::CloseParen, pos });
            },
            '0'..='9' => {
                let pos = pos.clone();
                lexer.lex_number(pos, Radix::Dec);
            },
            '#' => if let Some((pos, _)) = lexer.next() {
                match lexer.next() {
                    Some((_, 'b')) => lexer.lex_number(pos, Radix::Bin),
                    Some((_, 'o')) => lexer.lex_number(pos, Radix::Oct),
                    Some((_, 'd')) => lexer.lex_number(pos, Radix::Dec),
                    Some((_, 'x')) => lexer.lex_number(pos, Radix::Hex),
                    Some((_, '\\')) => lexer.lex_char(pos),
                    Some((_, 't')) => 
                        lexer.push_token(Token { kind: TokenKind::Literal(Value::Bool(true)), pos }),
                    Some((_, 'f')) => 
                        lexer.push_token(Token { kind: TokenKind::Literal(Value::Bool(false)), pos }),
                    Some((_, c)) => lexer.lex_number(pos, Radix::Unknown(format!("#{}", c))),
                    None => lexer.lex_number(pos, Radix::Unknown(String::from("#"))),
                };
            },
            ' ' => { lexer.next(); },
            _ => panic!("Invalid character: {}", c),
        }
    }

    TokList::from(lexer.tokens)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty() {
        let expected_tokens = TokList::from(vec![]);
        assert_eq!(expected_tokens, lex(""));
    }

    #[test]
    fn paren_and_int() {
        let expected_tokens = TokList::from(vec![
            Token { 
                kind: TokenKind::Literal(Value::Int(
                    Radix::Dec, 
                    String::from("1")
                )), 
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
                kind: TokenKind::Literal(Value::Int(
                    Radix::Dec, 
                    String::from("255")
                )), 
                pos: Position { line: 0, column: 6 }
            },
            Token { 
                kind: TokenKind::CloseParen, 
                pos: Position { line: 0, column: 10 }
            },
            Token { 
                kind: TokenKind::Literal(Value::Int(
                    Radix::Dec, 
                    String::from("0")
                )), 
                pos: Position { line: 0, column: 11 }
            },
        ]);

        assert_eq!(expected_tokens, lex("1(  ) 255 )0"));
    }
}