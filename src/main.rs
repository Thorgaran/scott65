mod lexer;

fn main() {
    let tokens = lexer::lex("1 2 3 4");
    
    for token in tokens.iter() {
        println!("{:?}", token);
    }
}
