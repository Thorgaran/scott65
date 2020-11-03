use scott65::Config;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    let config = Config {
        filename: String::from("scm_files/test.scm"),
        output: String::from("scm_files/test.asm")
    };

    scott65::run(config)
}
