use scott65::Config;
use std::process;

fn main() {
    let config = Config {
        source: String::from("scm_files/test.scm"),
        output: String::from("scm_files/test.asm")
    };

    if let Err(_) = scott65::run(config) {
        process::exit(1);
    }
}
