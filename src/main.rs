use scott65::Config;

fn main() {
    let config = Config {
        filename: String::from("scm_files/test.scm"),
    };

    scott65::run(config)
}
