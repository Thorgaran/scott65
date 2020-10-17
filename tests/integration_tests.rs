use scott65;
use std::fs::File;
use std::io::Read;

pub struct Test {
    pub source_code: String,
    pub expected_output: String,
}

fn read_test_file(name: &str) -> Vec<Test> {
    let mut s = String::new();
    File::open(name)
        .expect("Abort tests: failed to open test file")
        .read_to_string(&mut s)
        .expect("Abort tests: failed to read test file as string");

    let mut tests = Vec::new();
    let tests_iter = s.split('¦');
    for test in tests_iter {
        let mut test_iter = test.split('⟶');
        tests.push(Test {
            source_code: String::from(test_iter.next()
                .expect("Missing source code in test file")),
            expected_output: String::from(test_iter.next()
                .expect("Missing expected output in test file")),
        })
    }

    tests
}

#[test]
fn integers() {
    let tests = read_test_file("tests/integers.txt");
    for test in tests.iter() {
        let output = scott65::lexer::lex(&test.source_code); // Replace lex with full compile suite later
        assert_eq!(format!("{:?}", output), test.expected_output);
    }
}