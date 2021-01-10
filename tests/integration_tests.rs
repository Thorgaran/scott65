extern crate w65c02s;
use scott65;
use w65c02s::{System, W65C02S, State};
use std::fs::{self, File};
use std::io::{Read, Write};
use std::process::Command;

pub struct Test {
    pub name: String,
    pub source_code: String,
    pub expected_output: String,
}

fn read_test_file(name: &str) -> Vec<Test> {
    let mut s = String::new();
    File::open(format!("tests/{}.txt", name))
        .expect("Abort tests: failed to open test file")
        .read_to_string(&mut s)
        .expect("Abort tests: failed to read test file as string");

    let mut tests = Vec::new();
    let tests_iter = s.split('；');
    for test in tests_iter {
        let mut test_iter = test.split(|c| c == 'ː' || c == '⟶');

        if test_iter.clone().count() > 1 {
            tests.push(Test {
                name: String::from(test_iter.next()
                    .expect("Missing test name in test file")
                    .trim()),

                source_code: String::from(test_iter.next()
                    .expect("Missing source code in test file")),
                    
                expected_output: String::from(test_iter.next()
                    .expect("Missing expected output in test file")),
            });
        }
    }

    tests
}

fn run_tests(tests: Vec<Test>) {
    for test in tests.iter() {
        let scm_path = format!("tests/{}.scm", test.name);
        let asm_path = format!("tests/{}.asm", test.name);
        let bin_path = format!("tests/{}.bin", test.name);

        // Store the test scm code into a file that scott65 can read
        File::create(&scm_path)
            .expect("Failed to create scheme source file")
            .write(&test.source_code.as_bytes())
            .expect("Failed to write to scheme source file");

        let config = scott65::Config {
            source: scm_path.clone(),
            output: asm_path.clone(),
        };

        // Compile test scheme code
        scott65::run(config)
            .expect("Compiling error");
        
        // Turn assembly into binary
        assemble(&asm_path, &bin_path);

        // Run the newly created binary and capture the output
        let actual_output = run_bin(&bin_path);

        assert_eq!(actual_output, test.expected_output);

        // Clean up the test files (if the test was successful so far)
        fs::remove_file(scm_path)
            .expect("Failed to remove scheme file");
        fs::remove_file(asm_path)
            .expect("Failed to remove assembly file");
        fs::remove_file(bin_path)
            .expect("Failed to remove binary file");
    }
}

fn assemble(asm_path: &str, bin_path: &str) {
    let vasm_path = r"D:\Documents\GitHub\65C02_Project\vasm6502_oldstyle_win10\vasm6502_oldstyle.exe";
    let vasm_output = Command::new(vasm_path)
        .arg(&asm_path) // input file
        .arg("-Fbin") // binary output module
        .arg("-wdc02") // the processor used is a w65c02s
        .arg("-esc") // allow character escapes
        .arg("-dotdir") // directives are preceeded by a dot
        .arg("-chklabels") // warn when a label matches a mnemonic/directive
        .arg("-wfail") // all warnings are errors
        .arg("-x") // error on undefined symbols
        .args(&["-o", &bin_path]) // output file
        .output()
        .expect("Failed to run vasm");

    // Check whether vasm ran successfully or not
    let stderr_str = String::from_utf8(vasm_output.stderr.clone())
        .expect("Vasm didn't output valid utf8 error data");
    assert!(vasm_output.status.success(), "Vasm error: {}", stderr_str);
}

struct TestSystem {
    mem: [u8; 65536],
    serial: Vec<u8>,
}

/// A system with 32K of RAM, 32K of programmable (EEP)ROM,
/// and a serial port output mapped to $6000.
impl TestSystem {
    pub fn new(program: [u8; 32_768]) -> TestSystem {
        let mut mem: [u8; 65_536] = [0xFF; 65_536];
        // Insert the program into the second half of mem
        mem[0x8000..].copy_from_slice(&program);

        TestSystem { 
            mem,
            serial: Vec::new(),
        }
    }
}

impl System for TestSystem {
    fn read(&mut self, _cpu: &mut W65C02S, addr: u16) -> u8 {
        // all reads return RAM/ROM values directly
        self.mem[addr as usize]
    }

    fn write(&mut self, _cpu: &mut W65C02S, addr: u16, value: u8) {
        if addr == 0x6000 {
            // writing to address $6000 outputs data on a virtual "serial port"
            self.serial.push(value);
        }
        else if addr < 0x8000 {
            // all other writes below $8000 write to RAM
            self.mem[addr as usize] = value
        }
        // writes to the EEPROM are unsuccessful and do nothing
    }
}

fn run_bin(bin_path: &str) -> String {
    let mut program: [u8; 32_768] = [0x00; 32_768];
    File::open(&bin_path)
        .expect("Failed to open binary file")
        .read(&mut program)
        .expect("Failed to read binary file as a 32 768 bytes array");

    let mut system = TestSystem::new(program);
    let mut cpu = W65C02S::new();
    
    // Run program
    while cpu.get_state() != State::Stopped { 
        cpu.step(&mut system); 
    };

    system.serial.iter().map(|x| x.to_string()).collect()
}

#[test]
fn constants() {
    let name = "constants";

    let tests = read_test_file(name);
    run_tests(tests);
}

#[test]
fn operators() {
    let name = "operators";
    let tests = read_test_file(name);
    run_tests(tests);
}

#[test]
fn variables() {
    let name = "variables";
    let tests = read_test_file(name);
    run_tests(tests);
}
