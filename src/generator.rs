#![allow(unused)]
use super::parser::{Program, Exp, Proc, ProcName, Const};

const STACK_SIZE: usize = 0x2000;

pub struct Assembly {
    asm: Vec<String>,
    label_count: usize,
    stack_ptr: usize,
    known_y_val: Option<u8>,
}

impl Into<String> for Assembly {
    fn into(self) -> String {
        self.build()
    }
}

impl Assembly {
    fn add<S: Into<String>>(&mut self, string: S) {
        self.asm.push(string.into());
    }

    fn add_inden<S: Into<String>>(&mut self, string: S) {
        self.add(format!("        {}", string.into()));
    }

    fn create_labels<S: Into<String> + Copy>(&mut self, names: &[S]) -> Vec<String> {
        let mut labels = vec![];
        for name in names.iter() {
            labels.push(format!("{}_{}", (*name).into(), self.label_count));
            self.label_count += 1;
        }
        labels
    }

    fn push_stack<S: Into<String>>(&mut self, string: S) {
        self.add_inden(format!("{} STACK-{}", string.into(), self.stack_ptr));
        self.stack_ptr += 1;
        if self.stack_ptr == STACK_SIZE {
            panic!("Stack overflow");
        };
    }

    fn pull_stack<S: Into<String>>(&mut self, string: S) {
        self.stack_ptr -= 1;
        self.add_inden(format!("{} STACK-{}", string.into(), self.stack_ptr));
    }

    fn use_stack<S: Into<String>>(&mut self, string: S, offset: usize) {
        self.add_inden(format!("{} STACK-{}", string.into(), self.stack_ptr - (offset+1)))
    }

    fn build(&self) -> String {
        self.asm.join("\n")
    }

    pub fn generate(prog: Program) -> String {
        let mut asm = Assembly {
            asm: vec![], 
            label_count: 0, 
            stack_ptr: 0, 
            known_y_val: None 
        };
        
        asm.add("SER_OUT = $6000");
        asm.add("STACK = $3fff\n");
        asm.add_inden(".org $8000\n");
        asm.add("reset:");
        
        match prog {
            Program(exp) => asm.gen_exp(exp),
        };

        asm.add_inden("sta SER_OUT     ;Print A to serial output");
        asm.add_inden("stp             ;Program is over, stop processor\n");

        asm.add_inden(".org $fffa      ;Reset vector root");
        asm.add_inden(".word reset     ;NMI vector");
        asm.add_inden(".word reset     ;Reset vector");
        asm.add_inden(".word reset     ;IRQ vector");

        asm.build()
    }

    fn gen_exp(&mut self, exp: Exp) {
        match exp {
            Exp::ProcCall(procedure) => self.gen_proc(procedure),
            Exp::Constant(constant) => self.add_inden(format!("lda #{}", 
                Assembly::gen_const(constant)
            )),
        }
    }

    fn gen_proc(&mut self, procedure: Proc) {
        // prepare operands
        let mut operands = procedure.operands.into_iter().rev();
        self.gen_exp(*operands.next().expect("procedure with no operands"));
        for op in operands {
            self.push_stack("sta");
            self.gen_exp(*op);
        }

        // execute procedure
        match procedure.name {
            ProcName::Add1 => self.add_inden("inc"),
            ProcName::Sub1 => self.add_inden("dec"),
            ProcName::Zero => {
                self.add_inden("beq 2");
                self.add_inden("lda #1");
                self.add_inden("eor #1");
            },
            ProcName::Not => {
                self.add_inden("beq 2");
                self.add_inden("lda #1");
                self.add_inden("eor #1");
            },
            ProcName::BitwiseNot => {
                self.add_inden("eor #%11111111");
            },
            ProcName::Add => {
                self.add_inden("clc");
                self.pull_stack("adc"); 
            },
            ProcName::Sub => {
                self.add_inden("sec");
                self.pull_stack("sbc");
            },
            ProcName::Multiply => {
                let lbls = self.create_labels(&["mul_nextbit", "mul_skip", "mul_end"]);
                // store operand on the stack
                self.push_stack("sta");

                // compute multiplication using shifts and adds
                if self.known_y_val != Some(0) {
                    self.add_inden("ldy #0");
                    self.known_y_val = Some(0);
                };
                self.add_inden("lda #0");                   //     lda #0
                self.add(format!("{}:", lbls[0]));          // nextbit:
                self.use_stack("cpy", 1);                   //     cpy op2
                self.add_inden(format!("beq {}", lbls[2])); //     beq end
                self.use_stack("lsr", 1);                   //     lsr op2
                self.add_inden(format!("bcc {}", lbls[1])); //     bcc skip
                self.add_inden("clc");                      //     clc
                self.use_stack("adc", 0);                   //     adc op1
                self.add(format!("{}:", lbls[1]));          // skip:
                self.use_stack("asl", 0);                   //     asl op1
                self.add_inden(format!("jmp {}", lbls[0])); //     jmp nextbit
                self.add(format!("{}:", lbls[2]));          // end:

                // remove operands from stack
                self.stack_ptr -= 2;
            },
            ProcName::Equal => {
                let lbl = self.create_labels(&["eq_end"]);

                self.pull_stack("cmp");
                self.add_inden("beq 5");
                self.add_inden("lda #0");
                self.add_inden(format!("jmp {}", lbl[0]));
                self.add_inden("lda #1");
                self.add(format!("{}:", lbl[0]));
            },
            ProcName::GreaterOrEq => {
                let lbl = self.create_labels(&["greater_eq_end"]);

                self.pull_stack("cmp");
                self.add_inden("bcs 5");
                self.add_inden("lda #0");
                self.add_inden(format!("jmp {}", lbl[0]));
                self.add_inden("lda #1");
                self.add(format!("{}:", lbl[0]));
            },
            ProcName::Less => {
                let lbl = self.create_labels(&["less_end"]);

                self.pull_stack("cmp");
                self.add_inden("bcc 5");
                self.add_inden("lda #0");
                self.add_inden(format!("jmp {}", lbl[0]));
                self.add_inden("lda #1");
                self.add(format!("{}:", lbl[0]));
            },
            _ => todo!(),
        }
    }   

    fn gen_const(constant: Const) -> String {
        match constant {
            Const::UInt(uint) => uint.to_string(),
            Const::Bool(true) => String::from("1"),
            Const::Bool(false) => String::from("0"),
            Const::Char(c) => {
                let c_esc = c.escape_default().to_string();
                // if c has been escaped to Unicode
                if c_esc.contains(r"\u{") {
                    (c as usize).to_string()
                }
                // else, c has been escaped to something vasm supports
                else {
                    format!("'{}'", c_esc)
                }
            },
        }
    }
}