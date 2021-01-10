#![allow(unused)]
use super::parser::{Program, Exp, Assign, Proc, ProcName, Const};
use std::collections::HashMap;

const STACK_SIZE: usize = 0x2000;

pub struct Assembly {
    asm: Vec<String>,
    label_count: usize,
    pub stack_ptr: usize,
    pub var_map: HashMap<String, usize>,
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

    fn create_labels<S>(&mut self, names: &[S]) -> HashMap<String, String> 
    where
        S: Into<String> + Eq + Copy + std::hash::Hash
    {
        let mut labels = HashMap::new();
        for name in names.iter() {
            labels.insert(
                (*name).into(),
                format!("{}_{}", (*name).into(), self.label_count)
            );
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
            var_map: HashMap::new(),
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
            Exp::Assign(assign) => self.gen_assign(assign),
            Exp::Var(iden) => self.add_inden(format!(
                "lda STACK-{}", 
                self.var_map.get(&iden).expect("Use of unassigned variable")
            )),
            Exp::ExpList(exp_list) => {
                for exp in exp_list.exps.into_iter() {
                    self.gen_exp(*exp);
                }
            },
        }
    }

    fn gen_assign(&mut self, assign: Assign) {
        let var_addr = *self.var_map.entry(assign.iden).or_insert({
            self.stack_ptr += 1;
            
            if self.stack_ptr == STACK_SIZE {
                panic!("Stack overflow");
            };

            self.stack_ptr - 1
        });
        
        self.gen_exp(*assign.val);
        self.add_inden(format!("sta STACK-{}", var_addr));
    }

    fn gen_proc(&mut self, procedure: Proc) {
        // prepare operands
        let mut operands = procedure.operands.into_iter().rev();
        self.gen_exp(*operands.next().expect("procedure with no operands"));
        // only prepare the next operands if the procedure is not AND nor OR
        let leftover_op = if procedure.name != ProcName::And && procedure.name != ProcName::Or {
            for op in operands {
                self.push_stack("sta");
                self.gen_exp(*op);
            }
            None
        }
        else {
            Some(operands)
        };

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
                self.add_inden("lda #0");                                           //     lda #0
                self.add(format!("{}:", lbls.get("mul_nextbit").unwrap()));         // nextbit:
                self.use_stack("cpy", 1);                                           //     cpy op2
                self.add_inden(format!("beq {}", lbls.get("mul_end").unwrap()));    //     beq end
                self.use_stack("lsr", 1);                                           //     lsr op2
                self.add_inden(format!("bcc {}", lbls.get("mul_skip").unwrap()));   //     bcc skip
                self.add_inden("clc");                                              //     clc
                self.use_stack("adc", 0);                                           //     adc op1
                self.add(format!("{}:", lbls.get("mul_skip").unwrap()));            // skip:
                self.use_stack("asl", 0);                                           //     asl op1
                self.add_inden(format!("jmp {}", lbls.get("mul_nextbit").unwrap()));//     jmp nextbit
                self.add(format!("{}:", lbls.get("mul_end").unwrap()));             // end:

                // remove operands from stack
                self.stack_ptr -= 2;
            },
            ProcName::Equal => {
                let lbl = self.create_labels(&["eq_end"]);

                self.pull_stack("cmp");
                self.add_inden("beq 5");
                self.add_inden("lda #0");
                self.add_inden(format!("jmp {}", lbl.get("eq_end").unwrap()));
                self.add_inden("lda #1");
                self.add(format!("{}:", lbl.get("eq_end").unwrap()));
            },
            ProcName::And => {
                let lbl = self.create_labels(&["and_end"]);

                self.add_inden(format!("beq {}", lbl.get("and_end").unwrap())); // shortcut
                self.gen_exp(*leftover_op.unwrap().next().expect("missing 2nd operand"));
                self.add_inden(format!("beq {}", lbl.get("and_end").unwrap()));
                self.add_inden("lda #1");
                self.add(format!("{}:", lbl.get("and_end").unwrap()));
            },
            ProcName::Or => {
                let lbls = self.create_labels(&["or_shortcut", "or_end"]);

                self.add_inden(format!("bne {}", lbls.get("or_shortcut").unwrap())); // shortcut
                self.gen_exp(*leftover_op.unwrap().next().expect("missing 2nd operand"));
                self.add_inden(format!("beq {}", lbls.get("or_end").unwrap()));
                self.add(format!("{}:", lbls.get("or_shortcut").unwrap()));
                self.add_inden("lda #1");
                self.add(format!("{}:", lbls.get("or_end").unwrap()));
            },
            ProcName::GreaterOrEq => {
                let lbl = self.create_labels(&["greater_eq_end"]);

                self.pull_stack("cmp");
                self.add_inden("bcs 5");
                self.add_inden("lda #0");
                self.add_inden(format!("jmp {}", lbl.get("greater_eq_end").unwrap()));
                self.add_inden("lda #1");
                self.add(format!("{}:", lbl.get("greater_eq_end").unwrap()));
            },
            ProcName::Less => {
                let lbl = self.create_labels(&["less_end"]);

                self.pull_stack("cmp");
                self.add_inden("bcc 5");
                self.add_inden("lda #0");
                self.add_inden(format!("jmp {}", lbl.get("less_end").unwrap()));
                self.add_inden("lda #1");
                self.add(format!("{}:", lbl.get("less_end").unwrap()));
            },
            _ => todo!(), // functions?
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