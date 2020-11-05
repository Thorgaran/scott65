use super::parser::{Program, Exp, Const};

pub struct Assembly {
    asm: Vec<String>,
}

impl Into<String> for Assembly {
    fn into(self) -> String {
        self.build()
    }
}

impl Assembly {
    fn add<S: Into<String>>(&mut self, string: S) {
        self.asm.push(string.into())
    }

    fn add_inden<S: Into<String>>(&mut self, string: S) {
        self.add(format!("        {}", string.into()));
    }

    fn build(&self) -> String {
        self.asm.join("\n")
    }

    pub fn generate(prog: Program) -> String {
        let mut asm = Assembly { asm: vec![] };
        
        asm.add("SER_OUT = $6000\n");
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
            Exp::Constant(constant) => self.add_inden(format!("lda #{}", 
                Assembly::gen_const(constant)
            )),
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