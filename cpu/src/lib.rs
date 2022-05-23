// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

#![feature(concat_idents)]

pub mod cpu;
pub mod instr;

pub use cpu::Cpu;
pub use instr::Instr;

impl Cpu {
    pub fn execute_instr(&mut self, instr: Instr) {
        self.pipeline_mut().advance(self, || instr)
    }

    pub fn execute_next_instr(&mut self) {
        let op = self.fetch_opcode();
        self.execute_opcode(op);
    }

    fn fetch_opcode(&mut self) -> u32 {
        let pc = self.reg().pc();
        let op = self.read_mem(pc);

        // Increment PC.
        self.reg_mut().set_pc(pc.wrapping_add(4));

        op
    }

    pub fn execute_opcode(&mut self, op: u32) {
        self.pipeline_mut().advance(
            self,
            || Instr::decode(op).unwrap_or_else(|| {
                // Oops! Instruction decoding failed!
                //
                // A Reserved Instruction (RI) exception is made, and the first instruction in the
                // exception handler is fetched.
                todo!()
            },
        ));
    }
}
