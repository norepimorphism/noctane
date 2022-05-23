// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

#![feature(concat_idents, slice_as_chunks)]

pub mod cpu;
pub mod instr;

pub use cpu::Cpu;
pub use instr::Instr;

impl Cpu {
    pub fn execute_instr(&mut self, instr: Instr) {
        self.advance_pipeline(|| instr)
    }

    pub fn execute_next_instr(&mut self) {
        let op = self.fetch_opcode();
        self.execute_opcode(op);
    }

    pub fn fetch_opcode(&mut self) -> u32 {
        let pc = self.reg().pc();
        let op = self.read_virt_i_mem(pc);

        // Increment PC.
        *self.reg_mut().pc_mut() = pc.wrapping_add(4);

        op
    }

    pub fn execute_opcode(&mut self, op: u32) {
        self.advance_pipeline(
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
