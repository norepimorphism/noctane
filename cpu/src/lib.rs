// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

#![feature(box_syntax, exclusive_range_pattern, slice_as_chunks)]

pub mod bus;
pub mod cpu;
pub mod i_cache;
pub mod instr;
pub mod mem;
pub mod mmu;

pub use bus::Bus;
pub use cpu::Cpu;
pub use i_cache::InstrCache;
pub use instr::Instr;
pub use mem::Memory;
pub use mmu::Mmu;

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

        // TODO: Handle exception.
        let op = self.mmu_mut().read_virt_32(pc).unwrap_or(0);

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

                // TODO
                Instr::Sll(instr::r::Instr { rd: 0, rs: 0, rt: 0, shamt: 0, funct: 0 })
            },
        ));
    }
}
