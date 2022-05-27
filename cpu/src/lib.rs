// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

#![feature(box_syntax, exclusive_range_pattern, slice_as_chunks)]

pub mod bus;
pub mod cache;
pub mod exc;
pub mod instr;
pub mod mem;
pub mod mmu;
pub mod reg;

pub use bus::Bus;
pub use cache::Cache;
pub use instr::Instr;
pub use mem::Memory;
pub use mmu::Mmu;

#[derive(Default)]
pub struct State {
    pub cache: Cache,
    /// The instruction pipeline.
    pub pipeline: instr::Pipeline,
    /// The register file.
    pub reg: reg::File,
}

impl State {
    pub fn connect_bus<'s, 'b>(&'s mut self, bus: crate::Bus<'b>) -> Cpu<'s, 'b> {
        Cpu {
            pipeline: &mut self.pipeline,
            reg: &mut self.reg,
            mmu: Mmu::new(Memory::new(&mut self.cache, bus)),
        }
    }
}

pub struct Cpu<'s, 'b> {
    pipeline: &'s mut instr::Pipeline,
    reg: &'s mut reg::File,
    mmu: Mmu<'s, 'b>,
}

impl<'s, 'b> Cpu<'s, 'b> {
    pub fn pipeline(&self) -> &instr::Pipeline {
        &self.pipeline
    }

    pub fn pipeline_mut(&mut self) -> &mut instr::Pipeline {
        &mut self.pipeline
    }

    pub fn reg(&self) -> &reg::File {
        &self.reg
    }

    pub fn reg_mut(&mut self) -> &mut reg::File {
        &mut self.reg
    }

    pub fn mmu(&self) -> &Mmu<'s, 'b> {
        &self.mmu
    }

    pub fn mmu_mut(&mut self) -> &mut Mmu<'s, 'b> {
        &mut self.mmu
    }

    pub fn execute_instr(&mut self, instr: Instr) {
        self.advance_pipeline(|| instr)
    }

    pub fn execute_next_instr(&mut self) {
        let op = self.fetch_opcode();
        self.execute_opcode(op);
    }

    pub fn fetch_opcode(&mut self) -> u32 {
        let pc = self.reg.pc();

        // TODO: Handle exception.
        let op = self.mmu.read_virt_32(pc).unwrap_or(0);

        // Increment PC.
        *self.reg.pc_mut() = pc.wrapping_add(4);

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

    /// Processes the current stage, and then advances to the next stage, of each queued
    /// instruction.
    pub fn advance_pipeline(&mut self, fetch_instr: impl FnOnce() -> Instr) {
        // tracing::debug!("{}", self.pipe);
        self.pipeline.advance(&mut self.mmu, &mut self.reg, fetch_instr);
    }
}
