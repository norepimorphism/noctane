// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

#![feature(
    bigint_helper_methods,
    box_syntax,
    exclusive_range_pattern,
    slice_as_chunks,
    unchecked_math,
)]

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

impl Default for State {
    fn default() -> Self {
        Self {
            cache: Cache::default(),
            exc: exc::Queue::new(),
            pipeline: instr::Pipeline::default(),
            reg: reg::File::default(),
        }
    }
}

pub struct State {
    pub cache: Cache,
    /// The exception queue.
    pub exc: exc::Queue,
    /// The instruction pipeline.
    pub pipeline: instr::Pipeline,
    /// The register file.
    pub reg: reg::File,
}

impl State {
    pub fn connect_bus<'s, 'b>(&'s mut self, bus: crate::Bus<'b>) -> Cpu<'s, 'b> {
        Cpu {
            exc: &mut self.exc,
            pipeline: &mut self.pipeline,
            reg: &mut self.reg,
            mmu: Mmu::new(Memory::new(&mut self.cache, bus)),
        }
    }
}

pub struct Cpu<'s, 'b> {
    exc: &'s mut exc::Queue,
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

    pub fn execute_next_instr(&mut self) {
        self.advance_pipeline(|op| Instr::decode(op).unwrap());
    }

    pub fn execute_instr(&mut self, instr: Instr) {
        self.advance_pipeline(|_| instr)
    }

    pub fn execute_opcode(&mut self, op: u32) {
        self.advance_pipeline(|_| Instr::decode(op).unwrap());
    }

    /// Processes the current stage, and then advances to the next stage, of each queued
    /// instruction.
    pub fn advance_pipeline(&mut self, decode_instr: impl Fn(u32) -> Instr) {
        self.handle_exc();
        self.pipeline.advance(&mut self.exc, &mut self.mmu, &mut self.reg, &decode_instr);
    }

    fn handle_exc(&mut self) {
        if let Ok(e) = self.exc.pop() {
            tracing::error!("Exception: {:?}", e);
            *self.reg.pc_mut() = e.vector();
        }
    }
}
