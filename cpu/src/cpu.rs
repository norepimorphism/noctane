pub mod reg;

use crate::{Mmu, instr::{self, Instr}};

impl Cpu {
    pub fn new() -> Self {
        Self {
            mmu: Mmu::default(),
            pipe: instr::Pipeline::default(),
            reg: reg::File::default(),
        }
    }
}

pub struct Cpu {
    mmu: Mmu,
    pipe: instr::Pipeline,
    reg: reg::File,
}

impl Cpu {
    pub fn mmu(&self) -> &Mmu {
        &self.mmu
    }

    pub fn mmu_mut(&mut self) -> &mut Mmu {
        &mut self.mmu
    }

    /// The register file.
    pub fn reg(&self) -> &reg::File {
        &self.reg
    }

    pub fn reg_mut(&mut self) -> &mut reg::File {
        &mut self.reg
    }

    /// The instruction pipeline.
    pub fn pipeline(&self) -> &instr::Pipeline {
        &self.pipe
    }

    pub fn pipeline_mut(&mut self) -> &mut instr::Pipeline {
        &mut self.pipe
    }

    /// Processes the current stage, and then advances to the next stage, of each queued
    /// instruction.
    pub fn advance_pipeline(&mut self, fetch_instr: impl FnOnce() -> Instr) {
        // tracing::debug!("{}", self.pipe);
        self.pipe.advance(&mut self.mmu, &mut self.reg, fetch_instr);
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ExceptionKind {
    Syscall = 8,
    Breakpoint = 9,
    ReservedInstr = 10,
    UnusableCop = 11,
    IntegerOverflow = 12,
}
