use std::{cell::RefCell, rc::Rc};

pub mod i_cache;
pub mod reg;

use noctane_mem::Memory;

pub use i_cache::InstrCache;
use crate::instr::{self, Instr};

impl Cpu {
    pub fn new(
        mem: Rc<RefCell<Memory>>,
    ) -> Self {
        Self {
            i_cache: InstrCache::default(),
            mem,
            pipe: instr::Pipeline::default(),
            reg: reg::File::default(),
        }
    }
}

pub struct Cpu {
    i_cache: InstrCache,
    mem: Rc<RefCell<Memory>>,
    pipe: instr::Pipeline,
    reg: reg::File,
}

macro_rules! def_read_d_mem {
    (
        fn $virt_fn_name:ident + $phys_fn_name:ident () -> $fn_out_ty:ty = $mem_fn_name:ident
    ) => {
        /// Reads the data at the given virtual address.
        pub fn $virt_fn_name(&mut self, vaddr: u32) -> $fn_out_ty {
            let addr = self.translate_vaddr(vaddr);

            self.$phys_fn_name(addr)
        }

        /// Reads the data at the given physical address.
        pub fn $phys_fn_name(&mut self, addr: u32) -> $fn_out_ty {
            // The PSX lacks a D-cache, so we read straight from memory.
            self.mem.borrow_mut().$mem_fn_name(addr)
        }
    };
}

macro_rules! def_write_d_mem {
    (
        fn $virt_fn_name:ident + $phys_fn_name:ident ($fn_in_ty:ty) = $mem_fn_name:ident
    ) => {
        /// Writes data to the given virtual address.
        pub fn $virt_fn_name(&mut self, vaddr: u32, value: $fn_in_ty) {
            let addr = self.translate_vaddr(vaddr);
            self.$phys_fn_name(addr, value);
        }

        /// Writes data to the given physical address.
        pub fn $phys_fn_name(&mut self, addr: u32, value: $fn_in_ty) {
            // The PSX lacks a D-cache, so we write straight to memory.
            self.mem.borrow_mut().$mem_fn_name(addr, value)
        }
    };
}

macro_rules! def_read_i_mem {
    (
        fn $virt_fn_name:ident + $phys_fn_name:ident () -> $fn_out_ty:ty
            = $cache_fn_name:ident + $mem_fn_name:ident
    ) => {
        /// Reads the instruction at the given virtual address.
        pub fn $virt_fn_name(&mut self, vaddr: u32) -> $fn_out_ty {
            let addr = self.translate_vaddr(vaddr);

            self.$phys_fn_name(addr)
        }

        /// Reads the instruction at the given physical address.
        pub fn $phys_fn_name(&mut self, addr: u32) -> $fn_out_ty {
            self.i_cache.$cache_fn_name(
                &i_cache::Address::from_phys(addr),
                || self.mem.borrow_mut().$mem_fn_name(addr),
            )
        }
    };
}

macro_rules! def_write_i_mem {
    (
        fn $virt_fn_name:ident + $phys_fn_name:ident ($fn_in_ty:ty)
            = $cache_fn_name:ident + $mem_fn_name:ident
    ) => {
        /// Writes an instruction to the given virtual address.
        pub fn $virt_fn_name(&mut self, vaddr: u32, value: $fn_in_ty) {
            let addr = self.translate_vaddr(vaddr);
            self.$phys_fn_name(addr, value);
        }

        /// Writes an instruction to the given physical address.
        pub fn $phys_fn_name(&mut self, addr: u32, value: $fn_in_ty) {
            self.i_cache.$cache_fn_name(
                &i_cache::Address::from_phys(addr),
                value,
                || self.mem.borrow_mut().$mem_fn_name(addr, value),
            );
        }
    };
}

impl Cpu {
    def_read_d_mem! { fn read_virt_d_mem_32 + read_phys_d_mem_32 () -> u32 = read_32 }
    def_read_d_mem! { fn read_virt_d_mem_16 + read_phys_d_mem_16 () -> u16 = read_16 }
    def_read_d_mem! { fn read_virt_d_mem_8 + read_phys_d_mem_8 () -> u8 = read_8 }
    def_write_d_mem! { fn write_virt_d_mem_32 + write_phys_d_mem_32 (u32) = write_32 }
    def_write_d_mem! { fn write_virt_d_mem_16 + write_phys_d_mem_16 (u16) = write_16 }
    def_write_d_mem! { fn write_virt_d_mem_8 + write_phys_d_mem_8 (u8) = write_8 }

    def_read_i_mem! { fn read_virt_i_mem + read_phys_i_mem () -> u32 = read + read_32 }
    def_write_i_mem! { fn write_virt_i_mem + write_phys_i_mem (u32) = write + write_32 }


    /// Translates the given virtual address to a physical address.
    pub fn translate_vaddr(&self, vaddr: u32) -> u32 {
        // TODO: Page tables aren't implemented yet, so...
        vaddr
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
        tracing::debug!("{}", self.pipe);
        self.pipe.advance(&mut self.reg, fetch_instr);
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
