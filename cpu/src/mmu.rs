// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use std::ops::{Deref, DerefMut};

use crate::{InstrCache, Memory, i_cache};

impl Default for Mmu {
    fn default() -> Self {
        Self {
            i_cache: InstrCache::default(),
            mem: Memory::default(),
        }
    }
}

pub struct Mmu {
    i_cache: InstrCache,
    mem: Memory,
}

macro_rules! def_read_instr {
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
                || self.mem.$mem_fn_name(addr),
            )
        }
    };
}

macro_rules! def_write_instr {
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
                || self.mem.$mem_fn_name(addr, value),
            );
        }
    };
}

impl Mmu {
    def_read_instr! { fn read_virt_instr + read_phys_instr () -> u32 = read + read_32 }
    def_write_instr! { fn write_virt_instr + write_phys_instr (u32) = write + write_32 }

    /// Translates the given virtual address to a physical address.
    pub fn translate_vaddr(&self, vaddr: u32) -> u32 {
        // TODO: Page tables aren't implemented yet, so...
        vaddr
    }
}

impl Deref for Mmu {
    type Target = Memory;

    fn deref(&self) -> &Self::Target {
        &self.mem
    }
}

impl DerefMut for Mmu {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.mem
    }
}
