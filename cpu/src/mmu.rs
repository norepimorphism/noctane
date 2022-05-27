// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use std::ops::{Deref, DerefMut};

use crate::mem::{self, Memory};

#[derive(Debug)]
pub enum Error {
    Memory(mem::Error),
}

impl<'c, 'b> Mmu<'c, 'b> {
    pub fn new(mem: Memory<'c, 'b>) -> Self {
        Self(mem)
    }
}

pub struct Mmu<'c, 'b>(Memory<'c, 'b>);

macro_rules! def_read {
    (
        fn $fn_name:ident() -> $fn_out_ty:ty = $mem_fn_name:ident
    ) => {
        /// Reads the value at the given virtual address.
        pub fn $fn_name(&mut self, vaddr: u32) -> Result<$fn_out_ty, Error> {
            let addr = self.translate_vaddr(vaddr);

            self.$mem_fn_name(addr).map_err(Error::Memory)
        }
    };
}

macro_rules! def_write {
    (
        fn $fn_name:ident($fn_in_ty:ty) = $mem_fn_name:ident
    ) => {
        /// Writes a value to the given virtual address.
        pub fn $fn_name(&mut self, vaddr: u32, value: $fn_in_ty) -> Result<(), Error> {
            let addr = self.translate_vaddr(vaddr);
            self.$mem_fn_name(addr, value).map_err(Error::Memory)?;

            Ok(())
        }
    };
}

impl Mmu<'_, '_> {
    def_read! { fn read_virt_8() -> u8 = read_8 }
    def_read! { fn read_virt_16() -> u16 = read_16 }
    def_read! { fn read_virt_32() -> u32 = read_32 }
    def_write! { fn write_virt_8(u8) = write_8 }
    def_write! { fn write_virt_16(u16) = write_16 }
    def_write! { fn write_virt_32(u32) = write_32 }

    /// Translates the given virtual address to a physical address.
    pub fn translate_vaddr(&self, vaddr: u32) -> u32 {
        // TODO: Page tables aren't implemented yet, so...
        vaddr
    }
}

impl<'c, 'b> Deref for Mmu<'c, 'b> {
    type Target = Memory<'c, 'b>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Mmu<'_, '_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
