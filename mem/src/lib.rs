// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

#![feature(slice_as_chunks)]

impl Memory {
    pub fn new() -> Self {
        Self::default()
    }
}

impl Default for Memory {
    fn default() -> Self {
        Self { buf: [0; 8192] }
    }
}

pub struct Memory {
    buf: [u8; 8192],
}

impl Memory {
    /// Reads the 32-bit value at the given physical address.
    ///
    /// This function will silently ignore the lowest two bits if `addr` is unaligned.
    pub fn read_32(&mut self, addr: u32) -> u32 {
        u32::from_le_bytes(self.buf.as_chunks::<4>().0[(addr / 4) as usize])
    }

    /// Reads the 16-bit value at the given physical address.
    ///
    /// This function will silently ignore the lowest bit if `addr` is unaligned.
    pub fn read_16(&mut self, addr: u32) -> u16 {
        u16::from_be_bytes(self.buf.as_chunks::<2>().0[(addr / 2) as usize])
    }

    /// Reads the 8-bit value at the given physical address.
    pub fn read_8(&mut self, addr: u32) -> u8 {
        self.buf[addr as usize]
    }

    /// Writes a 32-bit value to the given physical address.
    ///
    /// This function will silently ignore the lowest two bits if `addr` is unaligned.
    pub fn write_32(&mut self, addr: u32, value: u32) {
        self.buf.as_chunks_mut::<4>().0[(addr / 4) as usize] = value.to_le_bytes();
    }

    /// Writes a 16-bit value to the given physical address.
    ///
    /// This function will silently ignore the lowest bit if `addr` is unaligned.
    pub fn write_16(&mut self, addr: u32, value: u16) {
        self.buf.as_chunks_mut::<2>().0[(addr / 2) as usize] = value.to_le_bytes();
    }

    /// Writes an 8-bit value to the given physical address.
    pub fn write_8(&mut self, addr: u32, value: u8) {
        self.buf[addr as usize] = value;
    }
}
