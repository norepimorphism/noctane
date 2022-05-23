// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

impl Memory {
    pub fn new() -> Self {
        Self::default()
    }
}

#[derive(Default)]
pub struct Memory {

}

impl Memory {
    /// Reads the 32-bit value at the given physical address.
    ///
    /// This function will silently ignore the lowest two bits if `addr` is unaligned.
    pub fn read_32(&mut self, addr: u32) -> u32 {
        // TODO
        0
    }

    /// Reads the 16-bit value at the given physical address.
    ///
    /// This function will silently ignore the lowest bit if `addr` is unaligned.
    pub fn read_16(&mut self, addr: u32) -> u16 {
        // TODO
        0
    }

    /// Reads the 8-bit value at the given physical address.
    pub fn read_8(&mut self, addr: u32) -> u8 {
        // TODO
        0
    }

    /// Writes a 32-bit value to the given physical address.
    ///
    /// This function will silently ignore the lowest two bits if `addr` is unaligned.
    pub fn write_32(&mut self, addr: u32, value: u32) {
        // TODO
    }

    /// Writes a 16-bit value to the given physical address.
    ///
    /// This function will silently ignore the lowest bit if `addr` is unaligned.
    pub fn write_16(&mut self, addr: u32, value: u16) {
        // TODO
    }

    /// Writes an 8-bit value to the given physical address.
    pub fn write_8(&mut self, addr: u32, value: u8) {
        // TODO
    }
}
