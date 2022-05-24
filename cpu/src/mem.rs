impl Default for Memory {
    fn default() -> Self {
        Self {
            buf: box [0; 0x80000],
        }
    }
}

pub struct Memory {
    buf: Box<[u8; 0x80000]>,
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
