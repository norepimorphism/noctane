impl Default for Memory {
    fn default() -> Self {
        Self {
            ram: box [0; 0x80000],
            scratchpad: box [0; 0xff],
            bios: box [0; 0x100000],
        }
    }
}

pub struct Memory {
    ram: Box<[u32; 0x80000]>,
    scratchpad: Box<[u32; 0xff]>,
    bios: Box<[u32; 0x100000]>,
}

macro_rules! def_read_write {
    (
        $read_8_name:ident
        $read_16_name:ident
        $read_32_name:ident
        $write_8_name:ident
        $write_16_name:ident
        $write_32_name:ident
    ) => {
        /// Reads the 8-bit value at the given physical address.
        pub fn $read_8_name(&mut self, addr: u32) -> u8 {
            let idx = (addr & 0b11) as usize;

            self.$read_32_name(addr).to_be_bytes()[idx]
        }

        /// Reads the 16-bit value at the given physical address.
        ///
        /// This function will silently ignore the lowest bit if `addr` is unaligned.
        pub fn $read_16_name(&mut self, addr: u32) -> u16 {
            let idx = (addr & 0b10) as usize;
            let chunk = self.$read_32_name(addr).to_be_bytes().as_chunks::<2>().0[idx];

            u16::from_be_bytes(chunk)
        }

        /// Writes an 8-bit value to the given physical address.
        pub fn $write_8_name(&mut self, addr: u32, value: u8) {
            todo!()
        }

        /// Writes a 16-bit value to the given physical address.
        ///
        /// This function will silently ignore the lowest bit if `addr` is unaligned.
        pub fn $write_16_name(&mut self, addr: u32, value: u16) {
            todo!()
        }
    };
}

impl Memory {
    def_read_write! {
        read_kuseg_8
        read_kuseg_16
        read_kuseg_32
        write_kuseg_8
        write_kuseg_16
        write_kuseg_32
    }

    def_read_write! {
        read_kseg0_8
        read_kseg0_16
        read_kseg0_32
        write_kseg0_8
        write_kseg0_16
        write_kseg0_32
    }

    def_read_write! {
        read_kseg1_8
        read_kseg1_16
        read_kseg1_32
        write_kseg1_8
        write_kseg1_16
        write_kseg1_32
    }

    /// Reads the 32-bit value at the given physical address.
    ///
    /// This function will silently ignore the lowest two bits if `addr` is unaligned.
    pub fn read_kuseg_32(&mut self, addr: u32) -> u32 {
        // TODO
        self.ram[(addr / 4) as usize]
    }

    /// Writes a 32-bit value to the given physical address.
    ///
    /// This function will silently ignore the lowest two bits if `addr` is unaligned.
    pub fn write_kuseg_32(&mut self, addr: u32, value: u32) {
        todo!()
    }

    pub fn read_kseg0_32(&mut self, addr: u32) -> u32 {
        todo!()
    }

    pub fn write_kseg0_32(&mut self, addr: u32, value: u32) {
        todo!()
    }

    pub fn read_kseg1_32(&mut self, addr: u32) -> u32 {
        todo!()
    }

    pub fn write_kseg1_32(&mut self, addr: u32, value: u32) {
        todo!()
    }
}
