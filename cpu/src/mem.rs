use crate::{Cache, bus::{self, Bus}};

#[derive(Debug)]
pub enum Error {
    Bus(bus::Error),
}

impl<'c, 'b> Memory<'c, 'b> {
    pub fn new(cache: &'c mut Cache, bus: Bus<'b>) -> Self {
        Self { cache, bus }
    }
}

pub struct Memory<'c, 'b> {
    cache: &'c mut Cache,
    bus: Bus<'b>,
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
        pub fn $read_8_name(&mut self, addr: u32) -> Result<u8, Error> {
            let idx = (addr & 0b11) as usize;

            self.$read_32_name(addr).map(|it| it.to_be_bytes()[idx])
        }

        /// Reads the 16-bit value at the given physical address.
        ///
        /// This function will silently ignore the lowest bit if `addr` is unaligned.
        pub fn $read_16_name(&mut self, addr: u32) -> Result<u16, Error> {
            let idx = (addr & 0b10) as usize;
            let chunk = self.$read_32_name(addr)?.to_be_bytes().as_chunks::<2>().0[idx];

            Ok(u16::from_be_bytes(chunk))
        }

        /// Writes an 8-bit value to the given physical address.
        pub fn $write_8_name(&mut self, addr: u32, value: u8) -> Result<(), Error> {
            let mut bytes = self.$read_32_name(addr)?.to_be_bytes();
            let idx = (addr & 0b11) as usize;
            bytes[idx] = value;
            self.$write_32_name(addr, u32::from_be_bytes(bytes))?;

            Ok(())
        }

        /// Writes a 16-bit value to the given physical address.
        ///
        /// This function will silently ignore the lowest bit if `addr` is unaligned.
        pub fn $write_16_name(&mut self, addr: u32, value: u16) -> Result<(), Error> {
            // TODO
            Ok(())
        }
    };
}

impl Memory<'_, '_> {
    def_read_write! {
        read_8
        read_16
        read_32
        write_8
        write_16
        write_32
    }

    fn select_access_region_fn<T>(
        &mut self,
        addr: u32,
        access_kuseg: impl FnOnce(&mut Self, u32) -> T,
        access_kseg0: impl FnOnce(&mut Self, u32) -> T,
        access_kseg1: impl FnOnce(&mut Self, u32) -> T,
        access_kseg2: impl FnOnce(&mut Self, u32) -> T,
    ) -> T {
        const KUSEG_BASE: u32 = 0x0000_0000;
        const KSEG0_BASE: u32 = 0x8000_0000;
        const KSEG1_BASE: u32 = 0xa000_0000;
        const KSEG2_BASE: u32 = 0xc000_0000;

        match addr {
            KUSEG_BASE..KSEG0_BASE => access_kuseg(self, addr - KUSEG_BASE),
            KSEG0_BASE..KSEG1_BASE => access_kseg0(self, addr - KSEG0_BASE),
            KSEG1_BASE..KSEG2_BASE => access_kseg1(self, addr - KSEG1_BASE),
            KSEG2_BASE.. => access_kseg2(self, addr - KSEG2_BASE),
        }
    }

    pub fn read_32(&mut self, addr: u32) -> Result<u32, Error> {
        let result = self.select_access_region_fn(
            addr,
            |this, offset| this.read_kuseg_32(addr, offset),
            |this, offset| this.read_kseg0_32(addr, offset),
            |this, offset| this.read_kseg1_32(offset),
            |this, offset| this.read_kseg2_32(offset),
        );

        // tracing::debug!(
        //     "mem[{:#010x}] -> {}",
        //     addr,
        //     result.as_ref().map(|it| format!("{:#010x}", it)).unwrap_or("!".into()),
        // );

        result
    }

    fn read_kuseg_32(&mut self, addr: u32, offset: u32) -> Result<u32, Error> {
        self.read_kseg0_32(addr, offset)
    }

    fn read_kseg0_32(&mut self, addr: u32, offset: u32) -> Result<u32, Error> {
        self.cache.i.read_32(
            addr,
            || self.bus.read_32(offset).map_err(Error::Bus),
        )
    }

    fn read_kseg1_32(&mut self, offset: u32) -> Result<u32, Error> {
        self.bus.read_32(offset).map_err(Error::Bus)
    }

    fn read_kseg2_32(&mut self, offset: u32) -> Result<u32, Error> {
        // TODO
        Ok(0)
    }

    pub fn write_32(&mut self, addr: u32, value: u32) -> Result<(), Error> {
        self.select_access_region_fn(
            addr,
            |this, offset| this.write_kuseg_32(addr, offset, value),
            |this, offset| this.write_kseg0_32(addr, offset, value),
            |this, offset| this.write_kseg1_32(offset, value),
            |this, offset| this.write_kseg2_32(offset, value),
        )
    }

    fn write_kuseg_32(&mut self, addr: u32, offset: u32, value: u32) -> Result<(), Error> {
        self.write_kseg0_32(addr, offset, value)
    }

    fn write_kseg0_32(&mut self, addr: u32, offset: u32, value: u32) -> Result<(), Error> {
        self.cache.i.write_32(
            addr,
            value,
            || self.bus.write_32(offset, value).map_err(Error::Bus),
        )
    }

    fn write_kseg1_32(&mut self, offset: u32, value: u32) -> Result<(), Error> {
        self.bus.write_32(offset, value).map_err(Error::Bus)
    }

    fn write_kseg2_32(&mut self, offset: u32, value: u32) -> Result<(), Error> {
        // TODO
        Ok(())
    }
}
