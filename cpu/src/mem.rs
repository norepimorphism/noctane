//! Cached CPU memory.
//!
//! This is the stage at which 16-bit addresses are stripped of their bottom bit, 32-bit addresses
//! are stripped of their bottom two bits, and accesses are delegated to either the CPU cache (see
//! [`crate::cache`]) or memory bus (see [`bus`]).

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

macro_rules! def_access {
    ($fn_name:ident $width:tt) => {
        fn $fn_name<T>(
            &mut self,
            addr: u32,
            access_kseg0: impl FnOnce(&mut Self, usize) -> T,
            access_kseg1: impl FnOnce(&mut Self, usize) -> T,
            access_kseg2: impl FnOnce(&mut Self, usize) -> T,
        ) -> T {
            #[cfg(not(any(target_pointer_width = "32", target_pointer_width = "64")))]
            compile_error!("The pointer width of the target system is too small. At least a 32-bit pointer width is necessary to index certain arrays.");

            // Now that we know we're running on at least a 32-bit system, we can cast to `usize`.
            // This is possible as well as important because certain memory banks have a size
            // greater than a `u8` or `u16` can possibly index, so it's best to issue a compile
            // error now such that, from now on, we can assume `usize` addresses are capable of
            // indexing all accessible memory.
            let addr = addr as usize;

            let addr = strip_addr!($width addr);

            let make_offset = |shift_amt: usize| {
                addr - (shift_amt << 29)
            };

            match addr >> 29 {
                // 0x0000_0000
                0 => access_kseg0(self, make_offset(0)),
                1 => access_kseg0(self, make_offset(0)),
                2 => access_kseg0(self, make_offset(0)),
                3 => access_kseg0(self, make_offset(0)),
                // 0x8000_0000
                4 => access_kseg0(self, make_offset(4)),
                // 0xa000_0000
                5 => access_kseg1(self, make_offset(5)),
                // 0xc000_0000
                6 => access_kseg2(self, make_offset(6)),
                7 => access_kseg2(self, make_offset(6)),
                _ => unreachable!(),
            }
        }
    };
}

macro_rules! strip_addr {
    (8 $addr:expr) => { $addr };
    (16 $addr:expr) => { $addr & !0b1 };
    (32 $addr:expr) => { $addr & !0b11 };
}

impl Memory<'_, '_> {
    def_access!(access_8 8);
    def_access!(access_16 16);
    def_access!(access_32 32);

    pub fn read_8(&mut self, addr: u32) -> Result<u8, Error> {
        self.access_8(
            addr,
            |this, offset| {
                this.cache.i.read_32(
                    addr,
                    || this.bus.read_32(offset).map_err(Error::Bus),
                )
                .map(|it| it.to_be_bytes()[offset & 0b11])
            },
            |this, offset| {
                this.bus.read_8(offset).map_err(Error::Bus)
            },
            |_, _| {
                // TODO
                Ok(0)
            },
        )
    }

    pub fn read_16(&mut self, addr: u32) -> Result<u16, Error> {
        let result = self.access_16(
            addr,
            |this, offset| {
                this.cache.i.read_32(
                    addr,
                    || this.bus.read_32(offset).map_err(Error::Bus),
                )
                .map(|it| {
                    u16::from_be_bytes(it.to_be_bytes().as_chunks::<2>().0[(offset >> 1) & 0b1])
                })
            },
            |this, offset| {
                this.bus.read_16(offset).map_err(Error::Bus)
            },
            |_, _| {
                // TODO
                Ok(0)
            },
        );

        result
    }

    pub fn read_32(&mut self, addr: u32) -> Result<u32, Error> {
        let result = self.access_32(
            addr,
            |this, offset| {
                this.cache.i.read_32(
                    addr,
                    || this.bus.read_32(offset).map_err(Error::Bus),
                )
            },
            |this, offset| {
                this.bus.read_32(offset).map_err(Error::Bus)
            },
            |_, _| {
                // TODO
                Ok(0)
            },
        );

        result
    }

    pub fn write_8(&mut self, addr: u32, value: u8) -> Result<(), Error> {
        self.access_8(
            addr,
            |this, offset| {
                todo!()
            },
            |this, offset| {
                this.bus.write_8(offset, value).map_err(Error::Bus)
            },
            |_, _| {
                // TODO
                Ok(())
            },
        )
    }

    pub fn write_16(&mut self, addr: u32, value: u16) -> Result<(), Error> {
        self.access_16(
            addr,
            |this, offset| {
                todo!()
            },
            |this, offset| {
                this.bus.write_16(offset, value).map_err(Error::Bus)
            },
            |_, _| {
                // TODO
                Ok(())
            },
        )
    }

    pub fn write_32(&mut self, addr: u32, value: u32) -> Result<(), Error> {
        self.access_32(
            addr,
            |this, offset| {
                this.cache.i.write_32(
                    addr,
                    value,
                    || this.bus.write_32(offset, value).map_err(Error::Bus),
                )
            },
            |this, offset| {
                this.bus.write_32(offset, value).map_err(Error::Bus)
            },
            |_, _| {
                // TODO
                Ok(())
            },
        )
    }
}
