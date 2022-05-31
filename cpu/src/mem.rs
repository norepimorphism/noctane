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
            access_kseg0: impl FnOnce(&mut Self, Address) -> T,
            access_kseg1: impl FnOnce(&mut Self, Address) -> T,
            access_kseg2: impl FnOnce(&mut Self, Address) -> T,
        ) -> T {
            /// Strips irrelevant bits from the bottom of the given address according to the bit
            /// width of the value it accesses.
            macro_rules! strip_addr {
                (8 $addr:expr) => { $addr };
                (16 $addr:expr) => { $addr & !0b1 };
                (32 $addr:expr) => { $addr & !0b11 };
            }

            #[cfg(not(any(target_pointer_width = "32", target_pointer_width = "64")))]
            compile_error!("The pointer width of the target system is too small. At least a 32-bit pointer width is necessary to index certain arrays.");

            // Now that we know we're running on at least a 32-bit system, we can cast to `usize`.
            // This is possible as well as important because certain memory banks have a size
            // greater than a `u8` or `u16` can possibly index, so it's best to issue a compile
            // error now such that, from now on, we can assume `usize` addresses are capable of
            // indexing all accessible memory.
            let addr = addr as usize;

            // To absolutely ensure that page boundaries cannot be crossed, we will strip the bottom
            // bits from the address as necessary.
            // TODO: Raise an exception if a page boundary is crossed.
            let addr = strip_addr!($width addr);

            let make_address = |shift_amt: usize| Address::new(addr, addr - (shift_amt << 29));

            match addr >> 29 {
                // 0x0000_0000
                0 => access_kseg0(self, make_address(0)),
                1 => access_kseg0(self, make_address(0)),
                2 => access_kseg0(self, make_address(0)),
                3 => access_kseg0(self, make_address(0)),
                // 0x8000_0000
                4 => access_kseg0(self, make_address(4)),
                // 0xa000_0000
                5 => access_kseg1(self, make_address(5)),
                // 0xc000_0000
                6 => access_kseg2(self, make_address(6)),
                7 => access_kseg2(self, make_address(6)),
                _ => unreachable!(),
            }
        }
    };
}

impl From<usize> for Address {
    fn from(init: usize) -> Self {
       Self::new(init, init)
    }
}

impl Address {
    pub const fn new(init: usize, working: usize) -> Self {
       Self {
           init,
           working,
           halfword_idx: (init >> 1) & 0b1,
           byte_idx: init & 0b11,
       }
    }
}

/// A physical memory address.
#[derive(Clone, Copy)]
pub struct Address {
    pub init: usize,
    pub working: usize,
    pub halfword_idx: usize,
    pub byte_idx: usize,
}

impl Address {
    pub fn map_working(mut self, mut f: impl FnMut(usize) -> usize) -> Self {
        self.working = f(self.working);

        self
    }

    pub fn index_byte_in_word(&self, word: u32) -> u8 {
        word.to_be_bytes()[self.byte_idx]
    }

    pub fn index_byte_in_halfword(&self, halfword: u16) -> u8 {
        halfword.to_be_bytes()[self.byte_idx & 0b1]
    }

    pub fn index_halfword_in_word(&self, word: u32) -> u16 {
        u16::from_be_bytes(word.to_be_bytes().as_chunks::<2>().0[self.halfword_idx])
    }
}

impl Memory<'_, '_> {
    def_access!(access_8 8);
    def_access!(access_16 16);
    def_access!(access_32 32);

    pub fn read_8(&mut self, addr: u32) -> Result<u8, Error> {
        let result = self.access_8(
            addr,
            |this, addr| {
                this.cache.i.read_8(
                    addr,
                    |addr| {
                        this.bus.fetch_cache_line(addr).map_err(Error::Bus)
                    },
                )
            },
            |this, addr| {
                this.bus.read_8(addr).map_err(Error::Bus)
            },
            |_, _| {
                // TODO
                Ok(0)
            },
        );

        if let Ok(value) = result {
            tracing::trace!("#[{:#010x}] -> {:#04x}", addr, value);
        }

        result
    }

    pub fn read_16(&mut self, addr: u32) -> Result<u16, Error> {
        let result = self.access_16(
            addr,
            |this, addr| {
                this.cache.i.read_16(
                    addr,
                    |addr| {
                        this.bus.fetch_cache_line(addr).map_err(Error::Bus)
                    },
                )
            },
            |this, addr| {
                this.bus.read_16(addr).map_err(Error::Bus)
            },
            |_, _| {
                // TODO
                Ok(0)
            },
        );

        if let Ok(value) = result {
            tracing::trace!("#[{:#010x}] -> {:#06x}", addr, value);
        }

        result
    }

    pub fn read_32(&mut self, addr: u32) -> Result<u32, Error> {
        let result = self.access_32(
            addr,
            |this, addr| {
                this.cache.i.read_32(
                    addr,
                    |addr| {
                        this.bus.fetch_cache_line(addr).map_err(Error::Bus)
                    },
                )
            },
            |this, addr| {
                this.bus.read_32(addr).map_err(Error::Bus)
            },
            |_, _| {
                // TODO
                Ok(0)
            },
        );

        if let Ok(value) = result {
            tracing::trace!("#[{:#010x}] -> {:#04x}", addr, value);
        }

        result
    }

    pub fn write_8(&mut self, addr: u32, value: u8) -> Result<(), Error> {
        tracing::trace!("#[{:#010x}] <- {:#04x}", addr, value);

        self.access_8(
            addr,
            |this, addr| {
                this.cache.i.write_8(addr, value);

                Ok(())
            },
            |this, addr| {
                this.bus.write_8(addr, value).map_err(Error::Bus)
            },
            |_, _| {
                // TODO
                Ok(())
            },
        )
    }

    pub fn write_16(&mut self, addr: u32, value: u16) -> Result<(), Error> {
        tracing::trace!("#[{:#010x}] <- {:#06x}", addr, value);

        self.access_16(
            addr,
            |this, addr| {
                this.cache.i.write_16(addr, value);

                Ok(())
            },
            |this, addr| {
                this.bus.write_16(addr, value).map_err(Error::Bus)
            },
            |_, _| {
                // TODO
                Ok(())
            },
        )
    }

    pub fn write_32(&mut self, addr: u32, value: u32) -> Result<(), Error> {
        tracing::trace!("#[{:#010x}] <- {:#010x}", addr, value);

        self.access_32(
            addr,
            |this, addr| {
                this.cache.i.write_32(addr, value);

                Ok(())
            },
            |this, addr| {
                this.bus.write_32(addr, value).map_err(Error::Bus)
            },
            |_, _| {
                // TODO
                Ok(())
            },
        )
    }
}
