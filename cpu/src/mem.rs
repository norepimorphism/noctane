// SPDX-License-Identifier: MPL-2.0

//! Potentially-cacheable CPU memory.
//!
//! This is the stage at which program addresses are translated to physical addresses and stripped
//! of their bottommost bits, and accesses are delegated to either the CPU cache (see
//! [`cache`]) or memory bus (see [`bus`]).
//!
//! [`cache`]: crate::cache
//! [`bus`]: crate::bus

use crate::{bus::Bus, Cache};

impl From<usize> for Address {
    fn from(init: usize) -> Self {
        Self::new(init, init)
    }
}

impl Address {
    /// Creates a new `Address`.
    pub(crate) const fn new(init: usize, working: usize) -> Self {
        Self {
            init,
            working,
            halfword_idx: (init >> 1) & 0b1,
            byte_idx: init & 0b11,
        }
    }
}

/// A 32-bit program (i.e. virtual) or physical address.
#[derive(Clone, Copy)]
pub struct Address {
    pub(crate) init: usize,
    pub(crate) working: usize,
    pub(crate) halfword_idx: usize,
    pub(crate) byte_idx: usize,
}

impl Address {
    pub(crate) fn map_working(mut self, mut f: impl FnMut(usize) -> usize) -> Self {
        self.working = f(self.working);

        self
    }

    pub(crate) fn index_byte_in_word(&self, word: u32) -> u8 {
        word.to_be_bytes()[self.byte_idx]
    }

    pub(crate) fn index_byte_in_halfword(&self, halfword: u16) -> u8 {
        halfword.to_be_bytes()[self.byte_idx & 0b1]
    }

    pub(crate) fn index_halfword_in_word(&self, word: u32) -> u16 {
        u16::from_be_bytes(word.to_be_bytes().as_chunks::<2>().0[self.halfword_idx])
    }
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

impl<'b> Memory<'_, 'b> {
    pub fn cache(&self) -> &Cache {
        self.cache
    }

    pub fn cache_mut(&mut self) -> &mut Cache {
        self.cache
    }

    pub fn bus(&self) -> &Bus<'b> {
        &self.bus
    }

    pub fn bus_mut(&mut self) -> &mut Bus<'b> {
        &mut self.bus
    }
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
            #[cfg(not(any(target_pointer_width = "32", target_pointer_width = "64")))]
            compile_error!("The pointer width of the target system is too small. At least a 32-bit pointer width is necessary to index certain arrays.");

            // Now that we know we're running on at least a 32-bit system, we can cast to `usize`.
            // This is possible as well as important because certain memory banks have a size
            // greater than a `u8` or `u16` can possibly index, so it's best to issue a compile
            // error now such that, from now on, we can assume `usize` addresses are capable of
            // indexing all accessible memory.
            let addr = addr as usize;

            /// Strips irrelevant bits from the bottom of the given address according to the bit
            /// width of the value it accesses.
            macro_rules! strip_addr {
                (8 $addr:expr) => { $addr };
                (16 $addr:expr) => { $addr & !0b1 };
                (32 $addr:expr) => { $addr & !0b11 };
            }

            // To absolutely ensure that page boundaries cannot be crossed, we will strip the bottom
            // bits from the address as necessary.
            // TODO: Raise an exception if a page boundary is crossed.
            let addr = strip_addr!($width addr);

            macro_rules! call_access_fn {
                ($f:expr, $shift_amt:expr) => {
                    {
                        let make_address = |shift_amt: usize| {
                            Address::new(addr, addr - (shift_amt << 29))
                        };

                        if self.cache.i.is_isolated() {
                            // When the I-cache is isolated, *all* memory accesses are cached, and
                            // there are no write-throughs. KSEG0 is like the other segments except
                            // cached, so we can simply redirect everything to that handler.
                            access_kseg0(self, make_address($shift_amt))
                        } else {
                            $f(self, make_address($shift_amt))
                        }
                    }
                };
            }

            match addr >> 29 {
                // 0x0000_0000
                0 => call_access_fn!(access_kseg0, 0),
                1 => call_access_fn!(access_kseg0, 0),
                2 => call_access_fn!(access_kseg0, 0),
                3 => call_access_fn!(access_kseg0, 0),
                // 0x8000_0000
                4 => call_access_fn!(access_kseg0, 4),
                // 0xa000_0000
                5 => call_access_fn!(access_kseg1, 5),
                // 0xc000_0000
                6 => call_access_fn!(access_kseg2, 6),
                7 => call_access_fn!(access_kseg2, 6),
                _ => unreachable!(),
            }
        }
    };
}

macro_rules! def_read_fn {
    ($fn_name:ident $access_name:ident $ty:ty) => {
        pub fn $fn_name(&mut self, addr: u32) -> $ty {
            let value = self.$access_name(
                addr,
                |this, addr| {
                    this.cache
                        .i
                        .$fn_name(addr, |addr| this.bus.fetch_cache_line(addr))
                },
                |this, addr| this.bus.$fn_name(addr),
                |_, _| {
                    // TODO
                    0
                },
            );
            tracing::trace!("#[{:#010x}] -> {:#08x}", addr, value);

            value
        }
    };
}

macro_rules! def_write_fn {
    ($fn_name:ident $access_name:ident $ty:ty) => {
        pub fn $fn_name(&mut self, addr: u32, value: $ty) {
            tracing::trace!("#[{:#010x}] <- {:#010x}", addr, value);

            self.$access_name(
                addr,
                |this, addr| {
                    this.cache.i.$fn_name(addr, value);
                    if !this.cache.i.is_isolated() {
                        // When not isolated, write-through to the CPU bus.
                        this.bus.$fn_name(addr, value);
                    }
                },
                |this, addr| this.bus.$fn_name(addr, value),
                |_, _| {
                    // TODO
                },
            )
        }
    };
}

impl Memory<'_, '_> {
    def_access!(access_8 8);

    def_access!(access_16 16);

    def_access!(access_32 32);

    def_read_fn!(read_8 access_8 u8);

    def_read_fn!(read_16 access_16 u16);

    def_read_fn!(read_32 access_32 u32);

    def_write_fn!(write_8 access_8 u8);

    def_write_fn!(write_16 access_16 u16);

    def_write_fn!(write_32 access_32 u32);
}
