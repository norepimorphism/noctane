// SPDX-License-Identifier: MPL-2.0

//! Potentially-cacheable CPU memory.
//!
//! This is the stage at which program addresses are translated to physical addresses and stripped
//! of their bottommost bits, and accesses are delegated to either the CPU cache (see
//! [`cache`]) or memory bus (see [`bus`]).
//!
//! [`cache`]: crate::cache
//! [`bus`]: crate::bus

// Because the PSX CPU is little-endian, as described in the module documentation, if the host CPU
// is big-endian, then:
//   - when we write to memory, we must swap the bytes first; and
//   - when we read from memory, we must swap the bytes again.
//
// This is accomplished using Rust's [`u32::to_le`], [`u32::to_le_bytes`], and
// [`u32::from_le_bytes`] methods.
//
// As this notice applies to all memory, it is located here in the [`mem`] module, but shall, by
// extension, apply to [`cache::i`], [`bus`], and [`bus::io`].

use crate::{bus::Bus, Cache};

impl From<usize> for Address {
    fn from(init: usize) -> Self {
        Self::new(init, init)
    }
}

impl Address {
    /// Creates a new [`Address`].
    pub const fn new(init: usize, working: usize) -> Self {
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
    /// The original program address.
    pub init: usize,
    /// The current 'working' address.
    ///
    /// This may be a program address or a physical address. It represents the current state of this
    /// structure. This field is rebased as it is passed through different memory stores.
    pub working: usize,
    /// The index of the halfword within the word represented by [`working`] to be accessed.
    ///
    /// This value may be 0 or 1.
    ///
    /// [`working`]: Self::working
    pub halfword_idx: usize,
    /// The index of the byte within the word represented by [`working`] to be accessed.
    ///
    /// This value may be 0, 1, 2, or 3.
    ///
    /// [`working`]: Self::working
    pub byte_idx: usize,
}

impl Address {
    /// Modifies the [`working`] field with a given map function.
    ///
    /// This is merely a convenience function for the following:
    /// ```ignore
    /// self.working = f(self.working);
    /// ```
    ///
    /// It allows for concise modification of the working address.
    ///
    /// [`working`]: Self::working
    pub(crate) fn map_working(mut self, mut f: impl FnMut(usize) -> usize) -> Self {
        self.working = f(self.working);

        self
    }

    // The following three methods work in little-endian as, even though we are not working directly
    // with memory here, byte and halfword indices require that data is laid out as such.

    /// Indexes into a word, returning the byte that this address points to.
    pub(crate) fn index_byte_in_word(&self, word: u32) -> u8 {
        word.to_le_bytes()[self.byte_idx]
    }

    /// Indexes into a halfword, returning the byte that this address points to.
    pub(crate) fn index_byte_in_halfword(&self, halfword: u16) -> u8 {
        halfword.to_le_bytes()[self.byte_idx & 0b1]
    }

    /// Indexes into a word, returning the halfword that this address points to.
    pub(crate) fn index_halfword_in_word(&self, word: u32) -> u16 {
        u16::from_le_bytes(word.to_le_bytes().as_chunks::<2>().0[self.halfword_idx])
    }
}

impl<'c, 'b> Memory<'c, 'b> {
    /// Creates a new [`Memory`].
    pub fn new(cache: &'c mut Cache, bus: Bus<'b>) -> Self {
        Self { cache, bus }
    }
}

/// Potentially-cacheable CPU memory.
///
/// This kind of memory accepts program addresses and performs accesses to either the cache or the
/// underlying CPU bus.
pub struct Memory<'c, 'b> {
    cache: &'c mut Cache,
    bus: Bus<'b>,
}

impl<'b> Memory<'_, 'b> {
    /// A shared reference to the CPU cache.
    pub fn cache(&self) -> &Cache {
        self.cache
    }

    /// An exclusive reference to the CPU cache.
    pub fn cache_mut(&mut self) -> &mut Cache {
        self.cache
    }

    /// A shared reference to the CPU bus.
    pub fn bus(&self) -> &Bus<'b> {
        &self.bus
    }

    /// An exclusive reference to the CPU bus.
    pub fn bus_mut(&mut self) -> &mut Bus<'b> {
        &mut self.bus
    }
}

/// Defines an `access` method for redirecting a program address into one of the three address
/// regions.
macro_rules! def_access {
    ($fn_name:ident $width:tt) => {
        fn $fn_name<T>(
            &mut self,
            addr: u32,
            access_kseg0: impl FnOnce(&mut Self, Address) -> Result<T, ()>,
            access_kseg1: impl FnOnce(&mut Self, Address) -> Result<T, ()>,
            access_kseg2: impl FnOnce(&mut Self, Address) -> Result<T, ()>,
        ) -> Result<T, ()> {
            #[cfg(not(any(target_pointer_width = "32", target_pointer_width = "64")))]
            compile_error!(indoc::indoc! {"
                The pointer width of the target system is too small. At least a 32-bit pointer width
                is necessary to index certain arrays.
            "});

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
                (16 $addr:expr) => {
                    {
                        if ($addr % 2) > 0 {
                            // This crosses a halfword boundary.
                            return Err(());
                        }

                        $addr & !0b1
                    }
                };
                (32 $addr:expr) => {
                    {
                        if ($addr % 4) > 0 {
                            // This crosses a word boundary.
                            return Err(());
                        }

                        $addr & !0b11
                    }
                };
            }

            // To ensure that (half)word boundaries cannot be crossed, we will strip the bottom bits
            // from the address as necessary.
            let addr = strip_addr!($width addr);

            macro_rules! call_access_fn {
                ($f:expr, $region_id:expr) => {
                    {
                        let make_address = |region_id: usize| {
                            Address::new(
                                addr,
                                // Rebase the address to the start of its address region.
                                addr - (region_id << 29),
                            )
                        };

                        if self.cache.i.is_isolated() {
                            // When the I-cache is isolated, *all* memory accesses are cached, and
                            // there are no write-throughs. KSEG0 is like the other segments except
                            // cached, so we can simply redirect everything to that handler.
                            access_kseg0(self, make_address($region_id))
                        } else {
                            $f(self, make_address($region_id))
                        }
                    }
                };
            }

            // We only need the top three bits of a program address to determine which address
            // region it belongs to.
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
                // Three bits allows for 2^3 different possibilities, so it is impossible for this
                // value to be greater than 7.
                _ => unreachable!(),
            }
        }
    };
}

/// Defines a `read` method for a given bit width.
macro_rules! def_read_fn {
    ($fn_name:ident $access_name:ident $ty:ty) => {
        pub fn $fn_name(&mut self, addr: u32) -> Result<$ty, ()> {
            let value = self.$access_name(
                addr,
                |this, addr| {
                    this.cache
                        .i
                        .$fn_name(addr, |addr| this.bus.fetch_cache_line(addr))
                },
                |this, addr| {
                    this.bus.$fn_name(addr)
                },
                |_, _| {
                    // TODO
                    Ok(0)
                },
            );
            if let Ok(value) = value {
                tracing::trace!("#[{:#010x}] -> {:#08x}", addr, value);
            }

            value
        }
    };
}

/// Defines a `write` method for a given bit width.
macro_rules! def_write_fn {
    ($fn_name:ident $access_name:ident $ty:ty) => {
        pub fn $fn_name(&mut self, addr: u32, value: $ty) -> Result<(), ()> {
            tracing::trace!("#[{:#010x}] <- {:#010x}", addr, value);

            self.$access_name(
                addr,
                |this, addr| {
                    this.cache.i.$fn_name(addr, value);
                    if !this.cache.i.is_isolated() {
                        // When not isolated, write-through to the CPU bus.
                        this.bus.$fn_name(addr, value)?;
                    }

                    Ok(())
                },
                |this, addr| {
                    this.bus.$fn_name(addr, value)
                },
                |_, _| {
                    // TODO
                    Ok(())
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
