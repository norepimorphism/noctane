// SPDX-License-Identifier: MPL-2.0

//! The CPU memory bus.
//!
//! This is the stage at which memory accesses are delegated to the appropriate memory bank.
//! Possible banks include:
//! - [main RAM](Ram)
//! - [I/O region](Io)
//! - Expansion Regions [1](Exp1)&ndash;[3](Exp3) (2 is located within the I/O region)
//! - [BIOS](Bios)

pub mod io;

pub use io::Io;

use crate::mem::Address;

macro_rules! def_bank {
    ($name:ident, $size:literal $(@ $addr:literal)? $(,)?) => {
        impl Default for $name {
            fn default() -> Self {
                // Use 'screaming `0xaa`'s so that it's more obvious when we access uninitialized
                // memory.
                Self(box [0xaa; Self::LEN])
            }
        }

        impl $name {
            $(
                /// The physical address of the start of this memory bank.
                pub const BASE_ADDR: u32 = $addr;
            )?

            /// The size, in bytes, of this memory bank.
            pub const SIZE: usize = $size;
            /// The length, in words, of this memory bank.
            ///
            /// This length determines the highest word within this bank that may be accessed by
            /// hardware. However, [certain I/O registers] may further restrict the range accessible
            /// to software. While it is possible to configure these registers in a manner that they
            /// permit access beyond the length of this bank, an exception will be raised on any
            /// such accesses.
            ///
            /// [certain I/O registers]: io::BusControl
            const LEN: usize = make_index($size);
        }

        /// A bank of memory.
        ///
        /// Internally, the data contained within this type is boxed so as to avoid otherwise
        /// certain stack overflows from occuring as a result of large allocations.
        pub struct $name(Box<[u32; Self::LEN]>);

        impl std::ops::Deref for $name {
            type Target = [u32; Self::LEN];

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }

        impl std::ops::DerefMut for $name {
            fn deref_mut(&mut self) -> &mut Self::Target {
                &mut self.0
            }
        }
    };
}

const fn make_index(addr: usize) -> usize {
    addr / std::mem::size_of::<u32>()
}

// Define the memory banks (except for the I/O region, which is not a simple buffer like the rest
// are).
def_bank!(Ram,  0x20_0000 @ 0x0000_0000);
def_bank!(Exp1, 0x80_0000);
// `Exp2` is missing here as it is located within the I/O region.
def_bank!(Exp3, 0x20_0000 @ 0x1fa0_0000);
def_bank!(Bios, 0x08_0000 @ 0x1fc0_0000);

/// The CPU memory bus.
pub struct Bus<'a> {
    /// [`Ram`].
    pub ram: &'a mut Ram,
    /// [`Exp1`].
    pub exp_1: &'a mut Exp1,
    /// Input and output (I/O).
    ///
    /// This field also contains Expansion Region 2.
    pub io: Io<'a>,
    /// [`Exp3`].
    pub exp_3: &'a mut Exp3,
    /// [`Bios`].
    pub bios: &'a mut Bios,
}

impl Bus<'_> {
    /// The physical address of the start of the I/O region.
    const IO_BASE_ADDR: u32 = 0x1f80_1000;

    /// Selects the appropriate memory bank for a given physical address and passes it to one of two
    /// functions depending on how the data should be accessed.
    ///
    /// `access_word` is called when a normal, buffer-backed memory bank is accessed, such as the
    /// BIOS ROM. A slice into the entire buffer, offset by a value determined from the address, is
    /// passed to `access_word`. Implementations of `access_word` are expected to extract the first
    /// few relevant bytes and form them into a single value.
    ///
    /// `access_io` is called when the given address points into the I/O region. An address relative
    /// to the start of the I/O region is passed to `access_io`. Implementations of `access_io` are
    /// expected to simply delegate to the appropriate I/O read/write function.
    fn access<T: Default + HighZ>(
        &mut self,
        addr: Address,
        access_word: impl FnOnce(&mut u32) -> T,
        access_io: impl FnOnce(&mut Self, Address) -> Result<T, ()>,
    ) -> Result<T, ()> {
        macro_rules! try_access {
            ($start:expr, $size:expr, $f:expr $(,)?) => {
                if (addr.working >= ($start as usize)) {
                    let rebased_addr = addr.working - ($start as usize);
                    if rebased_addr < ($size as usize) {
                        return $f(rebased_addr);
                    }
                }
            };
        }

        macro_rules! try_access_bank {
            ($field_name:ident, $start:expr, $size:expr $(,)?) => {
                try_access!(
                    $start,
                    $size,
                    |rebased_addr: usize| {
                        tracing::trace!(
                            "Accessing `cpu_bus.{}[{:#010x}]`",
                            stringify!($field_name),
                            rebased_addr,
                        );

                        self.$field_name
                            // Be sure not to use normal `[]` indexing here! Accesses may be
                            // out-of-bounds.
                            .get_mut(make_index(rebased_addr))
                            .map(|it| access_word(it))
                            .ok_or(())
                    }
                )
            };
        }

        const MB: u32 = 1024 * 1024;

        let ram_layout = io::ram_ctrl::Layout::from(self.io.ram_ctrl.layout());
        let ram_data_size = MB * ram_layout.data_mb;
        let ram_high_z_size = MB * ram_layout.high_z_mb;

        try_access_bank!(
            ram,
            Ram::BASE_ADDR,
            ram_data_size,
        );
        try_access!(
            Ram::BASE_ADDR.wrapping_add(ram_data_size),
            ram_high_z_size,
            |_| {
                Ok(<T as HighZ>::HIGH_Z)
            },
        );
        try_access_bank!(
            bios,
            Bios::BASE_ADDR,
            // TODO: This is incorrect.
            self.io.bus_ctrl.bios_size,
        );
        try_access_bank!(
            exp_1,
            self.io.bus_ctrl.exp_1_base,
            // TODO: This is incorrect.
            self.io.bus_ctrl.exp_1_size,
        );
        try_access_bank!(
            exp_3,
            Exp3::BASE_ADDR,
            // TODO: This is incorrect.
            self.io.bus_ctrl.exp_3_size,
        );
        try_access!(
            Self::IO_BASE_ADDR,
            0x1000,
            |rebased_addr: usize| {
                access_io(self, addr.map_working(|_| rebased_addr))
            },
        );
        try_access!(
            self.io.bus_ctrl.exp_2_base,
            self.io.bus_ctrl.exp_2_size,
            |rebased_addr: usize| {
                access_io(self, addr.map_working(|_| rebased_addr.wrapping_add(0x1000)))
            },
        );

        Err(())
    }
}

trait HighZ {
    const HIGH_Z: Self;
}

macro_rules! impl_high_z_for_num {
    ($ty:ty) => {
        impl HighZ for $ty {
            const HIGH_Z: Self = !0;
        }
    };
}

impl_high_z_for_num!(u8);
impl_high_z_for_num!(u16);
impl_high_z_for_num!(u32);

impl HighZ for () {
    const HIGH_Z: Self = ();
}

impl Bus<'_> {
    // In each of the three following cases for reads, as the data contained within `word` is
    // little-endian, we should use [`u32::to_le`] to convert to native-endian first.

    pub fn read_8(&mut self, addr: Address) -> Result<u8, ()> {
        self.access(
            addr,
            |word| {
                addr.index_byte_in_word(word.to_le())
            },
            |this, addr| {
                this.io.read_8(addr)
            },
        )
    }

    pub fn read_16(&mut self, addr: Address) -> Result<u16, ()> {
        self.access(
            addr,
            |word| {
                addr.index_halfword_in_word(word.to_le())
            },
            |this, addr| {
                this.io.read_16(addr)
            },
        )
    }

    pub fn read_32(&mut self, addr: Address) -> Result<u32, ()> {
        self.access(
            addr,
            |word| {
                word.to_le()
            },
            |this, addr| {
                this.io.read_32(addr)
            },
        )
    }

    pub fn write_8(&mut self, addr: Address, value: u8) -> Result<(), ()> {
        self.access(
            addr,
            |word| {
                // Be sure to use little-endian here!
                let mut bytes = word.to_le_bytes();
                bytes[addr.byte_idx] = value;
                *word = u32::from_le_bytes(bytes);
            },
            |this, addr| {
                this.io.write_8(addr, value)
            },
        )
    }

    pub fn write_16(&mut self, addr: Address, value: u16) -> Result<(), ()> {
        self.access(
            addr,
            |word| {
                // Be sure to use little-endian here!
                let mut bytes = word.to_le_bytes();
                bytes.as_chunks_mut::<2>().0[addr.halfword_idx] = value.to_le_bytes();
                *word = u32::from_le_bytes(bytes);
            },
            |this, addr| {
                this.io.write_16(addr, value)
            },
        )
    }

    pub fn write_32(&mut self, addr: Address, value: u32) -> Result<(), ()> {
        self.access(
            addr,
            |word| {
                // `word` is little-endian but `value` may not be, so must ensure they are of the
                // same endianness.
                *word = value.to_le();
            },
            |this, addr| {
                this.io.write_32(addr, value)
            },
        )
    }

    pub fn fetch_cache_line(&mut self, addr: Address) -> Result<[u32; 4], ()> {
        Ok([
            self.read_32(addr.map_working(|it| it.wrapping_add(0)))?,
            self.read_32(addr.map_working(|it| it.wrapping_add(4)))?,
            self.read_32(addr.map_working(|it| it.wrapping_add(8)))?,
            self.read_32(addr.map_working(|it| it.wrapping_add(12)))?,
        ])
    }
}
