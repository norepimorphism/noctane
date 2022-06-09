// SPDX-License-Identifier: MPL-2.0

//! The CPU memory bus.
//!
//! This is the stage at which memory accesses are delegated to the appropriate memory bank.

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
                /// The base address, relative to the start of each memory segment, of this memory
                /// bank.
                pub const BASE_ADDR: usize = $addr;
            )?

            const LEN: usize = make_index($size);
        }

        /// A bank of memory.
        ///
        /// Internally, the data contained within this type is boxed so as to avoid otherwise
        /// certain stack buffer overflows from occuring as a result of large allocations.
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
def_bank!(MainRam,  0x20_0000 @ 0x0000_0000);
def_bank!(Exp1,     0x80_0000);
def_bank!(Exp3,     0x20_0000 @ 0x1fa0_0000);
def_bank!(Bios,     0x08_0000 @ 0x1fc0_0000);

impl<'a> Bus<'a> {
    /// Creates a new [`Bus`].
    pub fn new(
        main_ram: &'a mut MainRam,
        exp_1: &'a mut Exp1,
        io: Io<'a>,
        exp_3: &'a mut Exp3,
        bios: &'a mut Bios,
    ) -> Self {
        Self {
            main_ram,
            exp_1,
            io,
            exp_3,
            bios,
        }
    }
}

/// The CPU memory bus.
pub struct Bus<'a> {
    /// [`MainRam`].
    pub main_ram: &'a mut MainRam,
    /// [`Exp1`].
    pub exp_1: &'a mut Exp1,
    /// Input and output (I/O).
    pub io: Io<'a>,
    /// [`Exp3`].
    pub exp_3: &'a mut Exp3,
    /// [`Bios`].
    pub bios: &'a mut Bios,
}

impl Bus<'_> {
    /// The base address, relative to the start of each memory segment, of the I/O region.
    const IO_BASE_ADDR: usize = 0x1f80_1000;

    /// Selects the appropriate memory bank for a given segment-relative address and passes it to
    /// one of two functions depending on how the data should be accessed.
    ///
    /// `access_word` is called when a normal, buffer-backed memory bank is accessed, such as the
    /// BIOS ROM. A slice into the entire buffer, offset by a value determined from the address, is
    /// passed to `access_word`. Implementations of `access_word` are expected to extract the first
    /// few relevant bytes and form them into a single value.
    ///
    /// `access_io` is called when the given address points into the I/O region. An address relative
    /// to the start of the I/O region is passed to `access_io`. Implementations of `access_io` are
    /// expected to simply delegate to the appropriate I/O read/write function.
    fn access<T: Default>(
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
                            .get_mut(make_index(rebased_addr))
                            .map(|it| access_word(it))
                            .ok_or(())
                    }
                )
            };
        }

        // TODO: Don't hardcode RAM size.
        try_access_bank!(main_ram,  MainRam::BASE_ADDR,             0x20_0000);
        try_access_bank!(bios,      Bios::BASE_ADDR,                self.io.mem_ctrl_1.bios_size);
        try_access_bank!(exp_1,     self.io.mem_ctrl_1.exp_1_base,  self.io.mem_ctrl_1.exp_1_size);
        try_access_bank!(exp_3,     Exp3::BASE_ADDR,                self.io.mem_ctrl_1.exp_3_size);

        try_access!(
            Self::IO_BASE_ADDR,
            0x1000,
            |rebased_addr: usize| {
                access_io(self, addr.map_working(|_| rebased_addr))
            },
        );
        try_access!(
            self.io.mem_ctrl_1.exp_2_base,
            self.io.mem_ctrl_1.exp_2_size,
            |rebased_addr: usize| {
                tracing::info!("rebased_addr: {:#010x}", rebased_addr + 0x1000);
                access_io(self, addr.map_working(|_| rebased_addr.wrapping_add(0x1000)))
            },
        );

        Err(())
    }

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
