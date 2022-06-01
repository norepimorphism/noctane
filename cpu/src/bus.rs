//! The CPU memory bus.
//!
//! This is the stage at which memory accesses are delegated to the appropriate memory bank.

pub mod io;

pub use io::Io;

use crate::mem::Address;

macro_rules! def_bank {
    ($name:ident, $size:literal @ $addr:literal) => {
        impl Default for $name {
            fn default() -> Self {
                // Use 'screaming 0xaa's so that it's more obvious when we access uninitialized
                // memory.
                Self(box [0xaa; Self::LEN])
            }
        }

        impl $name {
            /// The base address, relative to the start of each memory segment, of this memory bank.
            pub const BASE_ADDR: usize = $addr;

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
def_bank!(Exp1,     0x80_0000 @ 0x1f00_0000);
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
        mut addr: Address,
        access_word: impl FnOnce(&mut u32) -> T,
        access_io: impl FnOnce(&mut Self, Address) -> Result<T, ()>,
    ) -> T {
        macro_rules! get_bank {
            ($field_name:ident, $struct_name:ident $(,)?) => {
                {
                    addr.working -= $struct_name::BASE_ADDR;
                    tracing::trace!(
                        "Accessing `cpu_bus.{}[{:#010x}]`",
                        stringify!($field_name),
                        addr.working,
                    );

                    self.$field_name
                        .get_mut(make_index(addr.working))
                        .map(|it| access_word(it))
                        .ok_or(())
                }
            };
        }

        // To avoid explicitly defining a range for each memory bank, we can simply use an
        // unbounded range in the positive direction and work backwards; `match`es work in a
        // well-defined order from the first to last pattern.
        match addr.working {
            Bios::BASE_ADDR.. => get_bank!(bios, Bios),
            Exp3::BASE_ADDR.. => get_bank!(exp_3, Exp3),
            Self::IO_BASE_ADDR.. => {
                access_io(self, addr.map_working(|it| it - Self::IO_BASE_ADDR))
            }
            Exp1::BASE_ADDR.. => get_bank!(exp_1, Exp1),
            MainRam::BASE_ADDR.. => get_bank!(main_ram, MainRam),
            _ => Err(()),
        }
        .unwrap_or(T::default())
    }

    pub fn read_8(&mut self, addr: Address) -> u8 {
        self.access(
            addr,
            |word| {
                addr.index_byte_in_word(*word)
            },
            |this, addr| {
                this.io.read_8(addr)
            },
        )
    }

    pub fn read_16(&mut self, addr: Address) -> u16 {
        self.access(
            addr,
            |word| {
                addr.index_halfword_in_word(*word)
            },
            |this, addr| {
                this.io.read_16(addr)
            },
        )
    }

    pub fn read_32(&mut self, addr: Address) -> u32 {
        self.access(
            addr,
            |word| {
                *word
            },
            |this, addr| {
                this.io.read_32(addr)
            },
        )
    }

    pub fn write_8(&mut self, addr: Address, value: u8) {
        self.access(
            addr,
            |word| {
                let mut bytes = word.to_be_bytes();
                bytes[addr.byte_idx] = value;
                *word = u32::from_be_bytes(bytes);
            },
            |this, addr| {
                this.io.write_8(addr, value)
            },
        )
    }

    pub fn write_16(&mut self, addr: Address, value: u16) {
        self.access(
            addr,
            |word| {
                let mut bytes = word.to_be_bytes();
                bytes.as_chunks_mut::<2>().0[addr.halfword_idx] = value.to_be_bytes();
                *word = u32::from_be_bytes(bytes);
            },
            |this, addr| {
                this.io.write_16(addr, value)
            },
        )
    }

    pub fn write_32(&mut self, addr: Address, value: u32) {
        self.access(
            addr,
            |word| {
                *word = value;
            },
            |this, addr| {
                this.io.write_32(addr, value)
            },
        )
    }

    pub fn fetch_cache_line(&mut self, addr: Address) -> [u32; 4] {
        tracing::trace!("Fetching cache line (addr={:#010x})", addr.working);

        let a = self.read_32(addr.map_working(|it| it.wrapping_add(0)));
        let b = self.read_32(addr.map_working(|it| it.wrapping_add(4)));
        let c = self.read_32(addr.map_working(|it| it.wrapping_add(8)));
        let d = self.read_32(addr.map_working(|it| it.wrapping_add(12)));

        tracing::trace!("line[0] <- {:#010x}", a);
        tracing::trace!("line[1] <- {:#010x}", b);
        tracing::trace!("line[2] <- {:#010x}", c);
        tracing::trace!("line[3] <- {:#010x}", d);

        [a, b, c, d]
    }
}
