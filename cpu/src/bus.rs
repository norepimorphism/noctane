//! The CPU memory bus.
//!
//! This is the stage at which memory accesses are delegated to the appropriate memory bank.

pub mod io;

pub use io::Io;

use crate::mem::Address;

impl From<io::Error> for Error {
    fn from(e: io::Error) -> Self {
        match e {
            io::Error::UnmappedAddress(addr) => {
                Self::UnmappedAddress(addr.wrapping_add(Bus::IO_BASE_ADDR))
            }
        }
    }
}

/// The error type returned by `read` and `write` functions.
#[derive(Debug)]
pub enum Error {
    /// An invalid address was used in a bus access.
    ///
    /// You may receive this if your addresses are not rebased relative to the start of each memory
    /// segment (`kuseg`, `kseg0`, etc.).
    UnmappedAddress(usize),
}

macro_rules! def_bank {
    ($name:ident, $size:literal @ $addr:literal) => {
        impl Default for $name {
            fn default() -> Self {
                Self(box [0; $size])
            }
        }

        impl $name {
            /// The base address, relative to the start of each memory segment, of this memory bank.
            pub const BASE_ADDR: usize = $addr;
        }

        /// A bank of memory.
        ///
        /// Internally, the data contained within this type is boxed so as to avoid otherwise
        /// certain stack buffer overflows from occuring as a result of large allocations.
        pub struct $name(Box<[u8; $size]>);

        impl std::ops::Deref for $name {
            type Target = [u8; $size];

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

// Define the memory banks (except for the I/O region, which is not a simple buffer like the rest
// are).
def_bank!(MainRam,  0x20_0000 @ 0x0000_0000);
def_bank!(Exp1,     0x80_0000 @ 0x1f00_0000);
def_bank!(Exp2,     0x00_2000 @ 0x1f80_2000);
def_bank!(Exp3,     0x20_0000 @ 0x1fa0_0000);
def_bank!(Bios,     0x08_0000 @ 0x1fc0_0000);

impl<'a> Bus<'a> {
    /// Creates a new [`Bus`].
    pub fn new(
        main_ram: &'a mut MainRam,
        exp_1: &'a mut Exp1,
        io: Io<'a>,
        exp_2: &'a mut Exp2,
        exp_3: &'a mut Exp3,
        bios: &'a mut Bios,
    ) -> Self {
        Self {
            main_ram,
            exp_1,
            io,
            exp_2,
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
    /// [`Exp2`].
    pub exp_2: &'a mut Exp2,
    /// [`Exp3`].
    pub exp_3: &'a mut Exp3,
    /// [`Bios`].
    pub bios: &'a mut Bios,
}

impl Bus<'_> {
    /// The base address, relative to the start of each memory segment, of the I/O region.
    const IO_BASE_ADDR: usize = 0x1f80_0000;

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
    fn access<T>(
        &mut self,
        addr: Address,
        access_word: impl FnOnce(&mut [u8]) -> Result<T, Error>,
        access_io: impl FnOnce(&mut Self, Address) -> Result<T, io::Error>,
    ) -> Result<T, Error> {
        macro_rules! get_bank {
            ($field_name:ident, $struct_name:ident $(,)?) => {
                {
                    let idx = addr.working - $struct_name::BASE_ADDR;
                    tracing::trace!(
                        "Accessing `cpu_bus.{}[{:#010x}]`",
                        stringify!($field_name),
                        idx,
                    );

                    &mut self.$field_name[idx..]
                }
            };
        }

        // To avoid explicitly defining a range for each memory bank, we can simply use an
        // unbounded range in the positive direction and work backwards; `match`es, to my knowledge,
        // work in a well-defined order from the first to last pattern.
        match addr.working {
            Bios::BASE_ADDR.. => access_word(get_bank!(bios, Bios)),
            Exp3::BASE_ADDR.. => access_word(get_bank!(exp_3, Exp3)),
            Exp2::BASE_ADDR.. => access_word(get_bank!(exp_2, Exp2)),
            Self::IO_BASE_ADDR.. => access_io(self, addr).map_err(Error::from),
            Exp1::BASE_ADDR.. => access_word(get_bank!(exp_1, Exp1)),
            MainRam::BASE_ADDR.. => access_word(get_bank!(main_ram, MainRam)),
            _ => Err(Error::UnmappedAddress(addr.init)),
        }
    }

    pub fn read_8(&mut self, addr: Address) -> Result<u8, Error> {
        self.access(
            addr,
            |word| {
                word
                    .get(0)
                    .ok_or(Error::UnmappedAddress(addr.init))
                    .map(|it| *it)
            },
            |this, addr| {
                this.io.read_8(addr)
            },
        )
    }

    pub fn read_16(&mut self, addr: Address) -> Result<u16, Error> {
        self.access(
            addr,
            |word| {
                Ok(u16::from_be_bytes({
                    word
                        .get(0..2)
                        .ok_or(Error::UnmappedAddress(addr.init))?
                        .try_into()
                        .unwrap()
                }))
            },
            |this, addr| {
                this.io.read_16(addr)
            },
        )
    }

    pub fn read_32(&mut self, addr: Address) -> Result<u32, Error> {
        self.access(
            addr,
            |word| {
                Ok(u32::from_be_bytes({
                    word
                        .get(0..4)
                        .ok_or(Error::UnmappedAddress(addr.init))?
                        .try_into()
                        .unwrap()
                }))
            },
            |this, addr| {
                this.io.read_32(addr)
            },
        )
    }

    pub fn write_8(&mut self, addr: Address, value: u8) -> Result<(), Error> {
        self.access(
            addr,
            |word| {
                *word.get_mut(0).ok_or(Error::UnmappedAddress(addr.init))? = value;

                Ok(())
            },
            |this, addr| {
                this.io.write_8(addr, value)
            },
        )
    }

    pub fn write_16(&mut self, addr: Address, value: u16) -> Result<(), Error> {
        self.access(
            addr,
            |word| {
                *word
                    .as_chunks_mut::<2>()
                    .0
                    .get_mut(addr.halfword_idx)
                    .ok_or(Error::UnmappedAddress(addr.init))? = value.to_be_bytes();

                Ok(())
            },
            |this, addr| {
                this.io.write_16(addr, value)
            },
        )
    }

    pub fn write_32(&mut self, addr: Address, value: u32) -> Result<(), Error> {
        self.access(
            addr,
            |word| {
                *word
                    .as_chunks_mut::<4>()
                    .0
                    .get_mut(0)
                    .ok_or(Error::UnmappedAddress(addr.init))? = value.to_be_bytes();

                Ok(())
            },
            |this, addr| {
                this.io.write_32(addr, value)
            },
        )
    }

    pub fn fetch_cache_line(&mut self, addr: Address) -> Result<[u32; 4], Error> {
        tracing::trace!("Fetching cache line (addr={:#010x})", addr.working);

        let a = self.read_32(addr.map_working(|it| it.wrapping_add(0)))?;
        let b = self.read_32(addr.map_working(|it| it.wrapping_add(4)))?;
        let c = self.read_32(addr.map_working(|it| it.wrapping_add(8)))?;
        let d = self.read_32(addr.map_working(|it| it.wrapping_add(12)))?;

        tracing::trace!("line[0] <- {:#010x}", a);
        tracing::trace!("line[1] <- {:#010x}", b);
        tracing::trace!("line[2] <- {:#010x}", c);
        tracing::trace!("line[3] <- {:#010x}", d);

        Ok([a, b, c, d])
    }
}
