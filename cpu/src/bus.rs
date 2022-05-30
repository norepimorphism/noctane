pub mod io;

pub use io::Io;

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
            /// The size, in bytes, of this memory bank.
            pub const SIZE: usize = $size;

            /// The base address, relative to the start of each memory segment, of this memory bank.
            const BASE_ADDR: usize = $addr;
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
    fn select_access_bank_fn<T>(
        &mut self,
        addr: usize,
        access_word: impl FnOnce(&mut [u8]) -> T,
        access_io: impl FnOnce(&mut Self, usize) -> Result<T, io::Error>,
    ) -> Result<T, Error> {
        // To avoid explicitly defining a range for each memory bank, we can simply use an
        // unbounded range in the positive direction and work backwards; `match`es, to my knowledge,
        // work in a well-defined order from the first to last pattern.
        match addr {
            Bios::BASE_ADDR.. => {
                Ok(access_word(&mut self.bios[(addr - Bios::BASE_ADDR)..]))
            }
            Exp3::BASE_ADDR.. => {
                Ok(access_word(&mut self.exp_3[(addr - Exp3::BASE_ADDR)..]))
            }
            Exp2::BASE_ADDR.. => {
                Ok(access_word(&mut self.exp_2[(addr - Exp2::BASE_ADDR)..]))
            }
            Self::IO_BASE_ADDR.. => {
                tracing::debug!("io[{:#010x}]", addr);

                access_io(self, addr - Self::IO_BASE_ADDR).map_err(Error::from)
            }
            Exp1::BASE_ADDR.. => {
                Ok(access_word(&mut self.exp_1[(addr - Exp1::BASE_ADDR)..]))
            }
            MainRam::BASE_ADDR.. => {
                Ok(access_word(&mut self.main_ram[(addr - MainRam::BASE_ADDR)..]))
            }
            _ => Err(Error::UnmappedAddress(addr)),
        }
    }

    pub fn read_8(&mut self, addr: usize) -> Result<u8, Error> {
        self.select_access_bank_fn(
            addr,
            |word| word[0],
            |this, offset| this.io.read_8(offset),
        )
    }

    pub fn read_16(&mut self, addr: usize) -> Result<u16, Error> {
        self.select_access_bank_fn(
            addr,
            |word| u16::from_be_bytes(word[0..2].try_into().unwrap()),
            |this, offset| this.io.read_16(offset),
        )
    }

    pub fn read_32(&mut self, addr: usize) -> Result<u32, Error> {
        self.select_access_bank_fn(
            addr,
            |word| u32::from_be_bytes(word[0..4].try_into().unwrap()),
            |this, offset| this.io.read_32(offset),
        )
    }

    pub fn write_8(&mut self, addr: usize, value: u8) -> Result<(), Error> {
        self.select_access_bank_fn(
            addr,
            |word| word[0] = value,
            |this, offset| this.io.write_8(offset, value),
        )
    }

    pub fn write_16(&mut self, addr: usize, value: u16) -> Result<(), Error> {
        self.select_access_bank_fn(
            addr,
            |word| {
                word.as_chunks_mut::<2>().0[(addr >> 1) & 0b1] = value.to_be_bytes();
            },
            |this, offset| this.io.write_16(offset, value),
        )
    }

    pub fn write_32(&mut self, addr: usize, value: u32) -> Result<(), Error> {
        self.select_access_bank_fn(
            addr,
            |word| {
                word.as_chunks_mut::<4>().0[0] = value.to_be_bytes();
            },
            |this, offset| this.io.write_32(offset, value),
        )
    }
}
