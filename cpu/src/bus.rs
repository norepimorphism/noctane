pub mod io;

pub use io::Io;

impl From<io::Error> for Error {
    fn from(e: io::Error) -> Self {
        match e {
            io::Error::UnmappedAddress(addr) => {
                Self::UnmappedAddress(addr.wrapping_add(Bus::IO_BASE_IDX * 4))
            }
        }
    }
}

#[derive(Debug)]
pub enum Error {
    UnmappedAddress(usize),
}

macro_rules! def_bank {
    ($name:ident, $len:literal @ $addr:literal) => {
        impl Default for $name {
            fn default() -> Self {
                Self(box [0; Self::LEN])
            }
        }

        impl $name {
            pub const LEN: usize = $len;

            const BASE_IDX: usize = make_index($addr);
            const END_IDX: usize = $name::BASE_IDX + $name::LEN;
        }

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

def_bank!(MainRam,  0x08_0000 @ 0x0000_0000);
def_bank!(Exp1,     0x20_0000 @ 0x1f00_0000);
def_bank!(Exp2,     0x00_0800 @ 0x1f80_2000);
def_bank!(Exp3,     0x08_0000 @ 0x1fa0_0000);
def_bank!(Bios,     0x02_0000 @ 0x1fc0_0000);

impl<'a> Bus<'a> {
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

pub struct Bus<'a> {
    pub main_ram: &'a mut MainRam,
    pub exp_1: &'a mut Exp1,
    pub io: Io<'a>,
    pub exp_2: &'a mut Exp2,
    pub exp_3: &'a mut Exp3,
    pub bios: &'a mut Bios,
}

impl Bus<'_> {
    const IO_BASE_IDX:  usize = make_index(0x1f80_1000);
    const IO_LEN:       usize = 0x00_0800;
    const IO_END_IDX:   usize = Self::IO_BASE_IDX + Self::IO_LEN;

    fn select_access_bank_fn<T>(
        &mut self,
        addr: usize,
        access_word: impl FnOnce(&mut u32) -> T,
        access_io: impl FnOnce(&mut Self, usize) -> Result<T, io::Error>,
    ) -> Result<T, Error> {
        let idx = make_index(addr);

        match idx {
            MainRam::BASE_IDX..MainRam::END_IDX => {
                Ok(access_word(&mut self.main_ram[idx - MainRam::BASE_IDX]))
            }
            Exp1::BASE_IDX..Exp1::END_IDX => {
                Ok(access_word(&mut self.exp_1[idx - Exp1::BASE_IDX]))
            }
            Self::IO_BASE_IDX..Self::IO_END_IDX => {
                access_io(self, idx - Self::IO_BASE_IDX).map_err(Error::from)
            }
            Exp2::BASE_IDX..Exp2::END_IDX => {
                Ok(access_word(&mut self.exp_2[idx - Exp2::BASE_IDX]))
            }
            Exp3::BASE_IDX..Exp3::END_IDX => {
                Ok(access_word(&mut self.exp_3[idx - Exp3::BASE_IDX]))
            }
            Bios::BASE_IDX..Bios::END_IDX => {
                Ok(access_word(&mut self.bios[idx - Bios::BASE_IDX]))
            }
            _ => Err(Error::UnmappedAddress(addr)),
        }
    }

    pub fn read_32(&mut self, addr: usize) -> Result<u32, Error> {
        self.select_access_bank_fn(
            addr,
            |word| *word,
            |this, offset| {
                tracing::debug!("io[{:#010x}] -> 0x0", addr);

                this.io.read_32(offset)
            },
        )
    }

    pub fn write_32(&mut self, addr: usize, value: u32) -> Result<(), Error> {
        self.select_access_bank_fn(
            addr,
            |word| {
                *word = value;
            },
            |this, offset| {
                tracing::debug!("io[{:#010x}] <- {:#010x}", addr, value);

                this.io.write_32(offset, value)
            },
        )
    }
}
