use noctane_gpu::Gpu;

#[derive(Debug)]
pub enum Error {
    UnmappedAddress(u32),
}

macro_rules! def_bank {
    ($name:ident, $len:literal, $addr:literal) => {
        impl Default for $name {
            fn default() -> Self {
                Self([0; Self::LEN])
            }
        }

        impl $name {
            pub const LEN: usize = $len;

            const BASE_IDX: usize = make_index($addr);
            const END_IDX: usize = $name::BASE_IDX + $name::LEN;
        }

        pub struct $name([u32; Self::LEN]);

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
    (addr as usize) / std::mem::size_of::<u32>()
}

def_bank!(MainRam,  0x08_0000, 0x0000_0000);
def_bank!(Exp1,     0x20_0000, 0x1f00_0000);
def_bank!(Exp2,     0x00_0800, 0x1f80_2000);
def_bank!(Exp3,     0x08_0000, 0x1fa0_0000);
def_bank!(Bios,     0x02_0000, 0x1fc0_0000);

impl<'a> Bus<'a> {
    pub fn new(
        main_ram: &'a mut MainRam,
        exp_1: &'a mut Exp1,
        gpu: &'a mut Gpu,
        exp_2: &'a mut Exp2,
        exp_3: &'a mut Exp3,
        bios: &'a mut Bios,
    ) -> Self {
        Self {
            main_ram,
            exp_1,
            gpu,
            exp_2,
            exp_3,
            bios,
        }
    }
}

pub struct Bus<'a> {
    pub main_ram: &'a mut MainRam,
    pub exp_1: &'a mut Exp1,
    pub gpu: &'a mut Gpu,
    pub exp_2: &'a mut Exp2,
    pub exp_3: &'a mut Exp3,
    pub bios: &'a mut Bios,
}

impl Bus<'_> {
    const IO_LEN: usize = 0x00_0800;

    fn select_access_bank_fn<T>(
        &mut self,
        addr: u32,
        access_main_ram: impl FnOnce(&mut Self, usize) -> T,
        access_exp_1: impl FnOnce(&mut Self, usize) -> T,
        access_io: impl FnOnce(&mut Self, usize) -> T,
        access_exp_2: impl FnOnce(&mut Self, usize) -> T,
        access_exp_3: impl FnOnce(&mut Self, usize) -> T,
        access_bios: impl FnOnce(&mut Self, usize) -> T,
    ) -> Result<T, Error> {
        const IO_BASE_IDX:  usize = make_index(0x1f80_1000);
        const IO_END_IDX:   usize = IO_BASE_IDX + <Bus>::IO_LEN;

        let idx = make_index(addr as usize);

        match idx {
            MainRam::BASE_IDX..MainRam::END_IDX => {
                Ok(access_main_ram(self, idx - MainRam::BASE_IDX))
            }
            Exp1::BASE_IDX..Exp1::END_IDX => {
                Ok(access_exp_1(self, idx - Exp1::BASE_IDX))
            }
            IO_BASE_IDX..IO_END_IDX => {
                Ok(access_io(self, idx - IO_BASE_IDX))
            }
            Exp2::BASE_IDX..Exp2::END_IDX => {
                Ok(access_exp_2(self, idx - Exp2::BASE_IDX))
            }
            Exp3::BASE_IDX..Exp3::END_IDX => {
                Ok(access_exp_3(self, idx - Exp3::BASE_IDX))
            }
            Bios::BASE_IDX..Bios::END_IDX => {
                Ok(access_bios(self, idx - Bios::BASE_IDX))
            }
            _ => Err(Error::UnmappedAddress(addr)),
        }
    }

    pub fn read_32(&mut self, addr: u32) -> Result<u32, Error> {
        self.select_access_bank_fn(
            addr,
            |this, idx| {
                this.main_ram[idx]
            },
            |this, idx| {
                this.exp_1[idx]
            },
            |_, _| {
                tracing::debug!("READ IO @ {:08x}", addr);

                // TODO
                0
            },
            |this, idx| {
                this.exp_2[idx]
            },
            |this, idx| {
                this.exp_3[idx]
            },
            |this, idx| {
                this.bios[idx]
            },
        )
    }

    pub fn write_32(&mut self, addr: u32, value: u32) -> Result<(), Error> {
        self.select_access_bank_fn(
            addr,
            |this, idx| {
                this.main_ram[idx] = value;
            },
            |this, idx| {
                this.exp_1[idx] = value;
            },
            |_, _| {
                tracing::debug!("WRITE IO @ {:08x} = {:x}", addr, value);

                // TODO
            },
            |this, idx| {
                this.exp_2[idx] = value;
            },
            |this, idx| {
                this.exp_3[idx] = value;
            },
            |this, idx| {
                this.bios[idx] = value;
            },
        )
    }
}
