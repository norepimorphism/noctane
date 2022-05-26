#[derive(Debug)]
pub enum Error {
    UnmappedAddress(u32),
}

impl Default for Bus {
    fn default() -> Self {
        Self {
            main_ram: box [0; Self::MAIN_RAM_LEN],
            exp_1: box [0; Self::EXP_1_LEN],
            exp_2: box [0; Self::EXP_2_LEN],
            exp_3: box [0; Self::EXP_3_LEN],
            bios: box [0; Self::BIOS_LEN],
        }
    }
}

pub struct Bus {
    main_ram: Box<[u32; Self::MAIN_RAM_LEN]>,
    exp_1: Box::<[u32; Self::EXP_1_LEN]>,
    exp_2: Box::<[u32; Self::EXP_2_LEN]>,
    exp_3: Box::<[u32; Self::EXP_3_LEN]>,
    bios: Box<[u32; Self::BIOS_LEN]>,
}

impl Bus {
    const MAIN_RAM_LEN: usize = 0x08_0000;
    const EXP_1_LEN:    usize = 0x20_0000;
    const IO_LEN:       usize = 0x00_0800;
    const EXP_2_LEN:    usize = 0x00_0800;
    const EXP_3_LEN:    usize = 0x08_0000;
    const BIOS_LEN:     usize = 0x02_0000;

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
        const fn make_index(addr: usize) -> usize {
            (addr as usize) / std::mem::size_of::<u32>()
        }

        const MAIN_RAM_BASE_IDX:    usize = make_index(0x0000_0000);
        const MAIN_RAM_END_IDX:     usize = MAIN_RAM_BASE_IDX + <Bus>::MAIN_RAM_LEN;

        const EXP_1_BASE_IDX:       usize = make_index(0x1f00_0000);
        const EXP_1_END_IDX:        usize = EXP_1_BASE_IDX + <Bus>::EXP_1_LEN;

        const IO_BASE_IDX:          usize = make_index(0x1f80_1000);
        const IO_END_IDX:           usize = IO_BASE_IDX + <Bus>::IO_LEN;

        const EXP_2_BASE_IDX:       usize = make_index(0x1f80_2000);
        const EXP_2_END_IDX:        usize = EXP_2_BASE_IDX + <Bus>::EXP_2_LEN;

        const EXP_3_BASE_IDX:       usize = make_index(0x1fa0_0000);
        const EXP_3_END_IDX:        usize = EXP_3_BASE_IDX + <Bus>::EXP_3_LEN;

        const BIOS_BASE_IDX:        usize = make_index(0x1fc0_0000);
        const BIOS_END_IDX:         usize = BIOS_BASE_IDX + <Bus>::BIOS_LEN;

        let idx = make_index(addr as usize);

        match idx {
            MAIN_RAM_BASE_IDX..MAIN_RAM_END_IDX => {
                Ok(access_main_ram(self, idx - MAIN_RAM_BASE_IDX))
            }
            EXP_1_BASE_IDX..EXP_1_END_IDX => {
                Ok(access_exp_1(self, idx - EXP_1_BASE_IDX))
            }
            IO_BASE_IDX..IO_END_IDX => {
                Ok(access_io(self, idx - IO_BASE_IDX))
            }
            EXP_2_BASE_IDX..EXP_2_END_IDX => {
                Ok(access_exp_2(self, idx - EXP_2_BASE_IDX))
            }
            EXP_3_BASE_IDX..EXP_3_END_IDX => {
                Ok(access_exp_3(self, idx - EXP_3_BASE_IDX))
            }
            BIOS_BASE_IDX..BIOS_END_IDX => {
                Ok(access_bios(self, idx - BIOS_BASE_IDX))
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
