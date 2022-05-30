use noctane_gpu::Gpu;
use noctane_proc_macro::gen_cpu_bus_io;

pub struct Io<'a> {
    pub gpu: &'a mut Gpu,
}

// impl Io<'_> {
//     fn select_access_lut_fn<T>(
//         &mut self,
//         addr: usize,
//         access_mem_ctrl_1: impl FnOnce(&mut Self, usize) -> T,
//         access_perif_io_ports: impl FnOnce(&mut Self, usize) -> T,
//         access_mem_ctrl_2: impl FnOnce(&mut Self, usize) -> T,
//         access_int_ctrl: impl FnOnce(&mut Self, usize) -> T,
//         access_dma: impl FnOnce(&mut Self, usize) -> T,
//         access_timers: impl FnOnce(&mut Self, usize) -> T,
//         access_cdrom: impl FnOnce(&mut Self, usize) -> T,
//     ) -> Result<T, Error> {
//         const IO_BASE_IDX:  usize = make_index(0x1f80_1000);
//         const IO_END_IDX:   usize = IO_BASE_IDX + <Bus>::IO_LEN;

//         let idx = make_index(addr as usize);

//         match idx {
//             MainRam::BASE_IDX..MainRam::END_IDX => {
//                 Ok(access_main_ram(self, idx - MainRam::BASE_IDX))
//             }
//             Exp1::BASE_IDX..Exp1::END_IDX => {
//                 Ok(access_exp_1(self, idx - Exp1::BASE_IDX))
//             }
//             IO_BASE_IDX..IO_END_IDX => {
//                 Ok(access_io(self, idx - IO_BASE_IDX))
//             }
//             Exp2::BASE_IDX..Exp2::END_IDX => {
//                 Ok(access_exp_2(self, idx - Exp2::BASE_IDX))
//             }
//             Exp3::BASE_IDX..Exp3::END_IDX => {
//                 Ok(access_exp_3(self, idx - Exp3::BASE_IDX))
//             }
//             Bios::BASE_IDX..Bios::END_IDX => {
//                 Ok(access_bios(self, idx - Bios::BASE_IDX))
//             }
//             _ => Err(Error::UnmappedAddress(addr)),
//         }
//     }
// }

macro_rules! def_read_with_offset {
    ($fn_name:ident() -> $ty:ty) => {
        pub fn $fn_name(&self, offset: usize) -> $ty {
            let reg = &REGISTERS[LUT[offset].idx];

            (reg.$fn_name)(reg, self, offset)
        }
    };
}

macro_rules! def_read_without_offset {
    ($fn_name:ident() -> $ty:ty) => {
        pub fn $fn_name(&self, offset: usize) -> $ty {
            let reg = &REGISTERS[LUT[offset].idx];

            (reg.$fn_name)(reg, self)
        }
    };
}

macro_rules! def_write_with_offset {
    ($fn_name:ident($ty:ty)) => {
        pub fn $fn_name(&mut self, offset: usize, value: $ty) {
            let reg = &REGISTERS[LUT[offset].idx];
            (reg.$fn_name)(reg, self, offset, value);
        }
    };
}

macro_rules! def_write_without_offset {
    ($fn_name:ident($ty:ty)) => {
        pub fn $fn_name(&mut self, offset: usize, value: $ty) {
            let reg = &REGISTERS[LUT[offset].idx];
            (reg.$fn_name)(reg, self, value);
        }
    };
}

impl Io<'_> {
    def_read_with_offset!(read_8() -> u8);
    def_read_with_offset!(read_16() -> u16);
    def_read_without_offset!(read_32() -> u32);
    def_write_with_offset!(write_8(u8));
    def_write_with_offset!(write_16(u16));
    def_write_without_offset!(write_32(u32));
}

struct Register {
    read_8: fn(&Self, &Io, usize) -> u8,
    read_16: fn(&Self, &Io, usize) -> u16,
    read_32: fn(&Self, &Io) -> u32,
    write_8: fn(&Self, &mut Io, usize, u8),
    write_16: fn(&Self, &mut Io, usize, u16),
    write_32: fn(&Self, &mut Io, u32),
}

struct LutEntry {
    idx: usize,
    start_offset: usize,
}

gen_cpu_bus_io!(
    // Memory Control 1.
    Lut {
        name: MEM_CTRL_1,
        regs: [
            Register {
                name: EXP_1_BASE_ADDR,
                read_32: |this, io| 0,
                write_32: |this, io, value| {},
            },
            Register {
                name: EXP_2_BASE_ADDR,
                read_32: |this, io| 0,
                write_32: |this, io, value| {},
            },
            Register {
                name: EXP_1_DELAY,
                read_32: |this, io| 0,
                write_32: |this, io, value| {},
            },
            Register {
                name: EXP_3_DELAY,
                read_32: |this, io| 0,
                write_32: |this, io, value| {},
            },
            Register {
                name: SPU_DELAY,
                read_32: |this, io| 0,
                write_32: |this, io, value| {},
            },
            Register {
                name: CDROM_DELAY,
                read_32: |this, io| 0,
                write_32: |this, io, value| {},
            },
            Register {
                name: EXP_2_DELAY,
                read_32: |this, io| 0,
                write_32: |this, io, value| {},
            },
            Register {
                name: COMMON_DELAY,
                read_32: |this, io| 0,
                write_32: |this, io, value| {},
            }
        ]
    },
    // Peripheral I/O Ports.
    Lut {
        name: PERIF_IO_PORTS,
        regs: [
            Register {
                name: JOY_DATA,
                read_32: |this, io| 0,
                write_32: |this, io, value| {},
            },
            Register {
                name: JOY_STAT,
                read_32: |this, io| 0,
                write_32: |this, io, value| {},
            },
            Register {
                name: JOY_MODE,
                read_32: |this, io| 0,
                write_32: |this, io, value| {},
            },
            Register {
                name: JOY_CTRL,
                read_32: |this, io| 0,
                write_32: |this, io, value| {},
            },
            Register {
                name: JOY_BAUD,
                read_32: |this, io| 0,
                write_32: |this, io, value| {},
            },
            Register {
                name: SIO_DATA,
                read_32: |this, io| 0,
                write_32: |this, io, value| {},
            },
            Register {
                name: SIO_STAT,
                read_32: |this, io| 0,
                write_32: |this, io, value| {},
            },
            Register {
                name: SIO_MODE,
                read_32: |this, io| 0,
                write_32: |this, io, value| {},
            },
            Register {
                name: SIO_CTRL,
                read_32: |this, io| 0,
                write_32: |this, io, value| {},
            },
            Register {
                name: SIO_MISC,
                read_32: |this, io| 0,
                write_32: |this, io, value| {},
            },
            Register {
                name: SIO_BAUD,
                read_32: |this, io| 0,
                write_32: |this, io, value| {},
            }
        ]
    },
    // Memory Control 2.
    Lut {
        name: MEM_CTRL_2,
        regs: [
            Register {
                name: RAM_SIZE,
                read_32: |this, io| 0,
                write_32: |this, io, value| {},
            }
        ]
    },
    // Interrupt Control.
    Lut {
        name: INT_CTRL,
        regs: [
            Register {
                name: STAT,
                read_32: |this, io| 0,
                write_32: |this, io, value| {},
            },
            Register {
                name: MASK,
                read_32: |this, io| 0,
                write_32: |this, io, value| {},
            }
        ]
    },
    // DMA.
    Lut {
        name: DMA,
        regs: [
            Register {
                name: MDECin,
                read_32: |this, io| 0,
                write_32: |this, io, value| {},
            },
            Register {
                name: MDECout,
                read_32: |this, io| 0,
                write_32: |this, io, value| {},
            },
            Register {
                name: GPU,
                read_32: |this, io| 0,
                write_32: |this, io, value| {},
            },
            Register {
                name: CDROM,
                read_32: |this, io| 0,
                write_32: |this, io, value| {},
            },
            Register {
                name: SPU,
                read_32: |this, io| 0,
                write_32: |this, io, value| {},
            },
            Register {
                name: PIO,
                read_32: |this, io| 0,
                write_32: |this, io, value| {},
            },
            Register {
                name: OTC,
                read_32: |this, io| 0,
                write_32: |this, io, value| {},
            },
            Register {
                name: DPCR,
                read_32: |this, io| 0,
                write_32: |this, io, value| {},
            },
            Register {
                name: DICR,
                read_32: |this, io| 0,
                write_32: |this, io, value| {},
            }
        ]
    },
    // Timers (Root Counters).
    Lut {
        name: TIMERS,
        regs: [
            Register {
                name: DOTCLOCK,
                read_32: |this, io| 0,
                write_32: |this, io, value| {},
            },
            Register {
                name: H_RETRACE,
                read_32: |this, io| 0,
                write_32: |this, io, value| {},
            },
            Register {
                name: SYS_CLOCK,
                read_32: |this, io| 0,
                write_32: |this, io, value| {},
            }
        ]
    },
    // CD-ROM.
    Lut {
        name: CDROM,
        regs: [
            Register {
                name: IDX,
                read_32: |this, io| 0,
                write_32: |this, io, value| {},
            },
            Register {
                name: RESPONSE,
                read_32: |this, io| 0,
                write_32: |this, io, value| {},
            },
            Register {
                name: DATA,
                read_32: |this, io| 0,
                write_32: |this, io, value| {},
            },
            Register {
                name: INT_ENABLE,
                read_32: |this, io| 0,
                write_32: |this, io, value| {},
            },
            Register {
                name: INT_FLAG,
                read_32: |this, io| 0,
                write_32: |this, io, value| {},
            },
            Mirror { name: INT_ENABLE },
            Mirror { name: INT_FLAG },
            Register {
                name: CMD,
                read_32: |this, io| 0,
                write_32: |this, io, value| {},
            },
            Register {
                name: PARAM,
                read_32: |this, io| 0,
                write_32: |this, io, value| {},
            },
            Register {
                name: REQUEST,
                read_32: |this, io| 0,
                write_32: |this, io, value| {},
            },
            Register {
                name: UNK_0,
                read_32: |this, io| 0,
                write_32: |this, io, value| {},
            },
            Mirror { name: INT_ENABLE },
            Mirror { name: INT_FLAG },
            Register {
                name: UNK_1,
                read_32: |this, io| 0,
                write_32: |this, io, value| {},
            },
            Register {
                name: VOL_LL,
                read_32: |this, io| 0,
                write_32: |this, io, value| {},
            },
            Register {
                name: VOL_LR,
                read_32: |this, io| 0,
                write_32: |this, io, value| {},
            },
            Register {
                name: VOL_RR,
                read_32: |this, io| 0,
                write_32: |this, io, value| {},
            },
            Register {
                name: VOL_RL,
                read_32: |this, io| 0,
                write_32: |this, io, value| {},
            },
            Register {
                name: VOL_APPLY,
                read_32: |this, io| 0,
                write_32: |this, io, value| {},
            }
        ]
    }
    // GPU.
    // MDEC.
    // SPU Voice.
    // SPU Control.
    // SPU Reverb Configuration.
    // SPU Internal.
    // Expansion Region 2.
    // Expansion Region 2 DUART.
);
