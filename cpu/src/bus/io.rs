use noctane_gpu::Gpu;
use noctane_proc_macro::gen_cpu_bus_io;

pub enum Error {
    UnmappedAddress(usize),
}

pub struct Io<'a> {
    pub gpu: &'a mut Gpu,
}

impl Io<'_> {
    fn access<T>(
        &mut self,
        addr: usize,
        f: impl FnOnce(&mut Self, &Register, usize) -> T,
    ) -> Result<T, Error> {
        let lut_entry = match addr {
            mem_ctrl_1::BASE_ADDR..perif_ports::BASE_ADDR => {
                Ok(&mem_ctrl_1::LUT[addr - mem_ctrl_1::BASE_ADDR])
            }
            _ => Err(Error::UnmappedAddress(addr)),
        }?;

        Ok(f(self, &REGISTERS[lut_entry.idx], lut_entry.start_offset))
    }
}

macro_rules! def_read_with_offset {
    ($fn_name:ident() -> $ty:ty) => {
        pub fn $fn_name(&mut self, addr: usize) -> Result<$ty, Error> {
            self.access(addr, |this, reg, offset| (reg.$fn_name)(reg, this, offset))
        }
    };
}

macro_rules! def_read_without_offset {
    ($fn_name:ident() -> $ty:ty) => {
        pub fn $fn_name(&mut self, addr: usize) -> Result<$ty, Error> {
            self.access(addr, |this, reg, _| (reg.$fn_name)(reg, this))
        }
    };
}

macro_rules! def_write_with_offset {
    ($fn_name:ident($ty:ty)) => {
        pub fn $fn_name(&mut self, addr: usize, value: $ty) -> Result<(), Error> {
            self.access(addr, |this, reg, offset| (reg.$fn_name)(reg, this, offset, value))
        }
    };
}

macro_rules! def_write_without_offset {
    ($fn_name:ident($ty:ty)) => {
        pub fn $fn_name(&mut self, addr: usize, value: $ty) -> Result<(), Error> {
            self.access(addr, |this, reg, _| (reg.$fn_name)(reg, this, value))
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
        name: mem_ctrl_1,
        base_addr: 0x1000,
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
            },
        ],
    },
    // Peripheral I/O Ports.
    Lut {
        name: perif_ports,
        base_addr: 0x1040,
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
            },
        ],
    },
    // Memory Control 2.
    Lut {
        name: mem_ctrl_2,
        base_addr: 0x1060,
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
        name: int_ctrl,
        base_addr: 0x1070,
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
            },
        ],
    },
    // DMA.
    Lut {
        name: dma,
        base_addr: 0x1080,
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
            },
        ],
    },
    // Timers (Root Counters).
    Lut {
        name: timers,
        base_addr: 0x1100,
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
            },
        ],
    },
    // CD-ROM.
    Lut {
        name: cdrom,
        base_addr: 0x1800,
        regs: [],
    },
    // GPU.
    // MDEC.
    // SPU Voice.
    // SPU Control.
    // SPU Reverb Configuration.
    // SPU Internal.
    // Expansion Region 2.
    // Expansion Region 2 DUART.
);
