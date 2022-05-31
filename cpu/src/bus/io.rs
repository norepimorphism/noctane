use noctane_gpu::Gpu;
use noctane_proc_macro::gen_cpu_bus_io;

use crate::mem::Address;

/// The error type returned by `read` and `write` functions.
pub enum Error {
    /// An invalid address was used in an I/O access.
    ///
    /// You may receive this if your addresses are not rebased relative to the start of the I/O
    /// region (which they should be).
    UnmappedAddress(usize),
}

/// External components accessible to the CPU via I/O registers.
pub struct Io<'a> {
    pub gpu: &'a mut Gpu,
}

impl Io<'_> {
    /// Selects the appropriate I/O register entry and byte index for a given address and passes it
    /// to a callback.
    fn access<T>(
        &mut self,
        addr: Address,
        f: impl FnOnce(&mut Self, &Register, Address) -> T,
    ) -> Result<T, Error> {
        macro_rules! get_lut_entry {
            ($addr:expr, $mod_name:ident $(,)?) => {
                $mod_name::LUT.get($addr.working - $mod_name::BASE_ADDR)
            };
        }

        // To avoid explicitly defining a range for each register group, we can simply use an
        // unbounded range in the positive direction and work backwards; `match`es, to my knowledge,
        // work in a well-defined order from the first to last pattern.
        let lut_entry = match addr.working {
            cdrom::BASE_ADDR.. => get_lut_entry!(addr, cdrom),
            timers::BASE_ADDR.. => get_lut_entry!(addr, timers),
            dma::BASE_ADDR.. => get_lut_entry!(addr, dma),
            int_ctrl::BASE_ADDR.. => get_lut_entry!(addr, int_ctrl),
            mem_ctrl_2::BASE_ADDR.. => get_lut_entry!(addr, mem_ctrl_2),
            perif::BASE_ADDR.. => get_lut_entry!(addr, perif),
            mem_ctrl_1::BASE_ADDR.. => get_lut_entry!(addr, mem_ctrl_1),
            _ => None,
        }
        .ok_or(Error::UnmappedAddress(addr.init))?;

        // [`Option::get`] is unnecessary here as LUT entry indices are guaranteed by
        // [`gen_cpu_bus_io`] to point to valid register entries.
        let reg = &REGISTERS[lut_entry.reg_idx];

        Ok(f(self, reg, Address::from(lut_entry.byte_idx)))
    }
}

/// Defines a `read_` method in which a byte index is relevant (i.e., `read_8`, `read_16`).
macro_rules! def_read_with_addr {
    ($fn_name:ident() -> $ty:ty) => {
        pub fn $fn_name(&mut self, addr: Address) -> Result<$ty, Error> {
            self.access(addr, |this, reg, addr| (reg.$fn_name)(reg, this, addr))
        }
    };
}

/// Defines a `read_` method in which a byte index is not relevant (i.e., `read_32`).
macro_rules! def_read_without_addr {
    ($fn_name:ident() -> $ty:ty) => {
        pub fn $fn_name(&mut self, addr: Address) -> Result<$ty, Error> {
            self.access(addr, |this, reg, _| (reg.$fn_name)(reg, this))
        }
    };
}

/// Defines a `write_` method in which a byte index is relevant (i.e., `write_8`, `write_16`).
macro_rules! def_write_with_addr {
    ($fn_name:ident($ty:ty)) => {
        pub fn $fn_name(&mut self, addr: Address, value: $ty) -> Result<(), Error> {
            self.access(addr, |this, reg, addr| (reg.$fn_name)(reg, this, addr, value))
        }
    };
}

/// Defines a `write_` method in which a byte index is not relevant (i.e., `write_32`).
macro_rules! def_write_without_addr {
    ($fn_name:ident($ty:ty)) => {
        pub fn $fn_name(&mut self, addr: Address, value: $ty) -> Result<(), Error> {
            self.access(addr, |this, reg, _| (reg.$fn_name)(reg, this, value))
        }
    };
}

// This block defines `read_8`, `read_16`, `read_32`, `write_8`, `write_16`, and `write_32`.
impl Io<'_> {
    def_read_with_addr!(read_8() -> u8);
    def_read_with_addr!(read_16() -> u16);
    def_read_without_addr!(read_32() -> u32);
    def_write_with_addr!(write_8(u8));
    def_write_with_addr!(write_16(u16));
    def_write_without_addr!(write_32(u32));
}

/// An entry within the [`REGISTERS`] table that represents a single register.
///
/// As can be seen in the [`gen_cpu_bus_io`] macro, only one `read` and one `write` function are
/// explicitly defined for a register, and the remaining functions for other bit widths are
/// automagically implemented.
struct Register {
    read_8: fn(&Self, &Io, Address) -> u8,
    read_16: fn(&Self, &Io, Address) -> u16,
    read_32: fn(&Self, &Io) -> u32,
    write_8: fn(&Self, &mut Io, Address, u8),
    write_16: fn(&Self, &mut Io, Address, u16),
    write_32: fn(&Self, &mut Io, u32),
}

/// An entry within any of the lookup tables (LUTs) defined in the submodules for the various
/// I/O register groups. This entry represents one byte of a single register.
///
/// `reg_idx` points to an register entry in the [`REGISTERS`] table, while `byte_idx` indicates
/// the index of the byte represented by this [`LutEntry`] into the register it points to, and is
/// intended as the basis for the `addr` input to [`Register`] functions.
struct LutEntry {
    reg_idx: usize,
    byte_idx: usize,
}

// Define the I/O registers. This is going to be quite long...
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
                name: BIOS_DELAY,
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
        name: perif,
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
                read_16: |this, io, offset| 0,
                write_16: |this, io, offset, value| {},
            },
            Register {
                name: JOY_CTRL,
                read_16: |this, io, offset| 0,
                write_16: |this, io, offset, value| {},
            },
            Register {
                name: JOY_BAUD,
                read_16: |this, io, offset| 0,
                write_16: |this, io, offset, value| {},
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
                read_16: |this, io, offset| 0,
                write_16: |this, io, offset, value| {},
            },
            Register {
                name: SIO_CTRL,
                read_16: |this, io, offset| 0,
                write_16: |this, io, offset, value| {},
            },
            Register {
                name: SIO_MISC,
                read_16: |this, io, offset| 0,
                write_16: |this, i, offset, value| {},
            },
            Register {
                name: SIO_BAUD,
                read_16: |this, io, offset| 0,
                write_16: |this, io, offset, value| {},
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
