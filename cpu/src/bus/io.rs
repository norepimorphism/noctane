// SPDX-License-Identifier: MPL-2.0

use noctane_gpu::Gpu;
use noctane_proc_macro::gen_cpu_bus_io;

use crate::mem::Address;

pub use bus_ctrl::Control as BusControl;
pub use post::Post;
pub use ram_ctrl::Control as RamControl;
pub use spu_ctrl::Control as SpuControl;
pub use spu_voice::Voice as SpuVoice;
pub use timers::{Timer, Timers};

/// External components accessible to the CPU via I/O registers.
#[derive(Debug)]
pub struct Io<'a> {
    /// The graphics processing unit (GPU).
    pub gpu: &'a mut Gpu,
    pub bus_ctrl: &'a mut BusControl,
    pub post: &'a mut Post,
    pub ram_ctrl: &'a mut RamControl,
    pub spu_ctrl: &'a mut SpuControl,
    pub spu_voices: &'a mut [SpuVoice; 24],
    pub timers: &'a mut Timers,
}

impl Io<'_> {
    /// Selects the appropriate I/O register entry and byte index for a given address and passes it
    /// to a callback.
    fn access<T>(
        &mut self,
        addr: Address,
        f: impl FnOnce(&mut Self, &Register, Address) -> T,
    ) -> Result<T, ()> {
        macro_rules! get_lut_entry {
            ($mod_name:ident $(,)?) => {
                (
                    $mod_name::LUT
                        .get(addr.working - $mod_name::BASE_ADDR)
                        .ok_or(()),
                    stringify!($mod_name),
                )
            };
        }

        // To avoid explicitly defining a range for each register group, we can simply use an
        // unbounded range in the positive direction and work backwards; `match`es work in a
        // well-defined order from the first to last pattern.
        let (lut_entry, group_name) = match addr.working {
            // TODO
            post::BASE_ADDR.. => get_lut_entry!(post),
            duart::BASE_ADDR.. => get_lut_entry!(duart),
            atcons::BASE_ADDR.. => get_lut_entry!(atcons),
            spu_ctrl::BASE_ADDR.. => get_lut_entry!(spu_ctrl),
            spu_voice::BASE_ADDR.. => get_lut_entry!(spu_voice),
            mdec::BASE_ADDR.. => get_lut_entry!(mdec),
            gpu::BASE_ADDR.. => get_lut_entry!(gpu),
            cdrom::BASE_ADDR.. => get_lut_entry!(cdrom),
            timers::BASE_ADDR.. => get_lut_entry!(timers),
            dma::BASE_ADDR.. => get_lut_entry!(dma),
            int_ctrl::BASE_ADDR.. => get_lut_entry!(int_ctrl),
            ram_ctrl::BASE_ADDR.. => get_lut_entry!(ram_ctrl),
            perif::BASE_ADDR.. => get_lut_entry!(perif),
            bus_ctrl::BASE_ADDR.. => get_lut_entry!(bus_ctrl),
            _ => (Err(()), ""),
        };

        let Ok(lut_entry) = lut_entry else {
            tracing::debug!(
                "Accessing `cpu_bus.io[{:#06x}]`",
                addr.working,
            );

            return Err(());
        };

        // [`Option::get`] is unnecessary here as LUT entry indices are guaranteed by
        // [`gen_cpu_bus_io`] to point to valid register entries.
        let reg = &REGISTERS[lut_entry.reg_idx];

        tracing::debug!(
            "Accessing `cpu_bus.io.{}.{}[{}]`",
            group_name,
            reg.name,
            lut_entry.byte_idx,
        );

        Ok(f(self, reg, Address::from(lut_entry.byte_idx)))
    }
}

/// Defines a `read_` method in which a byte index is relevant (i.e., `read_8`, `read_16`).
macro_rules! def_read_with_addr {
    ($fn_name:ident() -> $ty:ty) => {
        pub fn $fn_name(&mut self, addr: Address) -> Result<$ty, ()> {
            self.access(addr, |this, reg, addr| (reg.$fn_name)(reg, this, addr))
        }
    };
}

/// Defines a `read_` method in which a byte index is not relevant (i.e., `read_32`).
macro_rules! def_read_without_addr {
    ($fn_name:ident() -> $ty:ty) => {
        pub fn $fn_name(&mut self, addr: Address) -> Result<$ty, ()> {
            self.access(addr, |this, reg, _| (reg.$fn_name)(reg, this))
        }
    };
}

/// Defines a `write_` method in which a byte index is relevant (i.e., `write_8`, `write_16`).
macro_rules! def_write_with_addr {
    ($fn_name:ident($ty:ty)) => {
        pub fn $fn_name(&mut self, addr: Address, value: $ty) -> Result<(), ()> {
            self.access(addr, |this, reg, addr| {
                (reg.$fn_name)(reg, this, addr, value)
            })
        }
    };
}

/// Defines a `write_` method in which a byte index is not relevant (i.e., `write_32`).
macro_rules! def_write_without_addr {
    ($fn_name:ident($ty:ty)) => {
        pub fn $fn_name(&mut self, addr: Address, value: $ty) -> Result<(), ()> {
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
    name: &'static str,
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
    // Bus Control.
    Lut {
        name: bus_ctrl,
        base_addr: 0x0000,
        regs: [
            Register {
                name: EXP_1_BASE,
                read_32: |_, io| {
                    io.bus_ctrl.exp_1_base
                },
                write_32: |_, io, value| {
                    io.bus_ctrl.exp_1_base = value;
                },
            },
            Register {
                name: EXP_2_BASE,
                read_32: |_, io| {
                    io.bus_ctrl.exp_2_base
                },
                write_32: |_, io, value| {
                    io.bus_ctrl.exp_2_base = value;
                },
            },
            Register {
                name: EXP_1_SIZE,
                read_32: |_, io| {
                    io.bus_ctrl.exp_1_size
                },
                write_32: |_, io, value| {
                    io.bus_ctrl.exp_1_size = value;
                },
            },
            Register {
                name: EXP_3_SIZE,
                read_32: |_, io| {
                    io.bus_ctrl.exp_3_size
                },
                write_32: |_, io, value| {
                    io.bus_ctrl.exp_3_size = value;
                },
            },
            Register {
                name: BIOS_SIZE,
                read_32: |_, io| {
                    io.bus_ctrl.bios_size
                },
                write_32: |_, io, value| {
                    io.bus_ctrl.bios_size = value;
                },
            },
            Register {
                name: SPU_SIZE,
                read_32: |_, io| {
                    io.bus_ctrl.spu_size
                },
                write_32: |_, io, value| {
                    io.bus_ctrl.spu_size = value;
                },
            },
            Register {
                name: CDROM_SIZE,
                read_32: |_, io| {
                    io.bus_ctrl.cdrom_size
                },
                write_32: |_, io, value| {
                    io.bus_ctrl.cdrom_size = value;
                },
            },
            Register {
                name: EXP_2_SIZE,
                read_32: |_, io| {
                    io.bus_ctrl.exp_2_size
                },
                write_32: |_, io, value| {
                    io.bus_ctrl.exp_2_size = value;
                },
            },
            Register {
                name: COMMON_SIZE,
                read_32: |_, io| {
                    io.bus_ctrl.common_size
                },
                write_32: |_, io, value| {
                    io.bus_ctrl.common_size = value;
                },
            },
        ],
        module: {
            impl Default for Control {
                fn default() -> Self {
                    Self {
                        exp_1_base: 0,
                        exp_2_base: 0,
                        exp_1_size: 0,
                        exp_2_size: 0,
                        exp_3_size: 0,
                        // This needs to be a decent size.
                        bios_size: 0x2_0000,
                        spu_size: 0,
                        cdrom_size: 0,
                        common_size: 0,
                    }
                }
            }

            #[derive(Debug)]
            pub struct Control {
                pub exp_1_base: u32,
                pub exp_2_base: u32,
                pub exp_1_size: u32,
                pub exp_2_size: u32,
                pub exp_3_size: u32,
                pub bios_size: u32,
                pub spu_size: u32,
                pub cdrom_size: u32,
                pub common_size: u32,
            }
        },
    },
    // Peripheral I/O Ports.
    Lut {
        name: perif,
        base_addr: 0x0040,
        regs: [
            Register {
                name: JOY_DATA,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: JOY_STAT,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: JOY_MODE,
                read_16: |_, io, addr| 0,
                write_16: |_, io, addr, value| {},
            },
            Register {
                name: JOY_CTRL,
                read_16: |_, io, addr| 0,
                write_16: |_, io, addr, value| {},
            },
            Register {
                name: JOY_BAUD,
                read_16: |_, io, addr| 0,
                write_16: |_, io, addr, value| {},
            },
            Register {
                name: SIO_DATA,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: SIO_STAT,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: SIO_MODE,
                read_16: |_, io, addr| 0,
                write_16: |_, io, addr, value| {},
            },
            Register {
                name: SIO_CTRL,
                read_16: |_, io, addr| 0,
                write_16: |_, io, addr, value| {},
            },
            Register {
                name: SIO_MISC,
                read_16: |_, io, addr| 0,
                write_16: |this, i, addr, value| {},
            },
            Register {
                name: SIO_BAUD,
                read_16: |_, io, addr| 0,
                write_16: |_, io, addr, value| {},
            },
        ],
        module: {},
    },
    // Bus Control 2.
    Lut {
        name: ram_ctrl,
        base_addr: 0x0060,
        regs: [
            Register {
                name: RAM_CTRL,
                read_32: |_, io| {
                    io.ram_ctrl.0
                },
                write_32: |_, io, value| {
                    *io.ram_ctrl = ram_ctrl::Control(value);
                },
            },
        ],
        module: {
            impl Default for Control {
                fn default() -> Self {
                    Self(0)
                }
            }

            // TODO
            bitfield::bitfield! {
                pub struct Control(u32);
                impl Debug;
                pub into LayoutKind, layout, set_layout: 11, 9;
            }

            impl From<u32> for LayoutKind {
                fn from(value: u32) -> Self {
                    match value & 0b111 {
                        0 => Self::_1M,
                        1 => Self::_4M,
                        2 => Self::_1MPlus1MHighZ,
                        3 => Self::_4MPlus4MHighZ,
                        4 => Self::_2M,
                        5 | 7 => Self::_8M,
                        6 => Self::_2MPlus2MHighZ,
                        _ => unreachable!(),
                    }
                }
            }

            #[derive(Debug)]
            pub enum LayoutKind {
                _1M,
                _4M,
                _1MPlus1MHighZ,
                _4MPlus4MHighZ,
                _2M,
                _8M,
                _2MPlus2MHighZ,
            }

            impl From<LayoutKind> for Layout {
                fn from(kind: LayoutKind) -> Self {
                    match kind {
                        LayoutKind::_1M             => Self::new(1, 0),
                        LayoutKind::_1MPlus1MHighZ  => Self::new(1, 1),
                        LayoutKind::_2M             => Self::new(2, 0),
                        LayoutKind::_2MPlus2MHighZ  => Self::new(2, 2),
                        LayoutKind::_4M             => Self::new(4, 0),
                        LayoutKind::_4MPlus4MHighZ  => Self::new(4, 4),
                        LayoutKind::_8M             => Self::new(8, 0),

                    }
                }
            }

            impl Layout {
                pub const fn new(data_mb: u32, high_z_mb: u32) -> Self {
                    Self {
                        data_mb,
                        high_z_mb,
                    }
                }
            }

            #[derive(Debug)]
            pub struct Layout {
                pub data_mb: u32,
                pub high_z_mb: u32,
            }
        },
    },
    // Interrupt Control.
    Lut {
        name: int_ctrl,
        base_addr: 0x0070,
        regs: [
            Register {
                name: STAT,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: MASK,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: 2,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: 3,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
        ],
        module: {},
    },
    // DMA.
    Lut {
        name: dma,
        base_addr: 0x0080,
        regs: [
            Register {
                name: MDECin_0,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: MDECin_1,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: MDECin_2,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: MDECin_3,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: MDECout_0,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: MDECout_1,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: MDECout_2,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: MDECout_3,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: GPU_0,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: GPU_1,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: GPU_2,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: GPU_3,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: CDROM_0,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: CDROM_1,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: CDROM_2,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: CDROM_3,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: SPU_0,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: SPU_1,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: SPU_2,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: SPU_3,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: PIO_0,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: PIO_1,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: PIO_2,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: PIO_3,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: OTC_0,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: OTC_1,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: OTC_2,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: OTC_3,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: DPCR,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: DICR,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: 30,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: 31,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
        ],
        module: {},
    },
    // Timers (Root Counters).
    Lut {
        name: timers,
        base_addr: 0x0100,
        regs: [
            Register {
                name: DOTCLOCK_COUNTER,
                read_32: |_, io| {
                    io.timers.dotclock.counter
                },
                write_32: |_, io, value| {
                    io.timers.dotclock.counter = value;
                },
            },
            Register {
                name: DOTCLOCK_MODE,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: DOTCLOCK_TARGET,
                read_32: |_, io| {
                    io.timers.dotclock.target
                },
                write_32: |_, io, value| {
                    io.timers.dotclock.target = value;
                },
            },
            Register {
                name: DOTCLOCK_3,
                read_32: |_, _| 0,
                write_32: |_, _, _| {},
            },
            Register {
                name: H_RETRACE_COUNTER,
                read_32: |_, io| {
                    io.timers.h_retrace.counter
                },
                write_32: |_, io, value| {
                    io.timers.h_retrace.counter = value;
                },
            },
            Register {
                name: H_RETRACE_MODE,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: H_RETRACE_TARGET,
                read_32: |_, io| {
                    io.timers.h_retrace.target
                },
                write_32: |_, io, value| {
                    io.timers.h_retrace.target = value;
                },
            },
            Register {
                name: H_RETRACE_3,
                read_32: |_, _| 0,
                write_32: |_, _, _| {},
            },
            Register {
                name: SYS_CLOCK_COUNTER,
                read_32: |_, io| {
                    io.timers.sys_clock.counter
                },
                write_32: |_, io, value| {
                    io.timers.sys_clock.counter = value;
                },
            },
            Register {
                name: SYS_CLOCK_MODE,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: SYS_CLOCK_TARGET,
                read_32: |_, io| {
                    io.timers.sys_clock.target
                },
                write_32: |_, io, value| {
                    io.timers.sys_clock.target = value;
                },
            },
            Register {
                name: SYS_CLOCK_3,
                read_32: |_, _| 0,
                write_32: |_, _, _| {},
            },
        ],
        module: {
            #[derive(Debug, Default)]
            pub struct Timers {
                pub dotclock: Timer,
                pub h_retrace: Timer,
                pub sys_clock: Timer,
            }

            #[derive(Debug, Default)]
            pub struct Timer {
                pub counter: u32,
                pub target: u32,
            }
        },
    },
    // CD-ROM.
    Lut {
        name: cdrom,
        base_addr: 0x0800,
        regs: [
            Register {
                name: INDEX,
                read_8: |_, io, addr| 0,
                write_8: |_, io, addr, value| {},
            },
            Register {
                name: 1,
                read_8: |_, io, addr| 0,
                write_8: |_, io, addr, value| {},
            },
            Register {
                name: 2,
                read_8: |_, io, addr| 0,
                write_8: |_, io, addr, value| {},
            },
            Register {
                name: 3,
                read_8: |_, io, addr| 0,
                write_8: |_, io, addr, value| {},
            },
        ],
        module: {},
    },
    // GPU.
    Lut {
        name: gpu,
        base_addr: 0x0810,
        regs: [
            Register {
                name: 0,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: 1,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
        ],
        module: {},
    },
    // MDEC.
    Lut {
        name: mdec,
        base_addr: 0x0820,
        regs: [
            Register {
                name: CHAN,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: CTRL,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
        ],
        module: {},
    },
    // SPU Voice.
    Lut {
        name: spu_voice,
        base_addr: 0x0c00,
        regs: [
            Register {
                name: L_VOL,
                read_16: |i: usize, _, io: &Io, _| {
                    io.spu_voices[i].l_vol
                },
                write_16: |i: usize, _, io: &mut Io, _, value| {
                    io.spu_voices[i].l_vol = value;
                },
            },
            Register {
                name: R_VOL,
                read_16: |i: usize, _, io: &Io, _| {
                    io.spu_voices[i].r_vol
                },
                write_16: |i: usize, _, io: &mut Io, _, value| {
                    io.spu_voices[i].r_vol = value;
                },
            },
            Register {
                name: ADPCM_SAMPLE_RATE,
                read_16: |i: usize, _, io: &Io, _| {
                    io.spu_voices[i].adpcm_sample_rate
                },
                write_16: |i: usize, _, io: &mut Io, _, value| {
                    io.spu_voices[i].adpcm_sample_rate = value;
                },
            },
            Register {
                name: ADPCM_BASE_ADDR,
                read_16: |i: usize, _, io: &Io, _| {
                    io.spu_voices[i].adpcm_base_addr
                },
                write_16: |i: usize, _, io: &mut Io, _, value| {
                    io.spu_voices[i].adpcm_base_addr = value;
                },
            },
            Register {
                name: ADSR_CONFIG,
                read_32: |i: usize, _, io: &Io| 0,
                write_32: |i: usize, _, io: &mut Io, value| {},
            },
            Register {
                name: ADSR_CURRENT_VOL,
                read_16: |i: usize, _, io: &Io, _| {
                    io.spu_voices[i].current_vol
                },
                write_16: |i: usize, _, io: &mut Io, _, value| {
                    io.spu_voices[i].current_vol = value;
                },
            },
            Register {
                name: ADSR_REP_ADDR,
                read_16: |i: usize, _, io: &Io, _| {
                    io.spu_voices[i].rep_addr
                },
                write_16: |i: usize, _, io: &mut Io, _, value| {
                    io.spu_voices[i].rep_addr = value;
                },
            },
        ],
        module: {
            #[derive(Debug, Default)]
            pub struct Voice {
                pub l_vol: u16,
                pub r_vol: u16,
                pub adpcm_sample_rate: u16,
                pub adpcm_base_addr: u16,
                pub attack: Attack,
                pub decay: Decay,
                pub sustain: Sustain,
                pub release: Release,
                pub current_vol: u16,
                pub rep_addr: u16,
            }

            #[derive(Debug, Default)]
            pub struct Attack {
                pub mode: Mode,
                pub shift: u32,
            }

            #[derive(Debug, Default)]
            pub struct Decay {
                pub shift: u32,
            }

            #[derive(Debug, Default)]
            pub struct Sustain {
                pub level: u32,
                pub mode: Mode,
                pub dir: Direction,
                pub shift: u32,
                pub step: u32,
            }

            #[derive(Debug, Default)]
            pub struct Release {
                pub mode: Mode,
                pub shift: u32,
            }

            #[derive(Debug, Default)]
            pub enum Direction {
                #[default]
                Increase,
                Decrease,
            }

            #[derive(Debug, Default)]
            pub enum Mode {
                #[default]
                Linear,
                Exponential,
            }
        },
        // There are 24 SPU voices, so we can simply define the register set once and automatically
        // instantiate it 24 times with the following line.
        regs_count: 24,
    },
    // SPU Control.
    Lut {
        name: spu_ctrl,
        base_addr: 0x0d80,
        regs: [
            Register {
                name: L_MAIN_VOL,
                read_16: |_, io, addr| {
                    io.spu_ctrl.l_main_vol
                },
                write_16: |_, io, addr, value| {
                    io.spu_ctrl.l_main_vol = value;
                },
            },
            Register {
                name: R_MAIN_VOL,
                read_16: |_, io, addr| 0,
                write_16: |_, io, addr, value| {},
            },
            Register {
                name: REVERB_VOL,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: KEY_ON,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: KEY_OFF,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: CHAN_FM,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: CHAN_NOISE,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: CHAN_REVERB,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: CHAN_STAT,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: 8,
                read_16: |_, io, addr| 0,
                write_16: |_, io, addr, value| {},
            },
            Register {
                name: REVERB_WRAM_BASE,
                read_16: |_, io, addr| 0,
                write_16: |_, io, addr, value| {},
            },
            Register {
                name: IRQ_ADDR,
                read_16: |_, io, addr| 0,
                write_16: |_, io, addr, value| {},
            },
            Register {
                name: DATA_TXFR_ADDR,
                read_16: |_, io, addr| 0,
                write_16: |_, io, addr, value| {},
            },
            Register {
                name: DATA_TXFR_FIFO,
                read_16: |_, io, addr| 0,
                write_16: |_, io, addr, value| {},
            },
            Register {
                name: CTRL,
                read_16: |_, io, addr| 0,
                write_16: |_, io, addr, value| {},
            },
            Register {
                name: DATA_TXFR_CTRL,
                read_16: |_, io, addr| 0,
                write_16: |_, io, addr, value| {},
            },
            Register {
                name: STAT,
                read_16: |_, io, addr| 0,
                write_16: |_, io, addr, value| {},
            },
            Register {
                name: CD_VOL,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: EXT_VOL,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: CUR_MAIN_VOL,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
            Register {
                name: 19,
                read_32: |_, io| 0,
                write_32: |_, io, value| {},
            },
        ],
        module: {
            #[derive(Debug, Default)]
            pub struct Control {
                pub l_main_vol: u16,
                pub r_main_vol: u16,
            }
        },
    },
    // SPU Reverb Configuration.
    // SPU Internal.
    // ATCONS (?).
    Lut {
        name: atcons,
        base_addr: 0x1000,
        regs: [
            Register {
                name: STAT,
                read_8: |_, io, addr| 0,
                write_8: |_, io, addr, value| {},
            },
            Register {
                name: 1,
                read_8: |_, io, addr| 0,
                write_8: |_, io, addr, value| {},
            },
            Register {
                name: DATA,
                read_8: |_, io, addr| 0,
                write_8: |_, io, addr, value| {},
            },
            Register {
                name: 3,
                read_8: |_, io, addr| 0,
                write_8: |_, io, addr, value| {},
            },
            Register {
                name: 4,
                read_16: |_, io, addr| 0,
                write_16: |_, io, addr, value| {},
            },
        ],
        module: {},
    },
    // Dual Serial Port.
    Lut {
        name: duart,
        base_addr: 0x1020,
        regs: [
            Register {
                name: MODE_0,
                read_8: |_, io, addr| 0,
                write_8: |_, io, addr, value| {},
            },
            Register {
                name: STAT_0,
                read_8: |_, io, addr| 0,
                write_8: |_, io, addr, value| {},
            },
            Register {
                name: 3,
                read_8: |_, io, addr| 0,
                write_8: |_, io, addr, value| {},
            },
            Register {
                name: FIFO_0,
                read_8: |_, io, addr| 0,
                write_8: |_, io, addr, value| {},
            },
            Register {
                name: 4,
                read_8: |_, io, addr| 0,
                write_8: |_, io, addr, value| {},
            },
            // TODO
        ],
        module: {},
    },
    Lut {
        name: post,
        base_addr: 0x1040,
        regs: [
            Register {
                name: BOOT_MODE,
                read_8: |_, io, addr| 0,
                write_8: |_, io, addr, value| {
                    todo!()
                },
            },
            Register {
                name: SEVEN_SEG,
                read_8: |_, io, addr| 0,
                write_8: |_, io, addr, value| {
                    io.post.seven_seg.push(value as u8);
                },
            },
            Register {
                name: LED,
                read_8: |_, io, addr| 0,
                write_8: |_, io, addr, value| {
                    io.post.led.push(value as u8);
                },
            },
        ],
        module: {
            #[derive(Debug, Default)]
            pub struct Post {
                /// A 7-segment display indicating the POST status.
                pub seven_seg: Vec<u8>,
                /// An LED indicating the POST status.
                pub led: Vec<u8>,
            }
        },
    }
);
