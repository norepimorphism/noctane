// SPDX-License-Identifier: MPL-2.0

/// External components accessible to the CPU via I/O registers.

use noctane_gpu::Gpu;
use noctane_proc_macro::gen_cpu_bus_io;

use crate::mem::Address;

pub use bus::Config as BusConfig;
pub use dma::Config as DmaConfig;
pub use post::Status as PostStatus;
pub use ram::Config as RamConfig;
pub use spu::Config as SpuConfig;
pub use spu_voice::Config as SpuVoiceConfig;
pub use timers::{Timer, Timers};

/// External components accessible to the CPU via I/O registers.
///
/// Addresses above or equal to `0x1000` access Expansion Region 2, which is contained within the
/// I/O region.
#[derive(Debug)]
pub struct Io<'a> {
    pub bus: &'a mut BusConfig,
    pub dma: &'a mut DmaConfig,
    /// The graphics processing unit (GPU).
    pub gpu: &'a mut Gpu,
    /// The last result of a GPU command (GP0 or GP1).
    pub last_gpu_result: &'a mut u32,
    pub post: &'a mut PostStatus,
    pub ram: &'a mut RamConfig,
    pub spu: &'a mut SpuConfig,
    pub spu_voices: &'a mut [SpuVoiceConfig; 24],
    pub timers: &'a mut Timers,
}

impl Io<'_> {
    pub fn update(&mut self, cause: &mut crate::reg::cpr::Cause) {
        self.dma.update();
        self.timers.update();

        if self.dma.take_irq().is_some() {
            cause.raise_std_interrupt();
        } else if self.timers.take_irq().is_some() {
            cause.raise_std_interrupt();
        }
    }

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
            post::BASE_ADDR..       => get_lut_entry!(post),
            duart::BASE_ADDR..      => get_lut_entry!(duart),
            atcons::BASE_ADDR..     => get_lut_entry!(atcons),
            spu::BASE_ADDR..   => get_lut_entry!(spu),
            spu_voice::BASE_ADDR..  => get_lut_entry!(spu_voice),
            mdec::BASE_ADDR..       => get_lut_entry!(mdec),
            gpu::BASE_ADDR..        => get_lut_entry!(gpu),
            cdrom::BASE_ADDR..      => get_lut_entry!(cdrom),
            timers::BASE_ADDR..     => get_lut_entry!(timers),
            dma::BASE_ADDR..   => get_lut_entry!(dma),
            int::BASE_ADDR..   => get_lut_entry!(int),
            ram::BASE_ADDR..   => get_lut_entry!(ram),
            perif::BASE_ADDR..      => get_lut_entry!(perif),
            bus::BASE_ADDR..   => get_lut_entry!(bus),
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
    // Bus Configuration.
    Lut {
        name: bus,
        base_addr: 0x0000,
        regs: [
            Register {
                name: EXP_1_BASE,
                read_32: |_, io| {
                    io.bus.exp_1_base
                },
                write_32: |_, io, value| {
                    io.bus.exp_1_base = value;
                },
            },
            Register {
                name: EXP_2_BASE,
                read_32: |_, io| {
                    // The bottom 24 bits show the base address, but the top byte is hardcoded to
                    // `0x1f`.
                    // TODO: This isn't synced with the bank selection code in [`bus`].
                    (io.bus.exp_2_base & ((1 << 25) - 1)) | (0x1f << 24)
                },
                write_32: |_, io, value| {
                    io.bus.exp_2_base = value;
                },
            },
            Register {
                name: EXP_1_SIZE,
                read_32: |_, io| {
                    io.bus.exp_1_size
                },
                write_32: |_, io, value| {
                    io.bus.exp_1_size = value;
                },
            },
            Register {
                name: EXP_3_SIZE,
                read_32: |_, io| {
                    io.bus.exp_3_size
                },
                write_32: |_, io, value| {
                    io.bus.exp_3_size = value;
                },
            },
            Register {
                name: BIOS_SIZE,
                read_32: |_, io| {
                    io.bus.bios_size
                },
                write_32: |_, io, value| {
                    io.bus.bios_size = value;
                },
            },
            Register {
                name: SPU_SIZE,
                read_32: |_, io| {
                    io.bus.spu_size
                },
                write_32: |_, io, value| {
                    io.bus.spu_size = value;
                },
            },
            Register {
                name: CDROM_SIZE,
                read_32: |_, io| {
                    io.bus.cdrom_size
                },
                write_32: |_, io, value| {
                    io.bus.cdrom_size = value;
                },
            },
            Register {
                name: EXP_2_SIZE,
                read_32: |_, io| {
                    io.bus.exp_2_size
                },
                write_32: |_, io, value| {
                    io.bus.exp_2_size = value;
                },
            },
            Register {
                name: COMMON_SIZE,
                read_32: |_, io| {
                    io.bus.common_size
                },
                write_32: |_, io, value| {
                    io.bus.common_size = value;
                },
            },
        ],
        module: {
            impl Default for Config {
                fn default() -> Self {
                    Self {
                        // Most of these fields can be 0 because the BIOS confgures them.
                        exp_1_base: 0,
                        exp_2_base: 0,
                        exp_1_size: 0,
                        exp_2_size: 0,
                        exp_3_size: 0,
                        // However, the BIOS size must be large enough on boot to at least fit the
                        // code to configure the other bank bases/sizes. We'll give it `0x800`.
                        bios_size: 0x800,
                        spu_size: 0,
                        cdrom_size: 0,
                        common_size: 0,
                    }
                }
            }

            /// Determines the base addresses and sizes of banks accessible to the CPU bus.
            #[derive(Debug)]
            pub struct Config {
                /// The base address of Expansion Region 1.
                pub exp_1_base: u32,
                /// The base address of Expansion Region 2.
                pub exp_2_base: u32,
                /// The size, in bytes, of Expansion Region 1.
                pub exp_1_size: u32,
                /// The size, in bytes, of Expansion Region 2.
                pub exp_2_size: u32,
                /// The size, in bytes, of Expansion Region 3.
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
    // RAM Configuration.
    Lut {
        name: ram,
        base_addr: 0x0060,
        regs: [
            Register {
                name: RAM_CTRL,
                read_32: |_, io| {
                    io.ram.0
                },
                write_32: |_, io, value| {
                    *io.ram = ram::Config(value);
                },
            },
        ],
        module: {
            impl Default for Config {
                fn default() -> Self {
                    Self(0)
                }
            }

            // TODO
            bitfield::bitfield! {
                pub struct Config(u32);
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
    // Interrupt Configuration.
    Lut {
        name: int,
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
    // DMA Configuration.
    Lut {
        name: dma,
        base_addr: 0x0080,
        regs: [
            Register {
                name: MDECin_MADR,
                read_32: |_, io| {
                    dma::read_chan_field!(io, mdec_in.base_addr)
                },
                write_32: |_, io, value| {
                    dma::write_chan_field!(io, mdec_in.base_addr = value);
                },
            },
            Register {
                name: MDECin_BCR,
                read_32: |_, io| {
                    dma::read_chan_field!(io, mdec_in.block_cfg)
                },
                write_32: |_, io, value| {
                    dma::write_chan_field!(io, mdec_in.block_cfg = value);
                },
            },
            Register {
                name: MDECin_CHCR,
                read_32: |_, io| {
                    dma::read_chan_field!(io, mdec_in.cfg)
                },
                write_32: |_, io, value| {
                    dma::write_chan_field!(io, mdec_in.cfg = value);
                },
            },
            Register {
                name: MDECin_4,
                read_32: |_, io| {
                    todo!()
                },
                write_32: |_, io, value| {
                    todo!()
                },
            },
            Register {
                name: MDECout_MADR,
                read_32: |_, io| {
                    dma::read_chan_field!(io, mdec_out.base_addr)
                },
                write_32: |_, io, value| {
                    dma::write_chan_field!(io, mdec_out.base_addr = value);
                },
            },
            Register {
                name: MDECout_BCR,
                read_32: |_, io| {
                    dma::read_chan_field!(io, mdec_out.block_cfg)
                },
                write_32: |_, io, value| {
                    dma::write_chan_field!(io, mdec_out.block_cfg = value);
                },
            },
            Register {
                name: MDECout_CHCR,
                read_32: |_, io| {
                    dma::read_chan_field!(io, mdec_out.cfg)
                },
                write_32: |_, io, value| {
                    dma::write_chan_field!(io, mdec_out.cfg = value);
                },
            },
            Register {
                name: MDECout_4,
                read_32: |_, io| {
                    todo!()
                },
                write_32: |_, io, value| {
                    todo!()
                },
            },
            Register {
                name: GPU_MADR,
                read_32: |_, io| {
                    dma::read_chan_field!(io, gpu.base_addr)
                },
                write_32: |_, io, value| {
                    dma::write_chan_field!(io, gpu.base_addr = value);
                },
            },
            Register {
                name: GPU_BCR,
                read_32: |_, io| {
                    dma::read_chan_field!(io, gpu.block_cfg)
                },
                write_32: |_, io, value| {
                    dma::write_chan_field!(io, gpu.block_cfg = value);
                },
            },
            Register {
                name: GPU_CHCR,
                read_32: |_, io| {
                    dma::read_chan_field!(io, gpu.cfg)
                },
                write_32: |_, io, value| {
                    dma::write_chan_field!(io, gpu.cfg = value);
                },
            },
            Register {
                name: GPU_4,
                read_32: |_, io| {
                    todo!()
                },
                write_32: |_, io, value| {
                    todo!()
                },
            },
            Register {
                name: CDROM_MADR,
                read_32: |_, io| {
                    dma::read_chan_field!(io, cdrom.base_addr)
                },
                write_32: |_, io, value| {
                    dma::write_chan_field!(io, cdrom.base_addr = value);
                },
            },
            Register {
                name: CDROM_BCR,
                read_32: |_, io| {
                    dma::read_chan_field!(io, cdrom.block_cfg)
                },
                write_32: |_, io, value| {
                    dma::write_chan_field!(io, cdrom.block_cfg = value);
                },
            },
            Register {
                name: CDROM_CHCR,
                read_32: |_, io| {
                    dma::read_chan_field!(io, cdrom.cfg)
                },
                write_32: |_, io, value| {
                    dma::write_chan_field!(io, cdrom.cfg = value);
                },
            },
            Register {
                name: CDROM_4,
                read_32: |_, io| {
                    todo!()
                },
                write_32: |_, io, value| {
                    todo!()
                },
            },
            Register {
                name: SPU_MADR,
                read_32: |_, io| {
                    dma::read_chan_field!(io, spu.base_addr)
                },
                write_32: |_, io, value| {
                    dma::write_chan_field!(io, spu.base_addr = value);
                },
            },
            Register {
                name: SPU_BCR,
                read_32: |_, io| {
                    dma::read_chan_field!(io, spu.block_cfg)
                },
                write_32: |_, io, value| {
                    dma::write_chan_field!(io, spu.block_cfg = value);
                },
            },
            Register {
                name: SPU_CHCR,
                read_32: |_, io| {
                    dma::read_chan_field!(io, spu.cfg)
                },
                write_32: |_, io, value| {
                    dma::write_chan_field!(io, spu.cfg = value);
                },
            },
            Register {
                name: SPU_4,
                read_32: |_, io| {
                    todo!()
                },
                write_32: |_, io, value| {
                    todo!()
                },
            },
            Register {
                name: PIO_MADR,
                read_32: |_, io| {
                    dma::read_chan_field!(io, pio.base_addr)
                },
                write_32: |_, io, value| {
                    dma::write_chan_field!(io, pio.base_addr = value);
                },
            },
            Register {
                name: PIO_BCR,
                read_32: |_, io| {
                    dma::read_chan_field!(io, pio.block_cfg)
                },
                write_32: |_, io, value| {
                    dma::write_chan_field!(io, pio.block_cfg = value);
                },
            },
            Register {
                name: PIO_CHCR,
                read_32: |_, io| {
                    dma::read_chan_field!(io, pio.cfg)
                },
                write_32: |_, io, value| {
                    dma::write_chan_field!(io, pio.cfg = value);
                },
            },
            Register {
                name: PIO_4,
                read_32: |_, io| {
                    todo!()
                },
                write_32: |_, io, value| {
                    todo!()
                },
            },
            Register {
                name: OTC_MADR,
                read_32: |_, io| {
                    dma::read_chan_field!(io, otc.base_addr)
                },
                write_32: |_, io, value| {
                    dma::write_chan_field!(io, otc.base_addr = value);
                },
            },
            Register {
                name: OTC_BCR,
                read_32: |_, io| {
                    dma::read_chan_field!(io, otc.block_cfg)
                },
                write_32: |_, io, value| {
                    dma::write_chan_field!(io, otc.block_cfg = value);
                },
            },
            Register {
                name: OTC_CHCR,
                read_32: |_, io| {
                    dma::read_chan_field!(io, otc.cfg)
                },
                write_32: |_, io, value| {
                    dma::write_chan_field!(io, otc.cfg = value);
                },
            },
            Register {
                name: OTC_4,
                read_32: |_, io| {
                    todo!()
                },
                write_32: |_, io, value| {
                    todo!()
                },
            },
            Register {
                name: DPCR,
                read_32: |_, io| {
                    io.dma.chan.cfg.0
                },
                write_32: |_, io, value| {
                    io.dma.chan.cfg = dma::ChannelsConfig(value);
                },
            },
            Register {
                name: DICR,
                read_32: |_, io| {
                    io.dma.int.0
                },
                write_32: |_, io, value| {
                    io.dma.int = dma::InterruptConfig(value);
                },
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
        module: {
            use bitfield::bitfield;

            macro_rules! read_chan_field {
                ($io:expr, $chan:ident . cfg) => {
                    $io.dma.chan.$chan.cfg.0
                };
                ($io:expr, $chan:ident . $field:ident) => {
                    $io.dma.chan.$chan.$field
                };
            }

            macro_rules! write_chan_field {
                ($io:expr, $chan:ident . cfg = $value:expr) => {
                    $io.dma.chan.$chan.cfg = dma::ChannelConfig($value)
                };
                ($io:expr, $chan:ident . $field:ident = $value:expr) => {
                    $io.dma.chan.$chan.$field = $value
                };
            }

            pub(crate) use read_chan_field;
            pub(crate) use write_chan_field;

            #[derive(Debug, Default)]
            pub struct Config {
                pub int: InterruptConfig,
                pub chan: Channels,
            }

            impl Config {
                pub fn take_irq(&mut self) -> Option<()> {
                    // TODO
                    if self.int.gpu_irq_is_pending() {
                        self.int.set_gpu_irq_is_pending(false);
                        // Some(())
                        None
                    } else {
                        None
                    }
                }

                pub fn update(&mut self) {
                    // TODO
                }
            }

            impl Default for InterruptConfig {
                fn default() -> Self {
                    Self(0)
                }
            }

            bitfield! {
                pub struct InterruptConfig(u32);
                impl Debug;
                pub mdec_in_irq_is_enabled, set_mdec_in_irq_is_enabled: 16;
                pub mdec_out_irq_is_enabled, set_mdec_out_irq_is_enabled: 17;
                pub gpu_irq_is_enabled, set_gpu_irq_is_enabled: 18;
                pub cdrom_irq_is_enabled, set_cdrom_irq_is_enabled: 19;
                pub spu_irq_is_enabled, set_spu_irq_is_enabled: 20;
                pub pio_irq_is_enabled, set_pio_irq_is_enabled: 21;
                pub otc_irq_is_enabled, set_otc_irq_is_enabled: 22;
                pub irq_is_enabled, set_irq_is_enabled: 23;
                pub mdec_in_irq_is_pending, set_mdec_in_irq_is_pending: 24;
                pub mdec_out_irq_is_pending, set_mdec_out_irq_is_pending: 25;
                pub gpu_irq_is_pending, set_gpu_irq_is_pending: 26;
                pub cdrom_irq_is_pending, set_cdrom_irq_is_pending: 27;
                pub spu_irq_is_pending, set_spu_irq_is_pending: 28;
                pub pio_irq_is_pending, set_pio_irq_is_pending: 29;
                pub otc_irq_is_pending, set_otc_irq_is_pending: 30;
                pub irq_is_pending, set_irq_is_pending: 31;
            }

            #[derive(Debug, Default)]
            pub struct Channels {
                pub cfg: ChannelsConfig,
                pub mdec_in: Channel,
                pub mdec_out: Channel,
                pub gpu: Channel,
                pub cdrom: Channel,
                pub spu: Channel,
                pub pio: Channel,
                pub otc: Channel,
            }

            impl Default for ChannelsConfig {
                fn default() -> Self {
                    Self(0)
                }
            }

            bitfield! {
                pub struct ChannelsConfig(u32);
                impl Debug;
                pub mdec_in_prio, set_mdec_in_prio: 2, 0;
                pub mdec_in_is_enabled, set_mdec_in_is_enabled: 3;
                pub mdec_out_prio, set_mdec_out_prio: 6, 4;
                pub mdec_out_is_enabled, set_mdec_out_is_enabled: 7;
                pub gpu_prio, set_gpu_prio: 10, 8;
                pub gpu_is_enabled, set_gpu_is_enabled: 11;
                pub cdrom_prio, set_cdrom_prio: 14, 12;
                pub cdrom_is_enabled, set_cdrom_is_enabled: 15;
                pub spu_prio, set_spu_prio: 18, 16;
                pub spu_is_enabled, set_spu_is_enabled: 19;
                pub pio_prio, set_pio_prio: 22, 20;
                pub pio_is_enabled, set_pio_is_enabled: 23;
                pub otc_prio, set_otc_prio: 26, 24;
                pub otc_is_enabled, set_otc_is_enabled: 27;
            }

            #[derive(Debug, Default)]
            pub struct Channel {
                pub cfg: ChannelConfig,
                pub base_addr: u32,
                pub block_cfg: u32,
            }

            impl Default for ChannelConfig {
                fn default() -> Self {
                    Self(0)
                }
            }

            bitfield! {
                pub struct ChannelConfig(u32);
                impl Debug;
            }

            macro_rules! impl_deref_for_chan {
                ($ty:ty) => {
                    impl std::ops::Deref for $ty {
                        type Target = super::Channel;

                        fn deref(&self) -> &Self::Target {
                            &self.inner
                        }
                    }

                    impl std::ops::DerefMut for $ty {
                        fn deref_mut(&mut self) -> &mut Self::Target {
                            &mut self.inner
                        }
                    }
                };
            }

            use impl_deref_for_chan;

            pub mod sm0 {
                //! Sync mode 0.

                #[derive(Debug)]
                pub struct Channel {
                    inner: super::Channel,
                    pub word_count: u16,
                }

                super::impl_deref_for_chan!(Channel);
            }

            pub mod sm1 {
                //! Sync mode 1.

                #[derive(Debug)]
                pub struct Channel {
                    inner: super::Channel,
                    pub block_size: u16,
                    pub block_count: u16,
                }

                super::impl_deref_for_chan!(Channel);
            }

            pub mod sm2 {
                //! Sync mode 2.

                #[derive(Debug)]
                pub struct Channel {
                    inner: super::Channel,
                }

                super::impl_deref_for_chan!(Channel);
            }
        },
    },
    // Timers (Root Counters).
    Lut {
        name: timers,
        base_addr: 0x0100,
        regs: [
            Register {
                name: DOTCLOCK_COUNTER,
                read_32: |_, io| {
                    timers::read_field!(io, dotclock.counter)
                },
                write_32: |_, io, value| {
                    timers::write_field!(io, dotclock.counter = value);
                },
            },
            Register {
                name: DOTCLOCK_MODE,
                read_32: |_, io| {
                    timers::read_field!(io, dotclock.mode)
                },
                write_32: |_, io, value| {
                    timers::write_field!(io, dotclock.mode = value);
                },
            },
            Register {
                name: DOTCLOCK_TARGET,
                read_32: |_, io| {
                    timers::read_field!(io, dotclock.target)
                },
                write_32: |_, io, value| {
                    timers::write_field!(io, dotclock.target = value);
                },
            },
            Register {
                name: DOTCLOCK_3,
                read_32: |_, _| {
                    todo!()
                },
                write_32: |_, _, _| {
                    todo!()
                },
            },
            Register {
                name: HBLANK_COUNTER,
                read_32: |_, io| {
                    timers::read_field!(io, hblank.counter)
                },
                write_32: |_, io, value| {
                    timers::write_field!(io, hblank.counter = value);
                },
            },
            Register {
                name: HBLANK_MODE,
                read_32: |_, io| {
                    timers::read_field!(io, hblank.mode)
                },
                write_32: |_, io, value| {
                    timers::write_field!(io, hblank.mode = value);
                },
            },
            Register {
                name: HBLANK_TARGET,
                read_32: |_, io| {
                    timers::read_field!(io, hblank.target)
                },
                write_32: |_, io, value| {
                    timers::write_field!(io, hblank.target = value);
                },
            },
            Register {
                name: HBLANK_3,
                read_32: |_, _| {
                    todo!()
                },
                write_32: |_, _, _| {
                    todo!()
                },
            },
            Register {
                name: SYSCLOCK_COUNTER,
                read_32: |_, io| {
                    timers::read_field!(io, sysclock.counter)
                },
                write_32: |_, io, value| {
                    timers::write_field!(io, sysclock.counter = value);
                },
            },
            Register {
                name: SYSCLOCK_MODE,
                read_32: |_, io| {
                    timers::read_field!(io, sysclock.mode)
                },
                write_32: |_, io, value| {
                    timers::write_field!(io, sysclock.mode = value);
                },
            },
            Register {
                name: SYSCLOCK_TARGET,
                read_32: |_, io| {
                    timers::read_field!(io, sysclock.target)
                },
                write_32: |_, io, value| {
                    timers::write_field!(io, sysclock.target = value);
                },
            },
            Register {
                name: SYSCLOCK_3,
                read_32: |_, _| {
                    todo!()
                },
                write_32: |_, _, _| {
                    todo!()
                },
            },
        ],
        module: {
            macro_rules! read_field {
                ($io:expr, $timer:ident . mode) => {
                    $io.timers.$timer.mode.0 as u32
                };
                ($io:expr, $timer:ident . $field:ident) => {
                    $io.timers.$timer.$field as u32
                };
            }

            macro_rules! write_field {
                ($io:expr, $timer:ident . mode = $value:expr) => {
                    $io.timers.$timer.mode = timers::Mode($value as u16);
                };
                ($io:expr, $timer:ident . $field:ident = $value:expr) => {
                    $io.timers.$timer.$field = $value as u16;
                };
            }

            pub(crate) use read_field;
            pub(crate) use write_field;

            #[derive(Debug, Default)]
            pub struct Timers {
                pub dotclock: Timer,
                pub hblank: Timer,
                pub sysclock: Timer,
            }

            impl Timers {
                pub fn take_irq(&mut self) -> Option<()> {
                    self.dotclock.take_irq()?;
                    self.hblank.take_irq()?;
                    self.sysclock.take_irq()?;

                    None
                }

                pub fn update(&mut self) {
                    self.dotclock.update(|clock_src| {
                        match clock_src.0 & 0b11 {
                            0 | 2 => Self::get_sysclock_increment(),
                            1 | 3 => Self::get_dotclock_increment(),
                            _ => unreachable!(),
                        }
                    });
                    self.hblank.update(|clock_src| {
                        match clock_src.0 & 0b11 {
                            0 | 2 => Self::get_sysclock_increment(),
                            1 | 3 => Self::get_hblank_increment(),
                            _ => unreachable!(),
                        }
                    });
                    self.sysclock.update(|clock_src| {
                        let inc = Self::get_sysclock_increment();

                        match clock_src.0 & 0b11 {
                            0 | 1 => inc,
                            2 | 3 => inc / 8,
                            _ => unreachable!(),
                        }
                    });
                }

                fn get_sysclock_increment() -> u16 {
                    // TODO
                    8
                }

                fn get_dotclock_increment() -> u16 {
                    // TODO
                    4
                }

                fn get_hblank_increment() -> u16 {
                    // TODO
                    1
                }
            }

            #[derive(Debug, Default)]
            pub struct Timer {
                // Timer fields are supposed to be 32-bit but contain 16 bits of garbage, so they
                // are represented here with [`u16`]. Accordingly, the [`read_field`] and
                // [`write_field`] macros above perform the appropriate conversions between [`u32`]
                // and [`u16`].

                pub mode: Mode,
                pub counter: u16,
                pub target: u16,
            }

            impl Default for Mode {
                fn default() -> Self {
                    Self(0)
                }
            }

            bitfield::bitfield! {
                pub struct Mode(u16);
                impl Debug;
                pub should_sync, set_should_sync: 0;
                pub sync_mode, set_sync_mode: 2, 1;
                pub use_target, set_use_target: 3;
                pub irqs_on_target_hit, set_irqs_on_target_hit: 4;
                pub irqs_on_overflow, set_irqs_on_overflow: 5;
                pub into ClockSource, clock_src, set_clock_src: 9, 8;
                pub irq_is_pending, set_irq_is_pending: 10;
                // TODO: Implement remaining fields.
            }

            impl From<u16> for ClockSource {
                fn from(value: u16) -> Self {
                    Self(value)
                }
            }

            #[derive(Debug)]
            pub struct ClockSource(u16);

            impl From<ClockSource> for u16 {
                fn from(src: ClockSource) -> Self {
                    src.0
                }
            }

            impl Timer {
                fn take_irq(&mut self) -> Option<()> {
                    if self.mode.irq_is_pending() {
                        self.mode.set_irq_is_pending(false);

                        Some(())
                    } else {
                        None
                    }
                }

                fn update(&mut self, calc_increment_amount: impl Fn(ClockSource) -> u16) {
                    let hit_target = self.mode.use_target() && (self.counter >= self.target);
                    let counter_overflowed = !hit_target && (self.counter == !0);

                    // Update the counter.
                    if hit_target {
                        self.counter = 0
                    } else {
                        let increment_amount = calc_increment_amount(self.mode.clock_src());
                        self.counter = self.counter.wrapping_add(increment_amount);
                    }

                    // Request an interrupt, if necessary.
                    if (self.mode.irqs_on_target_hit() && hit_target) ||
                       (self.mode.irqs_on_overflow() && counter_overflowed)
                    {
                        self.mode.set_irq_is_pending(true);
                    }
                }
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
                read_32: |_, io| {
                    *io.last_gpu_result
                },
                write_32: |_, io, mach| {
                    io.gpu.queue_gp0_machine_command(mach);
                },
            },
            Register {
                name: 1,
                read_32: |_, io| {
                    // TODO
                    0
                },
                write_32: |_, io, mach| {
                    io.gpu.queue_gp1_machine_command(mach);
                },
            },
        ],
        module: {
            bitfield::bitfield! {
                pub struct Status(u32);
                impl Debug;
            }
        },
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
            pub struct Config {
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
    // SPU Configuration.
    Lut {
        name: spu,
        base_addr: 0x0d80,
        regs: [
            Register {
                name: L_MAIN_VOL,
                read_16: |_, io, addr| {
                    io.spu.l_main_vol
                },
                write_16: |_, io, addr, value| {
                    io.spu.l_main_vol = value;
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
            pub struct Config {
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
            pub struct Status {
                /// A 7-segment display indicating the POST status.
                pub seven_seg: Vec<u8>,
                /// An LED indicating the POST status.
                pub led: Vec<u8>,
            }
        },
    }
);
