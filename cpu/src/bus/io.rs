// SPDX-License-Identifier: MPL-2.0

/// External components accessible to the CPU via I/O registers.

use noctane_gpu::Gpu;
use noctane_proc_macro::gen_cpu_bus_io;

use crate::mem::Address;

pub use timers::{Timer, Timers};

/// External components accessible to the CPU via I/O registers.
///
/// Addresses above or equal to `0x1000` access Expansion Region 2, which is contained within the
/// I/O region.
#[derive(Debug)]
pub struct Io<'a> {
    /// The configuration for the CPU bus, except for [main RAM](ram::Config).
    pub bus: &'a mut bus::Config,
    /// The configuration for Direct Memory Access (DMA) channels and transfers.
    pub dma: &'a mut dma::Config,
    /// The Graphics Processing Unit (GPU).
    pub gpu: &'a mut Gpu,
    pub int: &'a mut int::Sources,
    /// The last result of a GPU command (GP0 or GP1).
    pub last_gpu_result: &'a mut u32,
    pub post: &'a mut post::Status,
    /// The configuration for main RAM.
    pub ram: &'a mut ram::Config,
    /// The configuration for the Sound Processing Unit (SPU).
    pub spu: &'a mut spu::Config,
    pub spu_voices: &'a mut [spu_voice::Config; 24],
    pub timers: &'a mut Timers,
}

/// The result of [`Io::update`].
pub struct Update {
    /// Whether or not an interrupt was requested.
    pub requests_interrupt: bool,
    pub dma_txfer_packet: Option<dma::TransferPacket>,
}

impl Io<'_> {
    pub fn update(&mut self) -> Update {
        let dma_txfer_packet = self.dma.update();
        self.timers.update(&mut self.int);

        // Boolean operators are lazy, so `take_irq` is called a maximum of one time.
        let requests_interrupt = self.dma.take_irq().is_some() || self.timers.take_irq().is_some();

        Update {
            requests_interrupt,
            dma_txfer_packet,
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
            spu::BASE_ADDR..        => get_lut_entry!(spu),
            spu_voice::BASE_ADDR..  => get_lut_entry!(spu_voice),
            mdec::BASE_ADDR..       => get_lut_entry!(mdec),
            gpu::BASE_ADDR..        => get_lut_entry!(gpu),
            cdrom::BASE_ADDR..      => get_lut_entry!(cdrom),
            timers::BASE_ADDR..     => get_lut_entry!(timers),
            dma::BASE_ADDR..        => get_lut_entry!(dma),
            int::BASE_ADDR..        => get_lut_entry!(int),
            ram::BASE_ADDR..        => get_lut_entry!(ram),
            perif::BASE_ADDR..      => get_lut_entry!(perif),
            bus::BASE_ADDR..        => get_lut_entry!(bus),
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
                    io.bus.exp_1.base
                },
                write_32: |_, io, value| {
                    io.bus.exp_1.base = value;
                },
            },
            Register {
                name: EXP_2_BASE,
                read_32: |_, io| {
                    io.bus.exp_2.base
                },
                write_32: |_, io, value| {
                    // The top byte is hardcoded to `0x1f`.
                    io.bus.exp_2.base = (0x1f << 24) | (value & ((1 << 25) - 1));
                },
            },
            Register {
                name: EXP_1_CFG,
                read_32: |_, io| {
                    io.bus.exp_1.inner.encode()
                },
                write_32: |_, io, value| {
                    io.bus.exp_1.inner = bus::BankConfig::decode(value);
                },
            },
            Register {
                name: EXP_3_CFG,
                read_32: |_, io| {
                    io.bus.exp_3.encode()
                },
                write_32: |_, io, value| {
                    io.bus.exp_3 = bus::BankConfig::decode(value);
                },
            },
            Register {
                name: BIOS_CFG,
                read_32: |_, io| {
                    io.bus.bios.encode()
                },
                write_32: |_, io, value| {
                    io.bus.bios = bus::BankConfig::decode(value);
                },
            },
            Register {
                name: SPU_CFG,
                read_32: |_, io| {
                    io.bus.spu.encode()
                },
                write_32: |_, io, value| {
                    io.bus.spu = bus::BankConfig::decode(value);
                },
            },
            Register {
                name: CDROM_CFG,
                read_32: |_, io| {
                    io.bus.cdrom.encode()
                },
                write_32: |_, io, value| {
                    io.bus.cdrom = bus::BankConfig::decode(value);
                },
            },
            Register {
                name: EXP_2_CFG,
                read_32: |_, io| {
                    io.bus.exp_2.inner.encode()
                },
                write_32: |_, io, value| {
                    io.bus.exp_2.inner = bus::BankConfig::decode(value);
                },
            },
            Register {
                name: COMMON_CFG,
                read_32: |_, io| {
                    io.bus.common.encode()
                },
                write_32: |_, io, value| {
                    io.bus.common = bus::BankConfig::decode(value);
                },
            },
        ],
        module: {
            use std::ops::{Deref, DerefMut};

            use noctane_util::{BitStack as _, BitStackExt as _};

            impl Default for Config {
                fn default() -> Self {
                    Self {
                        exp_1: RebaseableBankConfig::default(),
                        exp_2: RebaseableBankConfig::default(),
                        exp_3: BankConfig::default(),
                        bios: BankConfig::bios(),
                        spu: BankConfig::default(),
                        cdrom: BankConfig::default(),
                        common: BankConfig::default(),
                    }
                }
            }

            /// Determines the base addresses and sizes of banks accessible to the CPU bus.
            #[derive(Clone, Debug)]
            pub struct Config {
                pub exp_1: RebaseableBankConfig,
                pub exp_2: RebaseableBankConfig,
                pub exp_3: BankConfig,
                pub bios: BankConfig,
                pub spu: BankConfig,
                pub cdrom: BankConfig,
                pub common: BankConfig,
            }

            #[derive(Clone, Debug, Default)]
            pub struct RebaseableBankConfig {
                pub(super) inner: BankConfig,
                pub base: u32,
            }

            impl Deref for RebaseableBankConfig {
                type Target = BankConfig;

                fn deref(&self) -> &Self::Target {
                    &self.inner
                }
            }

            impl DerefMut for RebaseableBankConfig {
                fn deref_mut(&mut self) -> &mut Self::Target {
                    &mut self.inner
                }
            }

            impl BankConfig {
                pub fn decode(code: u32) -> Self {
                    Self {
                        write_delay: code.pop_bits(4).wrapping_add(1),
                        read_delay: code.pop_bits(4).wrapping_add(1),
                        periods: PeriodsConfig::decode(code.pop_bits(4)),
                        bit_width: BitWidth::decode(code.pop_bits(1)),
                        should_auto_inc: code.pop_bool(),
                        unk_14: code.pop_bits(2),
                        size_shift: code.pop_bits(5),
                        unk_21: code.pop_bits(3),
                    }
                }

                pub fn bios() -> Self {
                    Self {
                        // The BIOS size must be large enough on boot to at least accommodate the
                        // code that configures the other banks.
                        size_shift: 4,
                        ..Default::default()
                    }
                }
            }

            #[derive(Clone, Debug, Default)]
            pub struct BankConfig {
                pub write_delay: u32,
                pub read_delay: u32,
                pub periods: PeriodsConfig,
                pub bit_width: BitWidth,
                pub should_auto_inc: bool,
                pub unk_14: u32,
                pub size_shift: u32,
                pub unk_21: u32,
                // TODO
            }

            impl BankConfig {
                pub fn encode(&self) -> u32 {
                    let mut code = 0;
                    code.push_bits(3, self.unk_21);
                    code.push_bits(5, self.size_shift);
                    code.push_bits(2, self.unk_14);
                    code.push_bool(self.should_auto_inc);
                    code.push_bits(1, self.bit_width.encode());
                    code.push_bits(4, self.periods.encode());
                    code.push_bits(4, self.read_delay);
                    code.push_bits(4, self.write_delay);

                    code
                }
            }

            impl PeriodsConfig {
                pub fn decode(mut code: u32) -> Self {
                    Self {
                        recovery_is_enabled:    code.pop_bool(),
                        hold_is_enabled:        code.pop_bool(),
                        floating_is_enabled:    code.pop_bool(),
                        pre_strobe_is_enabled:  code.pop_bool(),
                    }
                }
            }

            #[derive(Clone, Copy, Debug, Default)]
            pub struct PeriodsConfig {
                pub recovery_is_enabled: bool,
                pub hold_is_enabled: bool,
                pub floating_is_enabled: bool,
                pub pre_strobe_is_enabled: bool,
            }

            impl PeriodsConfig {
                pub fn encode(self) -> u32 {
                    let mut code = 0;
                    code.push_bool(self.pre_strobe_is_enabled);
                    code.push_bool(self.floating_is_enabled);
                    code.push_bool(self.hold_is_enabled);
                    code.push_bool(self.recovery_is_enabled);

                    code
                }
            }

            impl BitWidth {
                ///
                ///
                /// # Panics
                ///
                /// This function panics if `code` is greater than 1.
                pub fn decode(code: u32) -> Self {
                    match code {
                        0 => Self::Eight,
                        1 => Self::Sixteen,
                        _ => unreachable!(),
                    }
                }
            }

            #[derive(Clone, Copy, Debug, Default)]
            pub enum BitWidth {
                #[default]
                Eight,
                Sixteen,
            }

            impl BitWidth {
                pub fn encode(self) -> u32 {
                    match self {
                        Self::Eight     => 0,
                        Self::Sixteen   => 1,
                    }
                }
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
                read_32: |_, io| {
                    todo!()
                },
                write_32: |_, io, value| {
                    todo!()
                },
            },
            Register {
                name: JOY_STAT,
                read_32: |_, io| {
                    todo!()
                },
                write_32: |_, io, value| {
                    todo!()
                },
            },
            Register {
                name: JOY_MODE,
                read_16: |_, io, addr| {
                    todo!()
                },
                write_16: |_, io, addr, value| {
                    todo!()
                },
            },
            Register {
                name: JOY_CFG,
                read_16: |_, io, addr| {
                    todo!()
                },
                write_16: |_, io, addr, value| {
                    todo!()
                },
            },
            Register {
                name: JOY_BAUD,
                read_16: |_, io, addr| {
                    todo!()
                },
                write_16: |_, io, addr, value| {
                    todo!()
                },
            },
            Register {
                name: SIO_DATA,
                read_32: |_, io| {
                    todo!()
                },
                write_32: |_, io, value| {
                    todo!()
                },
            },
            Register {
                name: SIO_STAT,
                read_32: |_, io| {
                    todo!()
                },
                write_32: |_, io, value| {
                    todo!()
                },
            },
            Register {
                name: SIO_MODE,
                read_16: |_, io, addr| {
                    todo!()
                },
                write_16: |_, io, addr, value| {
                    todo!()
                },
            },
            Register {
                name: SIO_CFG,
                read_16: |_, io, addr| {
                    todo!()
                },
                write_16: |_, io, addr, value| {
                    todo!()
                },
            },
            Register {
                name: SIO_MISC,
                read_16: |_, io, addr| {
                    todo!()
                },
                write_16: |this, i, addr, value| {
                    todo!()
                },
            },
            Register {
                name: SIO_BAUD,
                read_16: |_, io, addr| 0,
                write_16: |_, io, addr, value| {
                    todo!()
                },
            },
        ],
        module: {
            // TODO
        },
    },
    // RAM Configuration.
    Lut {
        name: ram,
        base_addr: 0x0060,
        regs: [
            Register {
                name: RAM_CFG,
                read_32: |_, io| {
                    io.ram.encode()
                },
                write_32: |_, io, value| {
                    *io.ram = ram::Config::decode(value);
                },
            },
        ],
        module: {
            use noctane_util::{BitStack as _, BitStackExt as _};

            impl Config {
                pub fn decode(code: u32) -> Self {
                    Self {
                        unk_0: code.pop_bits(3),
                        unk_3: code.pop_bits(1),
                        unk_4: code.pop_bits(3),
                        should_delay_on_simult_read: code.pop_bool(),
                        unk_8: code.pop_bits(1),
                        layout_kind: LayoutKind::decode(code.pop_bits(3)),
                        unk_12: code.pop_bits(4),
                        unk_16: code.pop_bits(16),
                    }
                }
            }

            #[derive(Debug, Default)]
            pub struct Config {
                pub unk_0: u32,
                pub unk_3: u32,
                pub unk_4: u32,
                pub should_delay_on_simult_read: bool,
                pub unk_8: u32,
                pub layout_kind: LayoutKind,
                pub unk_12: u32,
                pub unk_16: u32,
            }

            impl Config {
                pub fn encode(&self) -> u32 {
                    let code = 0;
                    code.push_bits(16, self.unk_16);
                    code.push_bits(4, self.unk_12);
                    code.push_bits(3, self.layout_kind.encode());
                    code.push_bits(1, self.unk_8);
                    code.push_bool(self.should_delay_on_simult_read);
                    code.push_bits(3, self.unk_4);
                    code.push_bits(1, self.unk_3);
                    code.push_bits(3, self.unk_0);

                    code
                }
            }

            impl LayoutKind {
                ///
                ///
                /// # Panics
                ///
                /// This function panics if `code` is greater than 7.
                pub fn decode(code: u32) -> Self {
                    match code {
                        0 => Self::_1M,
                        1 => Self::_4M,
                        2 => Self::_1MPlus1MHighZ,
                        3 => Self::_4MPlus4MHighZ,
                        4 => Self::_2M,
                        5 => Self::_8M_0,
                        6 => Self::_2MPlus2MHighZ,
                        7 => Self::_8M_1,
                        _ => unreachable!(),
                    }
                }
            }

            #[derive(Clone, Copy, Debug, Default)]
            pub enum LayoutKind {
                #[default]
                _1M,
                _4M,
                _1MPlus1MHighZ,
                _4MPlus4MHighZ,
                _2M,
                _8M_0,
                _2MPlus2MHighZ,
                _8M_1,
            }

            impl LayoutKind {
                pub fn encode(self) -> u32 {
                    match self {
                        Self::_1M               => 0,
                        Self::_4M               => 1,
                        Self::_1MPlus1MHighZ    => 2,
                        Self::_4MPlus4MHighZ    => 3,
                        Self::_2M               => 4,
                        Self::_8M_0             => 5,
                        Self::_2MPlus2MHighZ    => 6,
                        Self::_8M_1             => 7,
                    }
                }

                pub fn gen_layout(self) -> Layout {
                    match self {
                        Self::_1M                   => Layout::new(1, 0),
                        Self::_1MPlus1MHighZ        => Layout::new(1, 1),
                        Self::_2M                   => Layout::new(2, 0),
                        Self::_2MPlus2MHighZ        => Layout::new(2, 2),
                        Self::_4M                   => Layout::new(4, 0),
                        Self::_4MPlus4MHighZ        => Layout::new(4, 4),
                        Self::_8M_0 | Self::_8M_1   => Layout::new(8, 0),
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

            #[derive(Clone, Copy, Debug)]
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
                read_32: |_, io| {
                    todo!()
                },
                write_32: |_, io, value| {
                    todo!()
                },
            },
            Register {
                name: MASK,
                read_32: |_, io| {
                    io.int.encode_mask()
                },
                write_32: |_, io, value| {
                    io.int.decode_mask(value);
                },
            },
            Register {
                name: 2,
                read_32: |_, io| {
                    todo!()
                },
                write_32: |_, io, value| {
                    todo!()
                },
            },
            Register {
                name: 3,
                read_32: |_, io| {
                    todo!()
                },
                write_32: |_, io, value| {
                    todo!()
                },
            },
        ],
        module: {
            impl Sources {
                pub fn decode_mask(&mut self, code: u32) {
                    self.vblank.is_enabled  = code.pop_bool();
                    self.gpu.is_enabled     = code.pop_bool();
                    self.cdrom.is_enabled   = code.pop_bool();
                    self.dma.is_enabled     = code.pop_bool();
                    for timer in self.timers.iter_mut() {
                        timer.is_enabled    = code.pop_bool();
                    }
                    self.perif.is_enabled   = code.pop_bool();
                    self.sio                = code.pop_bool();
                    self.spu                = code.pop_bool();
                    self.lightpen           = code.pop_bool();
                }
            }

            pub struct Sources {
                pub vblank: Source,
                pub gpu: Source,
                pub cdrom: Source,
                pub dma: Source,
                pub timers: [TimerSource; 3],
                pub perif: Source,
                pub sio: Source,
                pub spu: Source,
                pub lightpen: Source,
            }

            impl Sources {
                pub fn encode_mask(&self) -> u32 {
                    let mut code = 0;
                    code.push_bool(self.lightpen.is_enabled);
                    code.push_bool(self.spu.is_enabled);
                    code.push_bool(self.sio.is_enabled);
                    code.push_bool(self.perf.is_enabled);
                    for timer in self.timers.iter_mut().rev() {
                        code.push_bool(timer.is_enabled);
                    }
                    code.push_bool(self.dma.is_enabled);
                    code.push_bool(self.cdrom.is_enabled);
                    code.push_bool(self.cdrom.is_enabled);
                    code.push_bool(self.gpu.is_enabled);
                    code.push_bool(self.vblank.is_enabled);

                    code
                }
            }

            /// The source of an interrupt,
            pub struct Source {
                /// Whether or not this interrupt source is enabled.
                pub is_enabled: bool,
                /// Whether or not this interrupt source is requesting an interrupt.
                pub is_requesting: bool,
            }

            macro_rules! impl_deref_for_src {
                ($ty:ty) => {
                    impl std::ops::Deref for $ty {
                        type Target = Source;

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

            pub struct TimerSource {
                pub is_enabled_on_target_hit: bool,
                pub is_enabled_on_overflow: bool,
                pub is_requesting: bool,
            }

            impl TimerSource {
                pub fn is_enabled(&self) -> bool {
                    self.is_enabled_on_target_hit || self.is_enabled_on_overflow
                }
            }
        },
    },
    // DMA Configuration.
    Lut {
        name: dma,
        base_addr: 0x0080,
        regs: [
            Register {
                name: MDECin_MADR,
                read_32: |_, io| {
                    dma::read_madr!(io, mdec_in)
                },
                write_32: |_, io, value| {
                    dma::write_madr!(io, mdec_in, value);
                },
            },
            Register {
                name: MDECin_BCR,
                read_32: |_, io| {
                    dma::read_bcr!(io, mdec_in)
                },
                write_32: |_, io, value| {
                    dma::write_bcr!(io, mdec_in, value);
                },
            },
            Register {
                name: MDECin_CHCR,
                read_32: |_, io| {
                    dma::read_chcr!(io, mdec_in)
                },
                write_32: |_, io, value| {
                    dma::write_chcr!(io, mdec_in, value);
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
                    dma::read_madr!(io, mdec_out)
                },
                write_32: |_, io, value| {
                    dma::write_madr!(io, mdec_out, value);
                },
            },
            Register {
                name: MDECout_BCR,
                read_32: |_, io| {
                    dma::read_bcr!(io, mdec_out)
                },
                write_32: |_, io, value| {
                    dma::write_bcr!(io, mdec_out, value);
                },
            },
            Register {
                name: MDECout_CHCR,
                read_32: |_, io| {
                    dma::read_chcr!(io, mdec_out)
                },
                write_32: |_, io, value| {
                    dma::write_chcr!(io, mdec_out, value);
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
                    dma::read_madr!(io, gpu)
                },
                write_32: |_, io, value| {
                    dma::write_madr!(io, gpu, value);
                },
            },
            Register {
                name: GPU_BCR,
                read_32: |_, io| {
                    dma::read_bcr!(io, gpu)
                },
                write_32: |_, io, value| {
                    dma::write_bcr!(io, gpu, value);
                },
            },
            Register {
                name: GPU_CHCR,
                read_32: |_, io| {
                    dma::read_chcr!(io, gpu)
                },
                write_32: |_, io, value| {
                    dma::write_chcr!(io, gpu, value);
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
                    dma::read_madr!(io, cdrom)
                },
                write_32: |_, io, value| {
                    dma::write_madr!(io, cdrom, value);
                },
            },
            Register {
                name: CDROM_BCR,
                read_32: |_, io| {
                    dma::read_bcr!(io, cdrom)
                },
                write_32: |_, io, value| {
                    dma::write_bcr!(io, cdrom, value);
                },
            },
            Register {
                name: CDROM_CHCR,
                read_32: |_, io| {
                    dma::read_chcr!(io, cdrom)
                },
                write_32: |_, io, value| {
                    dma::write_chcr!(io, cdrom, value);
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
                    dma::read_madr!(io, spu)
                },
                write_32: |_, io, value| {
                    dma::write_madr!(io, spu, value);
                },
            },
            Register {
                name: SPU_BCR,
                read_32: |_, io| {
                    dma::read_bcr!(io, spu)
                },
                write_32: |_, io, value| {
                    dma::write_bcr!(io, spu, value);
                },
            },
            Register {
                name: SPU_CHCR,
                read_32: |_, io| {
                    dma::read_chcr!(io, spu)
                },
                write_32: |_, io, value| {
                    dma::write_chcr!(io, spu, value);
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
                    dma::read_madr!(io, pio)
                },
                write_32: |_, io, value| {
                    dma::write_madr!(io, pio, value);
                },
            },
            Register {
                name: PIO_BCR,
                read_32: |_, io| {
                    dma::read_bcr!(io, pio)
                },
                write_32: |_, io, value| {
                    dma::write_bcr!(io, pio, value);
                },
            },
            Register {
                name: PIO_CHCR,
                read_32: |_, io| {
                    dma::read_chcr!(io, pio)
                },
                write_32: |_, io, value| {
                    dma::write_chcr!(io, pio, value);
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
                    dma::read_madr!(io, otc)
                },
                write_32: |_, io, value| {
                    dma::write_madr!(io, otc, value);
                },
            },
            Register {
                name: OTC_BCR,
                read_32: |_, io| {
                    dma::read_bcr!(io, otc)
                },
                write_32: |_, io, value| {
                    dma::write_bcr!(io, otc, value);
                },
            },
            Register {
                name: OTC_CHCR,
                read_32: |_, io| {
                    dma::read_chcr!(io, otc)
                },
                write_32: |_, io, value| {
                    dma::write_chcr!(io, otc, value);
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
                    todo!()
                },
                write_32: |_, io, value| {
                    todo!()
                },
            },
            Register {
                name: DICR,
                read_32: |_, io| {
                    todo!()
                },
                write_32: |_, io, value| {
                    // TODO: Explain what's happening here.
                    let value = (io.dma.int.0 & !value & !((1 << 24) - 1)) | (value & ((1 << 24) - 1));
                    todo!()
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
            macro_rules! read_madr {
                ($io:expr, $chan:ident $(,)?) => {
                    $io.dma.chan.$chan.cpu_bus_base
                };
            }

            macro_rules! write_madr {
                ($io:expr, $chan:ident, $value:expr $(,)?) => {
                    // The top byte is zeroed.
                    $io.dma.chan.$chan.cpu_bus_base = $value & ((1 << 25) - 1);
                };
            }

            macro_rules! read_bcr {
                ($io:expr, $chan:ident $(,)?) => {
                    $io.dma.chan.$chan.block_cfg
                };
            }

            macro_rules! write_bcr {
                ($io:expr, $chan:ident, $value:expr $(,)?) => {
                    $io.dma.chan.$chan.block_cfg = $value;
                };
            }

            macro_rules! read_chcr {
                ($io:expr, $chan:ident $(,)?) => {
                    $io.dma.chan.$chan.encode_chcr()
                };
            }

            macro_rules! write_chcr {
                ($io:expr, $chan:ident, $value:expr $(,)?) => {
                    $io.dma.chan.$chan.decode_chcr($value);
                };
            }

            pub(crate) use read_madr;
            pub(crate) use write_madr;

            pub(crate) use read_bcr;
            pub(crate) use write_bcr;

            pub(crate) use read_chcr;
            pub(crate) use write_chcr;

            #[derive(Debug, Default)]
            pub struct Config {
                pub chan: Channels,
            }

            impl Config {
                pub fn update(&mut self) -> Option<TransferPacket> {
                    let active_chan = Channels::find_highest_prio(self.chan.enabled_with_prio());

                    if let Some(chan) = active_chan {
                        let txfer = chan.txfer.unwrap_or_else(|| {
                            // A transfer is not in progress. We will start one now.
                            let txfer = Transfer {
                                src: TransferSource::decode(chan.cfg.src()),
                                dir: TransferDirection::decode(chan.cfg.dir()),
                                cpu_bus_base: chan.cpu_bus_base,
                            };
                            chan.txfer = Some(txfer);

                            txfer
                        });

                        // TODO
                        if true {
                            // The transfer is complete.
                            // TODO
                            chan.txfer = None;
                            self.int.dma.spu.is_requesting = true;
                            self.int.dma.gpu.is_requesting = true;
                        }

                        None
                    } else {
                        None
                    }
                }
            }

            #[derive(Debug)]
            pub struct TransferPacket {
                pub source: TransferSource,
                /// The direction in which data is moved during the transfer.
                pub dir: TransferDirection,
                /// The starting address of the transfer on the CPU bus side.
                pub cpu_bus_base: u32,
                /// The starting address of the transfer on the side of the external device.
                pub extern_base: u32,
                /// The length, in 32-bit words, of the transfer.
                pub len: usize,
            }

            #[derive(Clone, Copy, Debug)]
            pub enum TransferSource {
                CpuBus,
                External,
            }

            #[derive(Clone, Copy, Debug)]
            pub enum TransferDirection {
                Forward,
                Backward,
            }

            impl Default for Channels {
                fn default() -> Self {
                    Self {
                        mdec_in: Channel::new(
                            |params| {
                                todo!()
                            },
                            |params| {
                                todo!()
                            },
                        ),
                        mdec_out: Channel::new(
                            |params| {
                                todo!()
                            },
                            |params| {
                                todo!()
                            },
                        ),
                        gpu: Channel::new(
                            |params| {
                                todo!()
                            },
                            |params| {
                                todo!()
                            },
                        ),
                        cdrom: Channel::new(
                            |params| {
                                todo!()
                            },
                            |params| {
                                todo!()
                            },
                        ),
                        spu: Channel::new(
                            |params| {
                                todo!()
                            },
                            |params| {
                                todo!()
                            },
                        ),
                        pio: Channel::new(
                            |params| {
                                todo!()
                            },
                            |params| {
                                todo!()
                            },
                        ),
                        otc: Channel::new(
                            |params| {
                                todo!()
                            },
                            |params| {
                                todo!()
                            },
                        ),
                    }
                }
            }

            #[derive(Debug)]
            pub struct Channels {
                pub mdec_in: Channel,
                pub mdec_out: Channel,
                pub gpu: Channel,
                pub cdrom: Channel,
                pub spu: Channel,
                pub pio: Channel,
                pub otc: Channel,
            }

            impl Channels {
                fn enabled_with_prio<'a>(
                    &'a mut self,
                ) -> impl 'a + Iterator<Item = (&'a mut Channel, Priority)> {
                    [
                        (
                            &mut self.mdec_in,
                            self.cfg.mdec_in_prio(),
                            self.cfg.mdec_in_is_enabled(),
                        ),
                        (
                            &mut self.mdec_out,
                            self.cfg.mdec_out_prio(),
                            self.cfg.mdec_out_is_enabled(),
                        ),
                        (
                            &mut self.gpu,
                            self.cfg.gpu_prio(),
                            self.cfg.gpu_is_enabled(),
                        ),
                        (
                            &mut self.cdrom,
                            self.cfg.cdrom_prio(),
                            self.cfg.cdrom_is_enabled(),
                        ),
                        (
                            &mut self.spu,
                            self.cfg.spu_prio(),
                            self.cfg.spu_is_enabled(),
                        ),
                        (
                            &mut self.pio,
                            self.cfg.pio_prio(),
                            self.cfg.pio_is_enabled(),
                        ),
                        (
                            &mut self.otc,
                            self.cfg.otc_prio(),
                            self.cfg.otc_is_enabled(),
                        ),
                    ]
                    .into_iter()
                    .filter(|(chan, _, is_enabled)| *is_enabled)
                    .map(|(chan, prio, _)| (chan, Priority(prio)))
                }
            }

            struct Priority(u32);

            impl Channels {
                fn find_highest_prio<'a>(
                    mut chan_with_prio: impl 'a + Iterator<Item = (&'a mut Channel, Priority)>,
                ) -> Option<&'a mut Channel> {
                    let Some((mut best_chan, mut highest_prio)) = chan_with_prio.next() else {
                        return None;
                    };

                    while let Some((chan, prio)) = chan_with_prio.next() {
                        // We use `<=` because, if two channels are of equal priority, then the
                        // channel with the higher number (i.e. OTC is highest) is selected. Because
                        // of [`Self::enabled_with_prio`], we are guaranteed to be iterating from
                        // the least to highest numbered channels.
                        if prio.0 <= highest_prio.0 {
                            best_chan = chan;
                            highest_prio = prio;
                        }
                    }

                    Some(best_chan)
                }
            }

            impl Channel {
                fn new(read: fn(ReadParameters), write: fn(WriteParameters)) -> Self {
                    Self {
                        txfer: None,
                        cpu_bus_base: 0,
                        block_cfg: 0,
                        read,
                        write,
                    }
                }
            }

            #[derive(Debug)]
            pub struct Channel {
                pub txfer: Option<Transfer>,
                pub cpu_bus_base: u32,
                pub block_cfg: u32,
                pub read: fn(ReadParameters),
                pub write: fn(WriteParameters)
            }

            #[derive(Clone, Copy, Debug)]
            pub struct Transfer {
                pub src: TransferSource,
                pub dir: TransferDirection,
                pub cpu_bus_base: u32,
            }

            impl TransferSource {
                fn decode(value: bool) -> Self {
                    if value {
                        Self::CpuBus
                    } else {
                        Self::External
                    }
                }
            }

            impl TransferDirection {
                fn decode(value: bool) -> Self {
                    if value {
                        Self::Backward
                    } else {
                        Self::Forward
                    }
                }
            }

            pub struct ReadParameters {

            }

            pub struct WriteParameters {

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
                    $io.timers.$timer.encode_mode()
                };
                ($io:expr, $timer:ident . $field:ident) => {
                    $io.timers.$timer.$field as u32
                };
            }

            macro_rules! write_field {
                ($io:expr, $timer:ident . mode = $value:expr) => {
                    $io.timers.$timer.decode_mode($value as u16);
                };
                ($io:expr, $timer:ident . $field:ident = $value:expr) => {
                    $io.timers.$timer.$field = $value as u16;
                };
            }

            pub(crate) use read_field;
            pub(crate) use write_field;

            impl Default for Timers {
                fn default() -> Self {
                    Self {
                        dotclock: Timer::new(
                            0,
                            |src| {
                                match src.0 & 0b11 {
                                    0 | 2 => Self::get_sysclock_increment(),
                                    1 | 3 => Self::get_dotclock_increment(),
                                    _ => unreachable!(),
                                }
                            },
                        ),
                        hblank: Timer::new(
                            1,
                            |src| {
                                match src.0 & 0b11 {
                                    0 | 2 => Self::get_sysclock_increment(),
                                    1 | 3 => Self::get_hblank_increment(),
                                    _ => unreachable!(),
                                }
                            },
                        ),
                        sysclock: Timer::new(
                            2,
                            |src| {
                                let inc = Self::get_sysclock_increment();

                                match src.0 & 0b11 {
                                    0 | 1 => inc,
                                    2 | 3 => inc / 8,
                                    _ => unreachable!(),
                                }
                            },
                        ),
                    }
                }
            }

            #[derive(Debug)]
            pub struct Timers {
                pub dotclock: Timer,
                pub hblank: Timer,
                pub sysclock: Timer,
            }

            impl Timers {
                pub fn update(&mut self, int: &mut super::int::Sources) {
                    self.dotclock.update(int);
                    self.hblank.update(int);
                    self.sysclock.update(int);
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

            impl Timer {
                pub fn new(
                    index: usize,
                    calc_increment_amount: fn(ClockSource) -> u16,
                ) -> Self {
                    Self {
                        index,
                        counter: 0,
                        target: 0,
                        clock_source: ClockSource(0),
                        calc_increment_amount,
                    }
                }
            }

            #[derive(Debug, Default)]
            pub struct Timer {
                pub index: usize,
                // Timer fields are supposed to be 32-bit but contain 16 bits of garbage, so they
                // are represented here with [`u16`]. Accordingly, the [`read_field`] and
                // [`write_field`] macros above perform the appropriate conversions between [`u32`]
                // and [`u16`].
                pub counter: u16,
                pub target: u16,

                pub clock_source: ClockSource,
                pub calc_increment_amount: fn(ClockSource) -> u16,
            }

            #[derive(Debug)]
            pub struct ClockSource(u16);

            impl Timer {
                fn update(&mut self, int: &mut super::int::Sources) {
                    let hit_target = self.mode.use_target() && (self.counter >= self.target);
                    let counter_overflowed = !hit_target && (self.counter == !0);

                    // Update the counter.
                    if hit_target {
                        self.counter = 0
                    } else {
                        let increment_amount = (self.calc_increment_amount)(self.clock_source);
                        self.counter = self.counter.wrapping_add(increment_amount);
                    }

                    let int_source = &mut int.timers[self.index];
                    // Request an interrupt, if necessary.
                    if (int_source.is_enabled_on_target_hit && hit_target) ||
                       (int_source.is_enabled_on_overflow && counter_overflowed)
                    {
                        int_source.is_requesting = true;
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
                    todo!()
                },
                write_32: |_, io, mach| {
                    io.gpu.queue_gp1_machine_command(mach);
                },
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
                read_16: |_, io, addr| {
                    todo!()
                },
                write_16: |_, io, addr, value| {
                    todo!()
                },
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
                name: DATA_TXFR_CFG,
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
                read_8: |_, io, addr| {
                    todo!()
                },
                write_8: |_, io, addr, value| {
                    todo!()
                },
            },
            Register {
                name: 1,
                read_8: |_, io, addr| {
                    todo!()
                },
                write_8: |_, io, addr, value| {
                    todo!()
                },
            },
            Register {
                name: DATA,
                read_8: |_, io, addr| {
                    todo!()
                },
                write_8: |_, io, addr, value| {
                    todo!()
                },
            },
            Register {
                name: 3,
                read_8: |_, io, addr| {
                    todo!()
                },
                write_8: |_, io, addr, value| {
                    todo!()
                },
            },
            Register {
                name: 4,
                read_16: |_, io, addr| {
                    todo!()
                },
                write_16: |_, io, addr, value| {
                    todo!()
                },
            },
        ],
        module: {
            // TODO
        },
    },
    // Dual Serial Port.
    Lut {
        name: duart,
        base_addr: 0x1020,
        regs: [
            Register {
                name: MODE_0,
                read_8: |_, io, addr| {
                    todo!()
                },
                write_8: |_, io, addr, value| {
                    todo!()
                },
            },
            Register {
                name: STAT_0,
                read_8: |_, io, addr| {
                    todo!()
                },
                write_8: |_, io, addr, value| {
                    todo!()
                },
            },
            Register {
                name: 3,
                read_8: |_, io, addr| {
                    todo!()
                },
                write_8: |_, io, addr, value| {
                    todo!()
                },
            },
            Register {
                name: FIFO_0,
                read_8: |_, io, addr| {
                    todo!()
                },
                write_8: |_, io, addr, value| {
                    todo!()
                },
            },
            Register {
                name: 4,
                read_8: |_, io, addr| {
                    todo!()
                },
                write_8: |_, io, addr, value| {
                    todo!()
                },
            },
            // TODO
        ],
        module: {
            // TODO
        },
    },
    Lut {
        name: post,
        base_addr: 0x1040,
        regs: [
            Register {
                name: BOOT_MODE,
                read_8: |_, io, addr| {
                    todo!()
                },
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
