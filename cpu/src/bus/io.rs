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
    /// Interrupt sources.
    pub int: &'a mut int::Sources,
    /// The last result of a GPU command (GP0 or GP1).
    pub last_gpu_result: &'a mut u32,
    /// The Power-On Self-Test (POST) state.
    pub post: &'a mut post::Status,
    /// The configuration for main RAM.
    pub ram: &'a mut ram::Config,
    /// The configuration for the Sound Processing Unit (SPU).
    pub spu: &'a mut spu::Config,
    /// The configurations for the 24 SPU voices.
    pub spu_voices: &'a mut [spu_voice::Config; 24],
    /// The system timers, or root counters.
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
            post::BASE_ADDR..       => get_lut_entry!(post),
            x1030::BASE_ADDR..      => get_lut_entry!(x1030),
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
    /// The name of this register.
    ///
    /// Conventionally, this is uppercase and snake-case with abbreviations understandable in
    /// context, e.g., `REG_NAME`.
    name: &'static str,
    /// Reads 8 bits from this register.
    read_8: fn(&Self, &Io, Address) -> u8,
    /// Reads 16 bits from this register.
    read_16: fn(&Self, &Io, Address) -> u16,
    /// Reads 32 bits from this register.
    read_32: fn(&Self, &Io) -> u32,
    /// Writes 8 bits to this register.
    write_8: fn(&Self, &mut Io, Address, u8),
    /// Writes 16 bits to this register.
    write_16: fn(&Self, &mut Io, Address, u16),
    /// Writes 32 bits to this register.
    write_32: fn(&Self, &mut Io, u32),
}

/// An entry within any of the lookup tables (LUTs) defined in the submodules for the various
/// I/O register groups. This entry represents one byte of a single register.
///
/// `reg_idx` points to an register entry in the [`REGISTERS`] table, while `byte_idx` indicates
/// the index of the byte represented by this [`LutEntry`] into the register it points to, and is
/// intended as the basis for the `addr` input to [`Register`] functions.
struct LutEntry {
    /// The index of the [`REGISTER`] entry that this LUT entry points to.
    reg_idx: usize,
    /// The index, rebased to the start of the register that this entry points to, of the byte to be
    /// accessed.
    ///
    /// This may be 0, 1, 2, or 3. All are valid for 8-bit accesses, but only 0 and 2 are valid for
    /// 16-bit accesses, and only 0 is valid for 32-bit accesses.
    byte_idx: usize,
}

// Define the I/O registers. This is going to be quite long...
gen_cpu_bus_io!(
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
                    // To properly align Exp. Region 1, the lower bits of the base address are
                    // ignored.
                    io.bus.exp_1.base = value & !((1 << io.bus.exp_1.size_shift) - 1);
                },
            },
            Register {
                name: EXP_2_BASE,
                read_32: |_, io| {
                    io.bus.exp_2.base
                },
                write_32: |_, io, value| {
                    // The top byte is hardcoded to `0x1f`.
                    io.bus.exp_2.base = 0x1f00_0000 | (value & 0x00ff_ffff);
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
            //! CPU bus configuration.
            //!
            //! The sizes, access delays, and, in some cases, base addresses of the various CPU bus
            //! banks may be configured here.

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

            /// The configurations for banks accessible to the CPU bus.
            #[derive(Clone, Debug)]
            pub struct Config {
                /// The configuration for Expansion Region 1.
                pub exp_1: RebaseableBankConfig,
                /// The configuration for Expansion Region 2.
                pub exp_2: RebaseableBankConfig,
                /// The configuration for Expansion Region 3.
                pub exp_3: BankConfig,
                /// The configuration for the Basic Input/Output System (BIOS) bank.
                pub bios: BankConfig,
                // TODO: Is this correct?
                /// The configuration for SPU RAM.
                pub spu: BankConfig,
                // TODO: Is this correct?
                /// The configuration for CD-ROM RAM.
                pub cdrom: BankConfig,
                // TODO: Document this. What is it used for?
                pub common: BankConfig,
            }

            /// The configuration for a rebaseable CPU bus bank.
            #[derive(Clone, Debug, Default)]
            pub struct RebaseableBankConfig {
                pub(super) inner: BankConfig,
                /// The base address of this bank on the CPU bus.
                ///
                /// Sometimes, this field is not respected. This is true in the case of Expansion
                /// Region 2 when any value other than `0x1f802000` is provided, in which case the
                /// region is entirely disabled.
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
                /// The appropriate default configuration for a BIOS bank.
                pub fn bios() -> Self {
                    Self {
                        // The BIOS size must be large enough on boot to at least accommodate the
                        // code that configures itself; that is usually the first two instructions.
                        // TODO: This has to be 4 for some reason. Why not 3?
                        size_shift: 4,
                        ..Default::default()
                    }
                }

                /// Decodes this configuration from an I/O register.
                pub fn decode(mut code: u32) -> Self {
                    Self {
                        delays: DelaysConfig::decode(code.pop_bits(8)),
                        periods: PeriodsConfig::decode(code.pop_bits(4)),
                        bit_width: BitWidth::decode(code.pop_bits(1)),
                        should_auto_inc: code.pop_bool(),
                        unk_14: code.pop_bits(2),
                        size_shift: code.pop_bits(5),
                        unk_21: code.pop_bits(3),
                    }
                }
            }

            /// The configuration for a CPU bus bank.
            #[derive(Clone, Debug, Default)]
            pub struct BankConfig {
                /// The configuration for access delays.
                pub delays: DelaysConfig,
                /// The configuration for access periods.
                pub periods: PeriodsConfig,
                // TODO: This field is currently not respected.
                /// The bit-width of this bank's data bus.
                pub bit_width: BitWidth,
                // TODO
                pub should_auto_inc: bool,
                // TODO
                pub unk_14: u32,
                // TODO
                pub size_shift: u32,
                // TODO
                pub unk_21: u32,
                // TODO
            }

            impl BankConfig {
                /// Encodes this configuration into an I/O register.
                pub fn encode(&self) -> u32 {
                    let mut code = 0;
                    code.push_bits(3, self.unk_21);
                    code.push_bits(5, self.size_shift);
                    code.push_bits(2, self.unk_14);
                    code.push_bool(self.should_auto_inc);
                    code.push_bits(1, self.bit_width.encode());
                    code.push_bits(4, self.periods.encode());
                    code.push_bits(8, self.delays.encode());

                    code
                }
            }

            impl DelaysConfig {
                /// Decodes a configuration from an I/O register.
                pub fn decode(mut code: u32) -> Self {
                    Self {
                        // There must always be at least a one-cycle delay. Therefore, Sony uses 0
                        // to represent a one-cycle delay. We will increment by one to reach the
                        // true cycle delay.
                        write: code.pop_bits(4).wrapping_add(1),
                        read: code.pop_bits(4).wrapping_add(1),
                    }
                }
            }

            /// The configuration for access delays.
            #[derive(Clone, Copy, Debug, Default)]
            pub struct DelaysConfig {
                /// The number of delay cycles after a write.
                pub write: u32,
                /// The number of delay cycles after a read.
                pub read: u32,
            }

            impl DelaysConfig {
                /// Encodes this configuration into an I/O register.
                pub fn encode(self) -> u32 {
                    let mut code = 0;
                    // See the note in [`Self::decode`] for why we subtract here.
                    code.push_bits(4, self.read.wrapping_sub(1));
                    code.push_bits(4, self.write.wrapping_sub(1));

                    code
                }
            }

            impl PeriodsConfig {
                /// Decodes a configuration from an I/O register.
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
                // TODO: What is this?
                /// Whether or not the recovery period is enabled.
                pub recovery_is_enabled: bool,
                // TODO: What is this?
                /// Whether or not the hold period is enabled.
                pub hold_is_enabled: bool,
                // TODO: What is this?
                /// Whether or not the floating period is enabled.
                pub floating_is_enabled: bool,
                // TODO: What is this?
                /// Whether or not the pre-strobe period is enabled.
                pub pre_strobe_is_enabled: bool,
            }

            impl PeriodsConfig {
                /// Encodes this configuration into an I/O register.
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
                /// Decodes a bit-width from an I/O register.
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

            /// The bit-width of a data bus.
            #[derive(Clone, Copy, Debug, Default)]
            pub enum BitWidth {
                /// The data bus is 8 bits wide.
                #[default]
                Eight,
                /// The data bus is 16 bits wide.
                Sixteen,
            }

            impl BitWidth {
                /// Encodes this bit-width into an I/O register.
                pub fn encode(self) -> u32 {
                    match self {
                        Self::Eight     => 0,
                        Self::Sixteen   => 1,
                    }
                }
            }
        },
    },
    Lut {
        name: perif,
        base_addr: 0x0040,
        regs: [
            Register {
                name: JOY_DATA,
                read_32: |_, _| {
                    todo!()
                },
                write_32: |_, _, _| {
                    todo!()
                },
            },
            Register {
                name: JOY_STAT,
                read_32: |_, _| {
                    todo!()
                },
                write_32: |_, _, _| {
                    todo!()
                },
            },
            Register {
                name: JOY_MODE,
                read_16: |_, _, _| {
                    todo!()
                },
                write_16: |_, _, _, _| {
                    todo!()
                },
            },
            Register {
                name: JOY_CFG,
                read_16: |_, _, _| {
                    todo!()
                },
                write_16: |_, _, _, _| {
                    todo!()
                },
            },
            Register {
                name: JOY_BAUD,
                read_16: |_, _, _| {
                    todo!()
                },
                write_16: |_, _, _, _| {
                    todo!()
                },
            },
            Register {
                name: SIO_DATA,
                read_32: |_, _| {
                    todo!()
                },
                write_32: |_, _, _| {
                    todo!()
                },
            },
            Register {
                name: SIO_STAT,
                read_32: |_, _| {
                    todo!()
                },
                write_32: |_, _, _| {
                    todo!()
                },
            },
            Register {
                name: SIO_MODE,
                read_16: |_, _, _| {
                    todo!()
                },
                write_16: |_, _, _, _| {
                    todo!()
                },
            },
            Register {
                name: SIO_CFG,
                read_16: |_, _, _| {
                    todo!()
                },
                write_16: |_, _, _, _| {
                    todo!()
                },
            },
            Register {
                name: SIO_MISC,
                read_16: |_, _, _| {
                    todo!()
                },
                write_16: |_, _, _, _| {
                    todo!()
                },
            },
            Register {
                name: SIO_BAUD,
                read_16: |_, _, _|  {
                    todo!()
                },
                write_16: |_, _, _, _| {
                    todo!()
                },
            },
        ],
        module: {
            //! Peripheral I/O ports.

            // TODO
        },
    },
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
            //! Main CPU RAM configuration.

            use noctane_util::{BitStack as _, BitStackExt as _};

            impl Config {
                /// Decodes a configuration from an I/O register.
                pub fn decode(mut code: u32) -> Self {
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

            /// The configuration for main CPU RAM.
            #[derive(Debug, Default)]
            pub struct Config {
                // TODO
                pub unk_0: u32,
                // TODO
                pub unk_3: u32,
                // TODO
                pub unk_4: u32,
                /// Whether or not a one-cycle delay should be incurred on a simultaneous code and
                /// data fetch.
                pub should_delay_on_simult_read: bool,
                // TODO
                pub unk_8: u32,
                /// The kind of layout that main CPU RAM should assume.
                pub layout_kind: LayoutKind,
                // TODO
                pub unk_12: u32,
                // TODO
                pub unk_16: u32,
            }

            impl Config {
                /// Encodes this configuration into an I/O register.
                pub fn encode(&self) -> u32 {
                    let mut code = 0;
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
                /// Decodes a layout from an I/O register.
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
                        5 => Self::_8M0,
                        6 => Self::_2MPlus2MHighZ,
                        7 => Self::_8M1,
                        _ => unreachable!(),
                    }
                }
            }

            /// A kind of RAM layout.
            #[derive(Clone, Copy, Debug, Default)]
            pub enum LayoutKind {
                // TODO
                #[default]
                _1M,
                // TODO
                _4M,
                // TODO
                _1MPlus1MHighZ,
                // TODO
                _4MPlus4MHighZ,
                // TODO
                _2M,
                // TODO
                _8M0,
                // TODO
                _2MPlus2MHighZ,
                // TODO
                _8M1,
            }

            impl LayoutKind {
                /// Encodes this layout into an I/O register.
                pub fn encode(self) -> u32 {
                    match self {
                        Self::_1M               => 0,
                        Self::_4M               => 1,
                        Self::_1MPlus1MHighZ    => 2,
                        Self::_4MPlus4MHighZ    => 3,
                        Self::_2M               => 4,
                        Self::_8M0              => 5,
                        Self::_2MPlus2MHighZ    => 6,
                        Self::_8M1              => 7,
                    }
                }

                /// Generates a [`Layout`].
                pub fn gen_layout(self) -> Layout {
                    match self {
                        Self::_1M                   => Layout::new(1, 0),
                        Self::_1MPlus1MHighZ        => Layout::new(1, 1),
                        Self::_2M                   => Layout::new(2, 0),
                        Self::_2MPlus2MHighZ        => Layout::new(2, 2),
                        Self::_4M                   => Layout::new(4, 0),
                        Self::_4MPlus4MHighZ        => Layout::new(4, 4),
                        Self::_8M0 | Self::_8M1     => Layout::new(8, 0),
                    }
                }
            }

            impl Layout {
                /// Creates a new `Layout`.
                ///
                /// This function is merely for convenience and is equivalent to constructing this
                /// type with a struct-field expression.
                pub const fn new(data_mb: u32, high_z_mb: u32) -> Self {
                    Self {
                        data_mb,
                        high_z_mb,
                    }
                }
            }

            /// A RAM layout.
            ///
            /// This defines how much of main CPU RAM should be accessible.
            #[derive(Clone, Copy, Debug)]
            pub struct Layout {
                /// The size, in megabytes, of accessible data.
                pub data_mb: u32,
                /// The number, in megabytes, of `0xff`s following the data region.
                ///
                /// This 'high Z' region does not generate address exceptions.
                pub high_z_mb: u32,
            }
        },
    },
    Lut {
        name: int,
        base_addr: 0x0070,
        regs: [
            Register {
                name: STAT,
                read_32: |_, io| {
                    io.int.encode_stat()
                },
                write_32: |_, io, value| {
                    io.int.decode_stat(value);
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
                read_32: |_, _| {
                    // TODO: Undocumented.
                    0
                },
                write_32: |_, _, _| {
                    // TODO: Undocumented.
                },
            },
            Register {
                name: 3,
                read_32: |_, _| {
                    // TODO: Undocumented.
                    0
                },
                write_32: |_, _, _| {
                    // TODO: Undocumented.
                },
            },
        ],
        module: {
            //! Interrupt configuration.
            //!
            //! Almost all CPUs can receive notifications from external devices when an event has
            //! occurred; the PSX CPU is no different. This mechanism is interrupts.
            //!
            //! # Sources
            //!
            //! There are 9 kind of interrupt sources. Each source is capable of generating
            //! interrupt requests to be processed by the CPU, and requests from different sources
            //! may even be concurrently pending. These sources are:
            //! - Vertical blanks (V-blanks). These requests are generated by the GPU in the
            //!   vertical blanking period after a frame is presented. PSX software commonly awaits
            //!   V-blanks to begin rendering for the next frame.
            //! - The GPU.
            //! - The CD-ROM drive.
            //! - DMA. These requests are generated when a DMA transfer has completed.
            //! - Timers. These requests may be generated either when a timer overflows, or when a
            //!   timer's counter reaches a specified target value. As there are three timers in the
            //!   PSX, there are three timer interrupt sources.
            //! - Peripherals&mdash;specifically, joypads (i.e. controllers) and the serial port
            //!   (SIO).
            //! - The SPU.
            //! - A lightpen. I don't actually know what these are, but the NO$ documentation claims
            //!   they exist.

            use noctane_util::{BitStack as _, BitStackExt as _};

            /// Sources of interrupt requests.
            #[derive(Clone, Debug, Default)]
            pub struct Sources {
                pub vblank: Source,
                pub gpu: Source,
                pub cdrom: Source,
                pub dma: DmaSource,
                pub timers: [TimerSource; 3],
                pub perif: Source,
                pub sio: Source,
                pub spu: Source,
                pub lightpen: Source,
            }

            impl Sources {
                /// Decodes a `STAT` register.
                pub fn decode_stat(&mut self, mut code: u32) {
                    macro_rules! ack {
                        ($field:ident) => {
                            // If this field is set to 0, the IRQ bit is cleared.
                            if !code.pop_bool() {
                                self.$field.request = None;
                            }
                        }
                    }

                    ack!(vblank);
                    ack!(gpu);
                    ack!(cdrom);
                    // Acknowledging a DMA interrupt in this manner only acknowledges the master
                    // flag and *not* the individual channel IRQ bits; those must be acknowledged
                    // separately.
                    ack!(dma);
                    for timer in self.timers.iter_mut() {
                        if !code.pop_bool() {
                            timer.request = None;
                        }
                    }
                    ack!(perif);
                    ack!(sio);
                    ack!(spu);
                    ack!(lightpen);
                }

                /// Encodes a `STAT` register.
                pub fn encode_stat(&self) -> u32 {
                    let mut code = 0;
                    code.push_bool(self.lightpen.request.is_some());
                    code.push_bool(self.spu.request.is_some());
                    code.push_bool(self.sio.request.is_some());
                    code.push_bool(self.perif.request.is_some());
                    for timer in self.timers.iter().rev() {
                        code.push_bool(timer.request.is_some());
                    }
                    code.push_bool(self.dma.request.is_some());
                    code.push_bool(self.cdrom.request.is_some());
                    code.push_bool(self.gpu.request.is_some());
                    code.push_bool(self.vblank.request.is_some());

                    code
                }

                /// Decodes a `MASK` register.
                pub fn decode_mask(&mut self, mut code: u32) {
                    self.vblank.is_enabled      = code.pop_bool();
                    self.gpu.is_enabled         = code.pop_bool();
                    self.cdrom.is_enabled       = code.pop_bool();
                    self.dma.is_enabled         = code.pop_bool();
                    for timer in self.timers.iter_mut() {
                        timer.is_enabled        = code.pop_bool();
                    }
                    self.perif.is_enabled       = code.pop_bool();
                    self.sio.is_enabled         = code.pop_bool();
                    self.spu.is_enabled         = code.pop_bool();
                    self.lightpen.is_enabled    = code.pop_bool();
                }

                /// Encodes a `MASK` register.
                pub fn encode_mask(&self) -> u32 {
                    let mut code = 0;
                    code.push_bool(self.lightpen.is_enabled);
                    code.push_bool(self.spu.is_enabled);
                    code.push_bool(self.sio.is_enabled);
                    code.push_bool(self.perif.is_enabled);
                    for timer in self.timers.iter().rev() {
                        code.push_bool(timer.is_enabled);
                    }
                    code.push_bool(self.dma.is_enabled);
                    code.push_bool(self.cdrom.is_enabled);
                    code.push_bool(self.gpu.is_enabled);
                    code.push_bool(self.vblank.is_enabled);

                    code
                }

                /// Decodes a `DICR` register.
                pub fn decode_dicr(&mut self, mut code: u32) {
                    // TODO: Unknown (R/W).
                    let _ = code.pop_bits(6);
                    // Always zero.
                    code.pop_bits(9);
                    // Force IRQ.
                    //
                    // If this bit is set, a DMA interrupt request is unconditionally generated.
                    self.dma.request = code.pop_bool().then_some(Request::default());
                    // IRQ Enable for DMA0..=DMA6.
                    for chan in self.dma.chan.iter_mut() {
                        chan.is_enabled = code.pop_bool();
                    }
                    // IRQ Master Enable for DMA0..=DMA6.
                    self.dma.is_enabled = code.pop_bool();
                    // IRQ Flags for DMA0..=DMA6.
                    for chan in self.dma.chan.iter_mut() {
                        // If this bit is set, the flag is acknowledged.
                        if code.pop_bool() {
                            chan.is_requesting = false;
                        }
                    }
                    // IRQ Master Flag.
                    //
                    // This field is read-only, so we don't need to decode it.
                }

                /// Encodes a `DICR` register.
                pub fn encode_dicr(&self) -> u32 {
                    let mut code = 0;
                    // IRQ Master Flag.
                    code.push_bool(self.dma.request.is_some());
                    // IRQ Flags for DMA0..=DMA6.
                    for chan in self.dma.chan.iter().rev() {
                        code.push_bool(chan.is_requesting);
                    }
                    // IRQ Master Enable for DMA0..=DMA6.
                    code.push_bool(self.dma.is_enabled);
                    // IRQ Enable for DMA0..=DMA6.
                    for chan in self.dma.chan.iter().rev() {
                        code.push_bool(chan.is_enabled);
                    }
                    // Force IRQ.
                    //
                    // I think this field is write-only as it doesn't really make sense to read it.
                    // We'll return 0.
                    code.push_bool(false);
                    // Always zero.
                    code.push_bits(9, 0);
                    // TODO: Unknown (R/W).
                    code.push_bits(6, 0);

                    code
                }

                /// Retrieves the next pending interrupt request.
                ///
                /// If no interrupt requests are pending, or the sources from which those requests
                /// originated are disabled, this method returns `None`.
                pub fn take_request(&mut self) -> Option<()> {
                    macro_rules! take_request {
                        ($field:ident) => {
                            if self.$field.take_request().is_some() {
                                tracing::info!("IRQ: {}", stringify!($field));
                                return Some(());
                            }
                        };
                    }

                    take_request!(vblank);
                    take_request!(gpu);
                    take_request!(cdrom);
                    take_request!(dma);
                    // TODO:
                    //
                    // Should individual DMA channel requests be checked here? The current behavior
                    // is that, when any DMA channel requests an interrupt, the master IRQ flag is
                    // set as well as that of the channel, and when the interrupt is served, only
                    // the master IRQ flag is checked. This means that acknowledging the
                    // channel-specific IRQ flag has no bearing on whether the interrupt request is
                    // reset. The alternative would be to require both the master IRQ flag and the
                    // channel-specific IRQ flag to be acknowledged in order for the interrupt
                    // request to be reset.
                    if self.timers.iter_mut().any(|it| it.take_request().is_some()) {
                        return Some(());
                    }
                    take_request!(perif);
                    take_request!(sio);
                    take_request!(spu);
                    take_request!(lightpen);

                    None
                }
            }

            /// The source of an interrupt.
            #[derive(Clone, Debug, Default)]
            pub struct Source {
                /// Whether or not this interrupt source is enabled.
                pub is_enabled: bool,
                /// The next interrupt request from this source, if one is pending.
                pub request: Option<Request>,
            }

            impl Source {
                /// Retrieves the next pending interrupt request.
                pub fn take_request(&mut self) -> Option<()> {
                    if self.is_enabled {
                        if let Some(req) = self.request.as_mut() {
                            if !req.was_served {
                                req.was_served = true;

                                return Some(());
                            }
                        }
                    }

                    None
                }
            }

            /// An interrupt request.
            #[derive(Clone, Debug, Default)]
            pub struct Request {
                /// Whether or not this interrupt request was served by the CPU.
                pub was_served: bool,
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

            /// The interrupt source for a timer.
            #[derive(Clone, Debug, Default)]
            pub struct TimerSource {
                inner: Source,
                /// Whether or not this source generates a request when its timer's counter reaches
                /// the target value.
                pub is_enabled_on_target_hit: bool,
                /// Whether or not this source generates a request when its timer overflows.
                pub is_enabled_on_overflow: bool,
            }

            impl_deref_for_src!(TimerSource);

            /// The interrupt source for all DMA channels.
            #[derive(Clone, Debug, Default)]
            pub struct DmaSource {
                inner: Source,
                pub chan: [DmaChannelSource; 7],
            }

            impl_deref_for_src!(DmaSource);

            /// The interrupt source for a DMA channel.
            #[derive(Clone, Copy, Debug, Default)]
            pub struct DmaChannelSource {
                /// Whether or not this source is enabled.
                pub is_enabled: bool,
                /// Whether or not an interrupt request from this channel is pending.
                pub is_requesting: bool,
            }
        },
    },
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
                read_32: |_, _| {
                    // TODO: Undocumented.
                    0
                },
                write_32: |_, _, _| {
                    // TODO: Undocumented.
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
                read_32: |_, _| {
                    // TODO: Undocumented.
                    0
                },
                write_32: |_, _, _| {
                    // TODO: Undocumented.
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
                read_32: |_, _| {
                    // TODO: Undocumented.
                    0
                },
                write_32: |_, _, _| {
                    // TODO: Undocumented.
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
                read_32: |_, _| {
                    // TODO: Undocumented.
                    0
                },
                write_32: |_, _, _| {
                    // TODO: Undocumented.
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
                read_32: |_, _| {
                    // TODO: Undocumented.
                    0
                },
                write_32: |_, _, _| {
                    // TODO: Undocumented.
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
                read_32: |_, _| {
                    // TODO: Undocumented.
                    0
                },
                write_32: |_, _, _| {
                    // TODO: Undocumented.
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
                read_32: |_, _| {
                    // TODO: Undocumented.
                    0
                },
                write_32: |_, _, _| {
                    // TODO: Undocumented.
                },
            },
            Register {
                name: DPCR,
                read_32: |_, io| {
                    io.dma.encode_dpcr()
                },
                write_32: |_, io, value| {
                    io.dma.decode_dpcr(value);
                },
            },
            Register {
                name: DICR,
                read_32: |_, io| {
                    io.int.encode_dicr()
                },
                write_32: |_, io, value| {
                    io.int.decode_dicr(value);
                },
            },
            Register {
                name: 30,
                read_32: |_, _| {
                    // TODO: Undocumented.
                    0
                },
                write_32: |_, _, _| {
                    // TODO: Undocumented.
                },
            },
            Register {
                name: 31,
                read_32: |_, _| {
                    // TODO: Undocumented.
                    0
                },
                write_32: |_, _, _| {
                    // TODO: Undocumented.
                },
            },
        ],
        module: {
            //! DMA configuration.
            //!
            //! Like interrupts, almost all CPUs have these. The way I see it, DMAs solve two
            //! problems:
            //! - Copying data between the CPU and external devices using only load and store
            //!   instructions to and from I/O registers is painfully slow compared to what can be
            //!   achieved with dedicated circuitry.
            //! - It is sometimes the only way for a CPU is access the memory of external devices,
            //!   i.e., if I/O registers aren't available.
            //!
            //! # Channels
            //!
            //! In the PSX, there are 7 DMA channels. They are:
            //! - MDEC (in).
            //! - MDEC (out).
            //! - GPU.
            //! - CD-ROM.
            //! - SPU.
            //! - PIO.
            //! - OTC.
            //!
            //! # Sync Strategies
            //!
            //! There are three different strategies for performing a transfer. Some channels
            //! support just one, and others support two. They are:
            //! - Copy everything instantaneously.
            //! - Break the data to be transferred into blocks and copy one block at a time.
            //! - Send a pointer to a linked list that the external device can traverse.

            use std::fmt;

            use noctane_util::{BitStack as _, BitStackExt as _};
            use ringbuffer::RingBuffer as _;

            macro_rules! read_madr {
                ($io:expr, $chan:ident $(,)?) => {
                    $io.dma.chan.$chan.txfer.cpu_bus_base
                };
            }

            macro_rules! write_madr {
                ($io:expr, $chan:ident, $value:expr $(,)?) => {
                    // The top byte is zeroed. This means that DMAs can only access RAM.
                    $io.dma.chan.$chan.txfer.cpu_bus_base = $value & 0x00ff_ffff;
                };
            }

            macro_rules! read_bcr {
                ($io:expr, $chan:ident $(,)?) => {
                    $io.dma.chan.$chan.txfer.sync_cfg
                };
            }

            macro_rules! write_bcr {
                ($io:expr, $chan:ident, $value:expr $(,)?) => {
                    $io.dma.chan.$chan.txfer.sync_cfg = $value;
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
                pub fn decode_dpcr(&mut self, mut code: u32) {
                    macro_rules! decode_chan {
                        ($chan:ident) => {
                            self.chan.$chan.prio = code.pop_bits(3);
                            self.chan.$chan.is_enabled = code.pop_bool();
                        };
                    }

                    decode_chan!(mdec_in);
                    decode_chan!(mdec_out);
                    decode_chan!(gpu);
                    decode_chan!(cdrom);
                    decode_chan!(spu);
                    decode_chan!(pio);
                    decode_chan!(otc);
                }

                pub fn encode_dpcr(&self) -> u32 {
                    let mut code = 0;

                    macro_rules! encode_chan {
                        ($chan:ident) => {
                            code.push_bool(self.chan.$chan.is_enabled);
                            code.push_bits(3, self.chan.$chan.prio);
                        };
                    }

                    encode_chan!(otc);
                    encode_chan!(pio);
                    encode_chan!(spu);
                    encode_chan!(cdrom);
                    encode_chan!(gpu);
                    encode_chan!(mdec_out);
                    encode_chan!(mdec_in);

                    code
                }

                pub fn next_transfer_packet<'int, 'ext>(
                    &'int mut self,
                    int: &'ext mut super::int::Sources,
                    ram: &'ext mut crate::bus::Ram,
                    gpu: &'ext mut noctane_gpu::Gpu,
                ) -> Option<TransferPacket> {
                    Channels::find_highest_prio(self.chan.in_transfer()).and_then(|chan| {
                        chan.next_transfer_packet(int, ram, gpu)
                    })
                }
            }

            impl Default for Channels {
                fn default() -> Self {
                    Self {
                        mdec_in: Channel::new(
                            0,
                            |_| {
                                todo!()
                            },
                            |_| {
                                todo!()
                            },
                            |_| {
                                todo!()
                            },
                            |_| {
                                todo!()
                            },
                            |_| {
                                todo!()
                            },
                            |_| {
                                todo!()
                            },
                        ),
                        mdec_out: Channel::new(
                            1,
                            |_| {
                                todo!()
                            },
                            |_| {
                                todo!()
                            },
                            |_| {
                                todo!()
                            },
                            |_| {
                                todo!()
                            },
                            |_| {
                                todo!()
                            },
                            |_| {
                                todo!()
                            },
                        ),
                        gpu: Channel::new(
                            2,
                            |_| {
                                todo!()
                            },
                            |_| {
                                todo!()
                            },
                            |_| {
                                todo!()
                            },
                            |_| {
                                todo!()
                            },
                            |mut params| {
                                tracing::info!("Sending GPU block sequence ({})...", params.count);
                                let start_addr = params.txfer.cpu_bus_base;
                                // TODO: Don't cast here.
                                let start_idx = (start_addr / 4) as usize;

                                let block_len = params.len;

                                // TODO: Respect transfer direction.
                                for block_idx in (0..params.count)
                                    .into_iter()
                                    .map(|i| start_idx + ((i * block_len) as usize))
                                {
                                    tracing::info!("DMA block start");
                                    for word_idx in (0..params.len)
                                        .into_iter()
                                        .map(|i| {
                                            // TODO: Don't cast here.
                                            block_idx + (i as usize)
                                        })
                                    {
                                        if let Some(word) = params.ram.get(word_idx).copied() {
                                            let _ = params.gpu.queue_gp0_word(word);
                                        } else {
                                            todo!()
                                        }
                                    }
                                }
                            },
                            |mut params| {
                                tracing::info!("Sending GPU command list...");
                                let mut next_entry_addr = params.txfer.cpu_bus_base;

                                // TODO: Respect transfer direction.
                                while next_entry_addr != 0xffffff {
                                    // TODO: Don't immediately execute.
                                    while !params.gpu.gp0().queue.is_empty() {
                                        params.gpu.execute_next_gp0_command();
                                    }

                                    let entry_addr = next_entry_addr;
                                    // TODO: Don't cast here.
                                    let entry_idx = (entry_addr / 4) as usize;
                                    tracing::debug!(
                                        "List entry: {:010x} ({})",
                                        entry_addr,
                                        entry_idx,
                                    );

                                    if let Some(mut entry_header) = params
                                        .ram
                                        .get(entry_idx)
                                        .copied()
                                    {
                                        next_entry_addr = entry_header.pop_bits(24);
                                        // TODO: Probably shouldn't cast here, but what's the chance
                                        // we're running on an 8-bit or 16-bit system?
                                        let cmd_count = entry_header as usize;

                                        for cmd_idx in (entry_idx.wrapping_add(1)..)
                                            .into_iter()
                                            .take(cmd_count)
                                        {
                                            if let Some(cmd) = params.ram.get(cmd_idx).copied() {
                                                if params.gpu.queue_gp0_word(cmd).is_err() {
                                                    // The queue is full. We'll stop here.
                                                    tracing::debug!("Queue is full");
                                                    todo!()
                                                }
                                            } else {
                                                // Error!
                                                todo!()
                                            }
                                        }
                                    } else {
                                        // Error!
                                        todo!()
                                    }
                                }
                            },
                        ),
                        cdrom: Channel::new(
                            3,
                            |_| {
                                todo!()
                            },
                            |_| {
                                todo!()
                            },
                            |_| {
                                todo!()
                            },
                            |_| {
                                todo!()
                            },
                            |_| {
                                todo!()
                            },
                            |_| {
                                todo!()
                            },
                        ),
                        spu: Channel::new(
                            4,
                            |_| {
                                todo!()
                            },
                            |_| {
                                todo!()
                            },
                            |_| {
                                todo!()
                            },
                            |_| {
                                todo!()
                            },
                            |_| {
                                todo!()
                            },
                            |_| {
                                todo!()
                            },
                        ),
                        pio: Channel::new(
                            5,
                            |_| {
                                todo!()
                            },
                            |_| {
                                todo!()
                            },
                            |_| {
                                todo!()
                            },
                            |_| {
                                todo!()
                            },
                            |_| {
                                todo!()
                            },
                            |_| {
                                todo!()
                            },
                        ),
                        otc: Channel::new(
                            6,
                            |mut params| {
                                tracing::info!("Init. OT of length {}...", params.count);
                                let mut next_entry_addr = params.txfer.cpu_bus_base;
                                for i in 1..=(params.count) {
                                    let entry_addr = next_entry_addr;
                                    next_entry_addr = if i == params.count {
                                        0xffffff
                                    } else {
                                        entry_addr.wrapping_sub(4)
                                    };

                                    let entry_idx = (entry_addr / 4) as usize;
                                    if let Some(entry_header) = params.ram.get_mut(entry_idx) {
                                        entry_header.push_bits(8, 0);
                                        entry_header.push_bits(24, next_entry_addr);
                                    } else {
                                        todo!()
                                    }
                                }
                            },
                            |_| {
                                todo!()
                            },
                            |_| {
                                todo!()
                            },
                            |_| {
                                todo!()
                            },
                            |_| {
                                todo!()
                            },
                            |_| {
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
                fn in_transfer<'a>(
                    &'a mut self,
                ) -> impl 'a + Iterator<Item = &'a mut Channel> {
                    [
                        &mut self.mdec_in,
                        &mut self.mdec_out,
                        &mut self.gpu,
                        &mut self.cdrom,
                        &mut self.spu,
                        &mut self.pio,
                        &mut self.otc,
                    ]
                    .into_iter()
                    .filter(|chan| chan.is_enabled && chan.txfer.current.is_some())
                }
            }

            impl Channels {
                fn find_highest_prio<'a>(
                    mut in_transfer: impl 'a + Iterator<Item = &'a mut Channel>,
                ) -> Option<&'a mut Channel> {
                    let Some(mut best_chan) = in_transfer.next() else {
                        return None;
                    };

                    while let Some(chan) = in_transfer.next() {
                        // We use `<=` because, if two channels are of equal priority, then the
                        // channel with the higher number (i.e. OTC is highest) is selected. Because
                        // of [`Self::in_transfer`], we are guaranteed to be iterating from the
                        // least to highest numbered channels.
                        if chan.prio <= best_chan.prio {
                            best_chan = chan;
                        }
                    }

                    Some(best_chan)
                }
            }

            macro_rules! impl_deref_for_txfer_params {
                ($ty:ident) => {
                    impl<'int, 'ext> std::ops::Deref for $ty<'int, 'ext> {
                        type Target = TransferParameters<'int, 'ext>;

                        fn deref(&self) -> &Self::Target {
                            &self.inner
                        }
                    }

                    impl<'int, 'ext> std::ops::DerefMut for $ty<'int, 'ext> {
                        fn deref_mut(&mut self) -> &mut Self::Target {
                            &mut self.inner
                        }
                    }
                };
            }

            pub struct TransferWordsParameters<'int, 'ext> {
                inner: TransferParameters<'int, 'ext>,
                pub count: u16,
            }
            impl_deref_for_txfer_params!(TransferWordsParameters);

            pub struct TransferBlockParameters<'int, 'ext> {
                inner: TransferParameters<'int, 'ext>,
                pub len: u16,
                pub count: u16,
            }
            impl_deref_for_txfer_params!(TransferBlockParameters);

            pub struct TransferListParameters<'int, 'ext> {
                inner: TransferParameters<'int, 'ext>,
            }
            impl_deref_for_txfer_params!(TransferListParameters);

            pub struct TransferParameters<'int, 'ext> {
                pub txfer: &'int Transfer,
                pub ram: &'ext mut crate::bus::Ram,
                pub gpu: &'ext mut noctane_gpu::Gpu,
            }

            impl Channel {
                fn new(
                    index: usize,
                    read_words: fn(TransferWordsParameters),
                    read_block: fn(TransferBlockParameters),
                    read_list: fn(TransferListParameters),
                    write_words: fn(TransferWordsParameters),
                    write_block: fn(TransferBlockParameters),
                    write_list: fn(TransferListParameters),
                ) -> Self {
                    Self {
                        index,
                        is_enabled: false,
                        prio: 0,
                        txfer: TransferState::default(),
                        read_words,
                        read_block,
                        read_list,
                        write_words,
                        write_block,
                        write_list,
                    }
                }
            }

            impl fmt::Debug for Channel {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    f
                        .debug_struct("Channel")
                        .field("index", &self.index)
                        .field("is_enabled", &self.is_enabled)
                        .field("prio", &self.prio)
                        .field("txfer", &self.txfer)
                        .finish()
                }
            }

            pub struct Channel {
                pub index: usize,
                pub is_enabled: bool,
                pub prio: u32,
                pub txfer: TransferState,
                pub read_words: fn(TransferWordsParameters),
                pub read_block: fn(TransferBlockParameters),
                pub read_list: fn(TransferListParameters),
                pub write_words: fn(TransferWordsParameters),
                pub write_block: fn(TransferBlockParameters),
                pub write_list: fn(TransferListParameters),
            }

            impl Channel {
                pub fn next_transfer_packet<'int, 'ext>(
                    &'int mut self,
                    int: &'ext mut super::int::Sources,
                    ram: &'ext mut crate::bus::Ram,
                    gpu: &'ext mut noctane_gpu::Gpu,
                ) -> Option<TransferPacket> {
                    if let Some(txfer) = self.txfer.current.as_ref() {
                        // TODO:
                        //
                        // The current approach is simply to complete a transfer in one cycle,
                        // but that is orders of magnitudes faster than the PSX does it. In the
                        // future, we should strive to split transfers into packets.

                        let params = TransferParameters {
                            txfer,
                            ram,
                            gpu,
                        };
                        match txfer.strat {
                            SyncStrategy::Wordwise { count } => {
                                let func = match txfer.source {
                                    TransferSource::External => self.read_words,
                                    TransferSource::CpuBus => self.write_words,
                                };
                                (func)(TransferWordsParameters {
                                    inner: params,
                                    count,
                                });
                            }
                            SyncStrategy::Blockwise { len, count } => {
                                let func = match txfer.source {
                                    TransferSource::External => self.read_block,
                                    TransferSource::CpuBus => self.write_block,
                                };
                                (func)(TransferBlockParameters {
                                    inner: params,
                                    len,
                                    count,
                                });
                            }
                            SyncStrategy::Listwise => {
                                let func = match txfer.source {
                                    TransferSource::External => self.read_list,
                                    TransferSource::CpuBus => self.write_list,
                                };
                                (func)(TransferListParameters {
                                    inner: params,
                                });
                            }
                        }

                        // The transfer is complete.
                        self.txfer.current = None;

                        if int.dma.chan[self.index].is_enabled {
                            // We must set both of these.
                            int.dma.request = Some(super::int::Request::default());
                            int.dma.chan[self.index].is_requesting = true;
                        }
                    }

                    None
                }

                pub fn decode_chcr(&mut self, mut code: u32) {
                    self.txfer.source = TransferSource::decode(code.pop_bits(1));
                    self.txfer.dir = TransferDirection::decode(code.pop_bits(1));
                    // Always zero.
                    code.pop_bits(6);
                    // TODO: Chopping Enable.
                    let _ = code.pop_bool();
                    self.txfer.sync_mode = code.pop_bits(2);
                    // Always zero.
                    code.pop_bits(5);
                    // TODO: Chopping DMA Window Size.
                    let _ = code.pop_bits(3);
                    // Always zero.
                    code.pop_bits(1);
                    // TODO: Chopping CPU Window Size.
                    let _ = code.pop_bits(3);
                    // Always zero.
                    code.pop_bits(1);
                    // Start/Busy.
                    if code.pop_bool() {
                        if self.txfer.current.is_none() {
                            self.txfer.current = Some(self.txfer.generate());
                        }
                    } else {
                        self.txfer.current = None;
                    };
                    // Always zero.
                    code.pop_bits(3);
                    // Start/trigger.
                    if code.pop_bool() {
                        self.is_enabled = true;
                    }
                    // TODO
                    let _ = code.pop_bits(2);
                    // Always zero.
                    code.pop_bits(1);
                }

                pub fn encode_chcr(&self) -> u32 {
                    let mut code = 0;
                    // TODO: Start/Trigger.
                    //
                    // This is cleared when a transfer starts, but setting it starts a transfer, so
                    // it is effectively always 0.
                    code.push_bool(false);
                    // Always zero.
                    code.push_bits(3, 0);
                    // TODO: Start/Busy.
                    code.push_bool(false);
                    // Always zero.
                    code.push_bits(1, 0);
                    // TODO: Chopping CPU Window Size.
                    code.push_bits(3, 0);
                    // Always zero.
                    code.push_bits(1, 0);
                    // TODO: Chopping DMA Window Size.
                    code.push_bits(3, 0);
                    // Always zero.
                    code.push_bits(5, 0);
                    code.push_bits(2, self.txfer.sync_mode);
                    // TODO: Chopping Enable.
                    code.push_bool(false);
                    // Always zero.
                    code.push_bits(6, 0);
                    code.push_bits(1, self.txfer.dir.encode());
                    code.push_bits(1, self.txfer.source.encode());

                    code
                }
            }

            #[derive(Clone, Copy, Debug, Default)]
            pub struct TransferState {
                pub current: Option<Transfer>,
                pub source: TransferSource,
                pub dir: TransferDirection,
                pub cpu_bus_base: u32,
                pub sync_mode: u32,
                pub sync_cfg: u32,
            }

            impl TransferState {
                pub fn start(&mut self) {
                    let current = self.generate();
                    self.current = Some(current);
                }

                pub fn generate(&self) -> Transfer {
                    Transfer {
                        source: self.source,
                        dir: self.dir,
                        cpu_bus_base: self.cpu_bus_base,
                        strat: SyncStrategy::decode(
                            self.sync_mode,
                            self.sync_cfg,
                        ),
                    }
                }
            }

            #[derive(Clone, Copy, Debug)]
            pub struct Transfer {
                pub source: TransferSource,
                pub dir: TransferDirection,
                pub cpu_bus_base: u32,
                pub strat: SyncStrategy,
            }

            #[derive(Clone, Debug)]
            pub struct TransferPacket {
                pub source: TransferSource,
                /// The direction in which data is moved during the transfer.
                pub dir: TransferDirection,
                /// The starting address of the transfer on the CPU bus side.
                pub cpu_bus_base: u32,
                pub strat: SyncStrategy,
            }

            impl TransferSource {
                fn decode(value: u32) -> Self {
                    match value {
                        0 => Self::External,
                        1 => Self::CpuBus,
                        _ => unreachable!(),
                    }
                }
            }

            #[derive(Clone, Copy, Debug, Default)]
            pub enum TransferSource {
                #[default]
                CpuBus,
                External,
            }

            impl TransferSource {
                fn encode(self) -> u32 {
                    match self {
                        Self::External => 0,
                        Self::CpuBus => 1,
                    }
                }
            }

            impl TransferDirection {
                fn decode(value: u32) -> Self {
                    match value {
                        0 => Self::Forward,
                        1 => Self::Backward,
                        _ => unreachable!(),
                    }
                }
            }

            #[derive(Clone, Copy, Debug, Default)]
            pub enum TransferDirection {
                #[default]
                Forward,
                Backward,
            }

            impl TransferDirection {
                fn encode(self) -> u32 {
                    match self {
                        Self::Forward => 0,
                        Self::Backward => 1,
                    }
                }
            }

            impl SyncStrategy {
                pub fn decode(mode: u32, mut code: u32) -> Self {
                    match mode {
                        0 => Self::Wordwise {
                            count: code.pop_bits(16) as u16,
                        },
                        1 => Self::Blockwise {
                            len: code.pop_bits(16) as u16,
                            count: code.pop_bits(16) as u16,
                        },
                        2 => Self::Listwise,
                        3 => todo!(),
                        _  => unreachable!(),
                    }
                }
            }

            #[derive(Clone, Copy, Debug)]
            pub enum SyncStrategy {
                /// Sync by copying individual words.
                Wordwise {
                    /// The number of words to be copied.
                    count: u16,
                },
                /// Sync by copying blocks of words.
                Blockwise {
                    /// The number of words in a block.
                    len: u16,
                    /// The number of blocks to be copied.
                    count: u16,
                },
                /// Sync by copying a GPU command list.
                Listwise,
            }
        },
    },
    Lut {
        name: timers,
        base_addr: 0x0100,
        regs: [
            Register {
                name: DOTCLOCK_COUNTER,
                read_32: |_, io| {
                    timers::read_counter!(io, dotclock)
                },
                write_32: |_, io, value| {
                    timers::write_counter!(io, dotclock, value);
                },
            },
            Register {
                name: DOTCLOCK_MODE,
                read_32: |_, io| {
                    timers::read_mode!(io, dotclock)
                },
                write_32: |_, io, value| {
                    timers::write_mode!(io, dotclock, value);
                },
            },
            Register {
                name: DOTCLOCK_TARGET,
                read_32: |_, io| {
                    timers::read_target!(io, dotclock)
                },
                write_32: |_, io, value| {
                    timers::write_target!(io, dotclock, value);
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
                    timers::read_counter!(io, hblank)
                },
                write_32: |_, io, value| {
                    timers::write_counter!(io, hblank, value);
                },
            },
            Register {
                name: HBLANK_MODE,
                read_32: |_, io| {
                    timers::read_mode!(io, hblank)
                },
                write_32: |_, io, value| {
                    timers::write_mode!(io, hblank, value);
                },
            },
            Register {
                name: HBLANK_TARGET,
                read_32: |_, io| {
                    timers::read_target!(io, hblank)
                },
                write_32: |_, io, value| {
                    timers::write_target!(io, hblank, value);
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
                    timers::read_counter!(io, sysclock)
                },
                write_32: |_, io, value| {
                    timers::write_counter!(io, sysclock, value);
                },
            },
            Register {
                name: SYSCLOCK_MODE,
                read_32: |_, io| {
                    timers::read_mode!(io, sysclock)
                },
                write_32: |_, io, value| {
                    timers::write_mode!(io, sysclock, value);
                },
            },
            Register {
                name: SYSCLOCK_TARGET,
                read_32: |_, io| {
                    timers::read_target!(io, sysclock)
                },
                write_32: |_, io, value| {
                    timers::write_target!(io, sysclock, value);
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
            //! Timers (Root Counters).

            use noctane_util::{BitStack as _, BitStackExt as _};

            macro_rules! read_counter {
                ($io:expr, $timer:ident $(,)?) => {
                    timers::read_field!($io, $timer.counter)
                };
            }

            macro_rules! write_counter {
                ($io:expr, $timer:ident, $value:expr $(,)?) => {
                    timers::write_field!($io, $timer.counter, $value)
                };
            }

            macro_rules! read_mode {
                ($io:expr, $timer:ident $(,)?) => {
                    $io.timers.$timer.encode_mode(&$io.int)
                };
            }

            macro_rules! write_mode {
                ($io:expr, $timer:ident, $value:expr $(,)?) => {
                    $io.timers.$timer.decode_mode(&mut $io.int, $value);
                };
            }

            macro_rules! read_target {
                ($io:expr, $timer:ident $(,)?) => {
                    timers::read_field!($io, $timer.target)
                };
            }

            macro_rules! write_target {
                ($io:expr, $timer:ident, $value:expr $(,)?) => {
                    timers::write_field!($io, $timer.target, $value)
                };
            }

            macro_rules! read_field {
                ($io:expr, $timer:ident . $field:ident $(,)?) => {
                    $io.timers.$timer.$field as u32
                };
            }

            macro_rules! write_field {
                ($io:expr, $timer:ident . $field:ident, $value:expr $(,)?) => {
                    $io.timers.$timer.$field = $value as u16;
                };
            }

            pub(crate) use read_counter;
            pub(crate) use write_counter;

            pub(crate) use read_mode;
            pub(crate) use write_mode;

            pub(crate) use read_target;
            pub(crate) use write_target;

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
                pub fn increment(&mut self, int: &mut super::int::Sources) {
                    self.dotclock.increment(int);
                    self.hblank.increment(int);
                    self.sysclock.increment(int);
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
                        uses_target: false,
                        clock_source: ClockSource(0),
                        calc_increment_amount,
                    }
                }
            }

            #[derive(Debug)]
            pub struct Timer {
                pub index: usize,
                // Timer fields are supposed to be 32-bit but contain 16 bits of garbage, so they
                // are represented here with [`u16`]. Accordingly, the [`read_field`] and
                // [`write_field`] macros above perform the appropriate conversions between [`u32`]
                // and [`u16`].
                pub counter: u16,
                pub target: u16,

                pub uses_target: bool,
                pub clock_source: ClockSource,
                pub calc_increment_amount: fn(ClockSource) -> u16,
            }

            impl Timer {
                pub fn decode_mode(&mut self, int: &mut super::int::Sources, mut code: u32) {
                    let int_source = &mut int.timers[self.index];

                    // TODO
                    let _ = code.pop_bits(3);
                    self.uses_target = code.pop_bool();
                    int_source.is_enabled_on_target_hit = code.pop_bool();
                    int_source.is_enabled_on_overflow = code.pop_bool();
                    // TODO
                    let _ = code.pop_bits(2);
                    self.clock_source.0 = code.pop_bits(2);
                    int_source.request =
                        // This is inverted for some reason.
                        (!code.pop_bool()).then_some(super::int::Request::default());
                    // TODO
                }

                pub fn encode_mode(&self, int: &super::int::Sources) -> u32 {
                    let int_source = &int.timers[self.index];

                    let mut code = 0;
                    // Seems to always be zero.
                    code.push_bits(3, 0);
                    // TODO
                    code.push_bits(2, 0);
                    code.push_bool(!int_source.request.is_none());
                    code.push_bits(2, self.clock_source.0);
                    // TODO
                    code.push_bits(2, 0);
                    code.push_bool(int_source.is_enabled_on_overflow);
                    code.push_bool(int_source.is_enabled_on_target_hit);
                    code.push_bool(self.uses_target);
                    // TODO
                    code.push_bits(3, 0);

                    code
                }

                fn increment(&mut self, int: &mut super::int::Sources) {
                    let hit_target = self.uses_target && (self.counter >= self.target);
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
                    let mut should_request = int_source.is_enabled_on_target_hit && hit_target;
                    should_request |= int_source.is_enabled_on_overflow && counter_overflowed;
                    if should_request {
                        int_source.request = Some(super::int::Request::default());
                    }
                }
            }

            #[derive(Clone, Copy, Debug)]
            pub struct ClockSource(u32);
        },
    },
    Lut {
        name: cdrom,
        base_addr: 0x0800,
        regs: [
            Register {
                name: INDEX,
                read_8: |_, _, _| {
                    // TODO
                    !0
                },
                write_8: |_, _, _, _| {
                    // TODO
                },
            },
            Register {
                name: 1,
                read_8: |_, _, _| {
                    // TODO
                    !0
                },
                write_8: |_, _, _, _| {
                    // TODO
                },
            },
            Register {
                name: 2,
                read_8: |_, _, _| {
                    // TODO
                    !0
                },
                write_8: |_, _, _, _| {
                    // TODO
                },
            },
            Register {
                name: 3,
                read_8: |_, _, _| {
                    // TODO
                    !0
                },
                write_8: |_, _, _, _| {
                    // TODO
                },
            },
        ],
        module: {
            //! CD-ROM.
        },
    },
    Lut {
        name: gpu,
        base_addr: 0x0810,
        regs: [
            Register {
                name: 0,
                read_32: |_, io| {
                    *io.last_gpu_result
                },
                write_32: |_, io, word| {
                    let _ = io.gpu.queue_gp0_word(word);
                },
            },
            Register {
                name: 1,
                read_32: |_, io| {
                    use noctane_util::{BitStack as _, BitStackExt as _};
                    use ringbuffer::RingBuffer as _;

                    let mut code = 0;
                    // TODO: Draw even/odd lines in interlace mode.
                    code.push_bits(1, 0);
                    // TODO: DMA direction.
                    code.push_bits(2, 2);
                    // TODO: Ready to receive DMA block.
                    code.push_bool(true);
                    // TODO: Ready to send VRAM to CPU.
                    code.push_bool(true);
                    // Ready to receive command.
                    code.push_bool(!io.gpu.gp0().queue.is_full());
                    // TODO: DMA.
                    code.push_bits(1, 1);
                    // TODO: IRQ.
                    code.push_bool(true);
                    // Display Enable.
                    code.push_bool(!io.gpu.display().is_enabled);
                    // TODO: Vertical Interlace.
                    code.push_bool(false);
                    // TODO: Display Area Color Depth.
                    code.push_bits(1, 0);
                    // TODO: Video Mode.
                    code.push_bits(1, 0);
                    // TODO: Vertical Resolution.
                    code.push_bits(1, 0);
                    // TODO: Horizontal Resolution 1.
                    code.push_bits(2, 0);
                    // TODO: Horizontal Resolution 2.
                    code.push_bits(1, 0);
                    // TODO: Texture Disable.
                    code.push_bool(false);
                    // TODO: Reverseflag.
                    code.push_bool(false);
                    // TODO: Interlace Field.
                    code.push_bits(1, 0);
                    // TODO: Draw Pixels.
                    code.push_bits(1, 0);
                    // TODO: Set Mask-bit when drawing pixels.
                    code.push_bool(false);
                    // TODO: Drawing to display area.
                    code.push_bool(true);
                    // TODO
                    code.push_bits(10, 0);

                    code
                },
                write_32: |_, io, mach| {
                    io.gpu.execute_gp1_machine_command(mach);
                },
            },
        ],
        module: {
            //! Graphics Processing Unit (GPU).
        },
    },
    Lut {
        name: mdec,
        base_addr: 0x0820,
        regs: [
            Register {
                name: CHAN,
                read_32: |_, _| {
                    todo!()
                },
                write_32: |_, _, _| {
                    todo!()
                },
            },
            Register {
                name: CTRL,
                read_32: |_, _| {
                    todo!()
                },
                write_32: |_, _, _| {
                    todo!()
                },
            },
        ],
        module: {
            //! MDEC.
        },
    },
    Lut {
        name: spu_voice,
        base_addr: 0x0c00,
        regs: [
            Register {
                name: L_VOL,
                read_16: |i: usize, _, io: &Io, _| {
                    io.spu_voices[i].vol.left
                },
                write_16: |i: usize, _, io: &mut Io, _, value| {
                    io.spu_voices[i].vol.left = value;
                },
            },
            Register {
                name: R_VOL,
                read_16: |i: usize, _, io: &Io, _| {
                    io.spu_voices[i].vol.right
                },
                write_16: |i: usize, _, io: &mut Io, _, value| {
                    io.spu_voices[i].vol.right = value;
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
                read_32: |_, _, _| {
                    // TODO: Don't care right now.
                    0
                },
                write_32: |_, _, _, _| {
                    // TODO
                },
            },
            Register {
                name: ADSR_CUR_VOL,
                read_16: |i: usize, _, io: &Io, _| {
                    io.spu_voices[i].cur_vol
                },
                write_16: |i: usize, _, io: &mut Io, _, value| {
                    io.spu_voices[i].cur_vol = value;
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
            //! SPU voices.

            #[derive(Debug, Default)]
            pub struct Config {
                pub is_enabled: bool,
                pub key: Key,
                pub vol: super::spu::StereoVolume,
                pub adpcm_sample_rate: u16,
                pub adpcm_base_addr: u16,
                pub cur_vol: u16,
                pub rep_addr: u16,
            }

            #[derive(Debug, Default)]
            pub struct Key {
                pub state: KeyState,
                pub attack: Attack,
                pub decay: Decay,
                pub sustain: Sustain,
                pub release: Release,
            }

            #[derive(Clone, Copy, Debug, Default)]
            pub enum KeyState {
                #[default]
                Off,
                On,
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
    Lut {
        name: spu,
        base_addr: 0x0d80,
        regs: [
            Register {
                name: L_MAIN_VOL,
                read_16: |_, io, _| {
                    io.spu.vol.main.left
                },
                write_16: |_, io, _, value| {
                    io.spu.vol.main.left = value;
                },
            },
            Register {
                name: R_MAIN_VOL,
                read_16: |_, io, _| {
                    io.spu.vol.main.right
                },
                write_16: |_, io, _, value| {
                    io.spu.vol.main.right = value;
                },
            },
            Register {
                name: L_REVERB_VOL,
                read_16: |_, io, _| {
                    io.spu.vol.reverb.left
                },
                write_16: |_, io, _, value| {
                    io.spu.vol.reverb.left = value;
                },
            },
            Register {
                name: R_REVERB_VOL,
                read_16: |_, io, _| {
                    io.spu.vol.reverb.right
                },
                write_16: |_, io, _, value| {
                    io.spu.vol.reverb.right = value;
                },
            },
            Register {
                name: KEY_ON,
                read_32: |_, _| {
                    // TODO: I think this is unreadable (?).
                    0
                },
                write_32: |_, io, value| {
                    io.spu.decode_kon(value);
                },
            },
            Register {
                name: KEY_OFF,
                read_32: |_, _| {
                    // TODO: I think this is unreadable (?).
                    0
                },
                write_32: |_, io, value| {
                    io.spu.decode_koff(value);
                },
            },
            // I don't care about these four registers right now.
            Register {
                name: CHAN_FM,
                read_32: |_, _| {
                    // TODO
                    0
                },
                write_32: |_, _, _| {
                    // TODO
                },
            },
            Register {
                name: CHAN_NOISE,
                read_32: |_, _| {
                    // TODO
                    0
                },
                write_32: |_, _, _| {
                    // TODO
                },
            },
            Register {
                name: CHAN_REVERB,
                read_32: |_, _| {
                    // TODO
                    0
                },
                write_32: |_, _, _| {
                    // TODO
                },
            },
            Register {
                name: CHAN_STAT,
                read_32: |_, _| {
                    // TODO
                    0
                },
                write_32: |_, _, _| {
                    // TODO
                },
            },
            Register {
                name: 8,
                read_16: |_, _, _| {
                    // TODO: Undocumented.
                    0
                },
                write_16: |_, _, _, _| {
                    // TODO: Undocumented.
                },
            },
            Register {
                name: REVERB_WRAM_BASE,
                read_16: |_, io, _| {
                    io.spu.reverb_wram_base
                },
                write_16: |_, io, _, value| {
                    io.spu.reverb_wram_base = value;
                },
            },
            Register {
                name: IRQ_IDX,
                read_16: |_, io, _| {
                    io.spu.irq_idx
                },
                write_16: |_, io, _, value| {
                    io.spu.irq_idx = value;
                },
            },
            Register {
                name: DATA_TXFR_IDX,
                read_16: |_, io, _| {
                    io.spu.txfr.idx
                },
                write_16: |_, io, _, value| {
                    io.spu.txfr.idx = value;
                },
            },
            Register {
                name: DATA_TXFR_FIFO,
                read_16: |_, io, _| {
                    io.spu.txfr.fifo
                },
                write_16: |_, io, _, value| {
                    io.spu.txfr.fifo = value;
                },
            },
            Register {
                name: CTRL,
                read_16: |_, io, _| {
                    io.spu.encode_ctrl()
                },
                write_16: |_, io, _, value| {
                    io.spu.decode_ctrl(value);
                },
            },
            Register {
                name: DATA_TXFR_CTRL,
                read_16: |_, io, _| {
                    io.spu.txfr.encode_ctrl()
                },
                write_16: |_, io, _, value| {
                    io.spu.txfr.decode_ctrl(value);
                },
            },
            Register {
                name: STAT,
                read_16: |_, io, _| {
                    // TODO: Don't care right now.
                    0
                },
                write_16: |_, io, _, value| {
                    // TODO: Don't care right now.
                },
            },
            Register {
                name: L_CD_VOL,
                read_16: |_, io, _| {
                    io.spu.vol.cd.left
                },
                write_16: |_, io, _, value| {
                    io.spu.vol.cd.left = value;
                },
            },
            Register {
                name: R_CD_VOL,
                read_16: |_, io, _| {
                    io.spu.vol.cd.right
                },
                write_16: |_, io, _, value| {
                    io.spu.vol.cd.right = value;
                },
            },
            Register {
                name: L_EXT_VOL,
                read_16: |_, io, _| {
                    io.spu.vol.ext.left
                },
                write_16: |_, io, _, value| {
                    io.spu.vol.ext.left = value;
                },
            },
            Register {
                name: R_EXT_VOL,
                read_16: |_, io, _| {
                    io.spu.vol.ext.right
                },
                write_16: |_, io, _, value| {
                    io.spu.vol.ext.right = value;
                },
            },
            Register {
                name: L_CUR_MAIN_VOL,
                read_16: |_, io, _| {
                    io.spu.vol.cur_main.left
                },
                write_16: |_, io, _, value| {
                    io.spu.vol.cur_main.left = value;
                },
            },
            Register {
                name: R_CUR_MAIN_VOL,
                read_16: |_, io, _| {
                    io.spu.vol.cur_main.right
                },
                write_16: |_, io, _, value| {
                    io.spu.vol.cur_main.right = value;
                },
            },
            Register {
                name: 19,
                read_32: |_, _| {
                    // TODO: Undocumented.
                    0
                },
                write_32: |_, _, _| {
                    // TODO: Undocumented.
                },
            },
        ],
        module: {
            //! SPU configuration.

            use noctane_util::{BitStack as _, BitStackExt as _};

            #[derive(Clone, Debug, Default)]
            pub struct Config {
                pub is_enabled: bool,
                pub is_muted: bool,
                pub vol: VolumeConfig,
                pub irq_idx: u16,
                pub txfr: TransferState,
                pub reverb_wram_base: u16,
            }

            impl Config {
                pub fn decode_ctrl(&mut self, mut code: u16) {
                    // TODO
                    let _ = code.pop_bits(14);
                    self.is_muted = !code.pop_bool();
                    self.is_enabled = code.pop_bool();
                }

                pub fn encode_ctrl(&self) -> u16 {
                    let mut code = 0;
                    code.push_bool(self.is_enabled);
                    code.push_bool(!self.is_muted);
                    // TODO

                    code
                }

                // TODO: Move these two methods to SPU voices somehow. Maybe make SPU voices a
                // newtype around an array.

                pub fn decode_kon(&mut self, mut code: u32) {
                    // TODO
                }

                pub fn decode_koff(&mut self, mut code: u32) {
                    // TODO
                }
            }

            #[derive(Clone, Debug, Default)]
            pub struct VolumeConfig {
                pub main: StereoVolume,
                pub reverb: StereoVolume,
                pub cd: StereoVolume,
                pub ext: StereoVolume,
                pub cur_main: StereoVolume,
            }

            #[derive(Clone, Copy, Debug, Default)]
            pub struct StereoVolume {
                /// The volume level of the left channel.
                pub left: u16,
                /// The volume level of the right channel.
                pub right: u16,
            }

            #[derive(Clone, Debug, Default)]
            pub struct TransferState {
                pub idx: u16,
                pub fifo: u16,
            }

            impl TransferState {
                pub fn decode_ctrl(&mut self, mut code: u16) {
                    // TODO
                }

                pub fn encode_ctrl(&self) -> u16 {
                    // TODO
                    0
                }
            }
        },
    },
    // SPU Reverb Configuration.
    // SPU Internal.
    Lut {
        name: atcons,
        base_addr: 0x1000,
        regs: [
            Register {
                name: STAT,
                read_8: |_, _, _| {
                    todo!()
                },
                write_8: |_, _, _, _| {
                    todo!()
                },
            },
            Register {
                name: 1,
                read_8: |_, _, _| {
                    todo!()
                },
                write_8: |_, _, _, _| {
                    todo!()
                },
            },
            Register {
                name: DATA,
                read_8: |_, _, _| {
                    todo!()
                },
                write_8: |_, _, _, _| {
                    todo!()
                },
            },
            Register {
                name: 3,
                read_8: |_, _, _| {
                    todo!()
                },
                write_8: |_, _, _, _| {
                    todo!()
                },
            },
            Register {
                name: 4,
                read_16: |_, _, _| {
                    todo!()
                },
                write_16: |_, _, _, _| {
                    todo!()
                },
            },
        ],
        module: {
            //! ATCONS (?).
        },
    },
    Lut {
        name: duart,
        base_addr: 0x1020,
        regs: [
            Register {
                name: MODE_0,
                read_8: |_, _, _| {
                    todo!()
                },
                write_8: |_, _, _, _| {
                    todo!()
                },
            },
            Register {
                name: STAT_0,
                read_8: |_, _, _| {
                    todo!()
                },
                write_8: |_, _, _, _| {
                    todo!()
                },
            },
            Register {
                name: 3,
                read_8: |_, _, _| {
                    todo!()
                },
                write_8: |_, _, _, _| {
                    todo!()
                },
            },
            Register {
                name: FIFO_0,
                read_8: |_, _, _| {
                    todo!()
                },
                write_8: |_, _, _, _| {
                    todo!()
                },
            },
            Register {
                name: 4,
                read_8: |_, _, _| {
                    todo!()
                },
                write_8: |_, _, _, _| {
                    todo!()
                },
            },
            // TODO
        ],
        module: {
            //! Dual serial port (DUART).

            // TODO
        },
    },
    Lut {
        name: x1030,
        base_addr: 0x1030,
        regs: [
            // Who knows what these do!
            Register {
                name: LIGHTPEN,
                read_16: |_, _, _| {
                    todo!()
                },
                write_16: |_, _, _, _| {
                    todo!()
                },
            },
            Register {
                name: CTRL,
                read_8: |_, _, _| {
                    todo!()
                },
                write_8: |_, _, _, _| {
                    todo!()
                },
            },
        ],
        module: {
            // TODO
        }
    },
    Lut {
        name: post,
        base_addr: 0x1040,
        regs: [
            Register {
                name: BOOT_MODE,
                read_8: |_, _, _| {
                    todo!()
                },
                write_8: |_, _, _, _| {
                    todo!()
                },
            },
            Register {
                name: SEVEN_SEG,
                read_8: |_, io, _| {
                    // TODO: I don't know if this is right.
                    io.post.seven_seg.last().copied().unwrap_or(0)
                },
                write_8: |_, io, _, value| {
                    io.post.seven_seg.push(value as u8);
                },
            },
            Register {
                name: LED,
                read_8: |_, io, _| {
                    // TODO: I don't know if this is right.
                    io.post.led.last().copied().unwrap_or(0)
                },
                write_8: |_, io, _, value| {
                    io.post.led.push(value as u8);
                },
            },
        ],
        module: {
            //! POST status.

            /// POST status.
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
