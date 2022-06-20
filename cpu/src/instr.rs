// SPDX-License-Identifier: MPL-2.0

//! CPU instructions.
//!
//! The PSX CPU is compatible with the 32-bit MIPS ISA. The exact composition of an instruction is
//! explained in-depth in the [`decode`] module, but briefly, each instruction is tagged with an
//! identifying opcode, and the remaining bits are used to store parameters.
//!
//! # A Brief Note on Terminology
//!
//! Noctane uses 'instruction' to mean the application of operands to an operation and operands. A
//! *machine* instruction is an instruction in its encoded, machine-readable form.
//!
//! # Processing
//!
//! When the [intruction pipeline](Pipeline) is advanced, the following occurs:
//! - A new machine instruction is fetched at the program address represented by the program counter
//!   (PC) register.
//! - The instruction is decoded using [`Instr::decode`].
//! - The registers whose values are necessary for the execution of the instruction are fetched.
//! - The instruction is finally executed.
//! - If the instruction modified the PC&mdash;meaning it is a 'jump' or 'branch' instruction
//!   &mdash;the instruction following the one just executed, or that which is in the 'delay slot',
//!   is prepped to be executed in the next pipeline advancement, after which execution will proceed
//!   to the target location.
//! - Otherwise, the PC is simply incremented by the size of one instruction such that it points
//!   to the next instruction.

pub mod asm;

pub use asm::Asm;
pub use opx::Operation;

use crate::{exc, reg, Memory};

/// A 5-stage instruction pipeline.
///
/// A defining feature of the MIPS architecture is pipelined instruction execution. 5 instructions
/// may be queued concurrently, each in one of the 5 pipestages:
///
/// 1. Instruction Fetch (IF)
/// 2. Read and Decode (RD)
/// 3. Arithmetic/Logic Unit (ALU)
/// 4. Memory access (MEM)
/// 5. Write Back (WB)
///
/// This structure is the queue that holds these 5 instructions.
#[derive(Default)]
pub struct Pipeline {
    /// Information pertaining to potential upcoming jumps.
    upcoming_jump: Option<PipelineJump>,
}

/// Information pertaining to a jump instruction within a [`Pipeline`].
pub struct PipelineJump {
    /// The target address.
    pub target_addr: u32,
    /// The instruction fetch following the jump instruction.
    ///
    /// This is commonly known as the 'delay slot'.
    pub delay_slot: Fetched,
}

/// The result of an instruction fetch.
#[derive(Clone, Debug)]
pub struct Fetched {
    /// The program address at which the fetched instruction is located.
    pub addr: u32,
    /// The original opcode of the instruction fetched.
    pub op: u32,
    /// The instruction fetched.
    pub instr: Instr,
}

/// The result of an instruction execution.
#[derive(Clone, Debug)]
pub struct Executed {
    /// The instruction that was fetched and eventually executed.
    pub fetched: Fetched,
    /// The behavior of the program counter (PC) after this instruction was executed.
    pub pc_behavior: PcBehavior,
}

/// The behavior of a CPU's program counter (PC) after an instruction is executed.
#[derive(Clone, Copy, Debug)]
pub enum PcBehavior {
    /// The CPU jumps to a new program address.
    Jumps {
        /// The kind of jump.
        kind: JumpKind,
        /// The target program address.
        target_addr: u32,
    },
    /// The CPU increments the PC by the size of one instruction.
    Increments,
}

/// A kind of jump.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum JumpKind {
    /// This is a function call.
    ///
    /// Once this function ends, execution is returned to the function immediately above it in the
    /// callstack.
    Call,
    /// This jumps to an exception vector.
    Exception,
    /// This returns from a function.
    Return,
    /// This is a normal jump to a new program address in which execution is never automatically
    /// returned to the previous PC address.
    WithoutReturn,
}

impl PcBehavior {
    /// Creates a new `PcBehavior` that jumps without return to the given target address.
    fn jumps_without_return(target_addr: u32) -> Self {
        Self::Jumps { kind: JumpKind::WithoutReturn, target_addr }
    }

    /// Creates a new `PcBehavior` that jumps to the given target address with the intention of
    /// returning.
    fn calls(target_addr: u32) -> Self {
        Self::Jumps { kind: JumpKind::Call, target_addr }
    }

    /// Creates a new `PcBehavior` that returns from a function.
    fn returns(target_addr: u32) -> Self {
        Self::Jumps { kind: JumpKind::Return, target_addr }
    }
}

impl Pipeline {
    /// Advances each instruction contained within this pipeline to the next stage of execution.
    ///
    /// The `decode_instr` argument decodes a given opcode into an instruction.
    pub fn advance(
        &mut self,
        mem: &mut Memory,
        reg: &mut reg::File,
        decode_instr: &impl Fn(u32) -> Instr,
    ) -> Executed {
        // As it turns out, we don't actually have to implement a traditional pipeline here. The
        // magic lies in the fact that the only programmer-visible effect of pipelining is the delay
        // slot, which we can handle specially.

        if let Some(jump) = self.upcoming_jump.take() {
            // The last instruction was a jump instruction. We will proceed by executing the
            // instruction in the delay slot before setting the PC to the target address.

            let exec = self.execute_fetched(
                jump.delay_slot,
                mem,
                reg,
                decode_instr,
            );
            *reg.pc_mut() = jump.target_addr;

            exec
        } else {
            let fetched = self.fetch_next_instr(mem, reg, decode_instr);
            self.execute_fetched(
                fetched,
                mem,
                reg,
                decode_instr,
            )
        }
    }

    /// Fetches and decodes the instruction pointed to by the program counter (PC).
    fn fetch_next_instr(
        &mut self,
        mem: &mut Memory,
        reg: &mut reg::File,
        decode_instr: &impl Fn(u32) -> Instr,
    ) -> Fetched {
        let pc = reg.pc();
        // Increment the PC.
        *reg.pc_mut() = pc.wrapping_add(4);

        // I'm not sure if this is really necessary, but we will continuously attempt to fetch the
        // next instruction, generating exceptions on every erroneous access.
        let op = loop {
            if let Ok(op) = mem.read_instr(pc) {
                // Success! Here's the operation.
                break op;
            }

            // TODO: We can be more efficient here by only raising an exception after a successful
            // read from the exception handler.
            reg.raise_exception(exc::code::ADDRESS_LOAD);
        };

        Fetched { addr: pc, op, instr: decode_instr(op) }
    }

    /// Executes the given fetched instruction.
    fn execute_fetched(
        &mut self,
        fetched: Fetched,
        mem: &mut Memory,
        reg: &mut reg::File,
        decode_instr: &impl Fn(u32) -> Instr,
    ) -> Executed {
        let mut state = opx::State::read(fetched.instr, reg);
        let pc_behavior = state.process(reg, mem);
        match &pc_behavior {
            PcBehavior::Increments => {
                // The PC was already incremented in [`Self::fetch_next_instr`], so we don't need to
                // do it here.
            }
            PcBehavior::Jumps { kind: JumpKind::Exception, target_addr } => {
                // To my knowledge, unlike other types of jumps, raising an exception does not
                // execute the instruction in the delay slot. We simply jump to the target address,
                // which should be the exception vector.
                *reg.pc_mut() = *target_addr;
            }
            PcBehavior::Jumps { target_addr, .. } => {
                // This is a jump instruction, and it is requesting to jump to the program address
                // identified by `target_addr`. Per the MIPS I architecture, the instruction in the
                // delay slot, or the instruction following the jump instruction, *must* be executed
                // before the branch is taken (if it is taken) (RM[1-7]).
                let delay_slot = self.fetch_next_instr(mem, reg, decode_instr);
                // We can't perform this jump now, so we will indicate that it should be performed
                // later.
                self.upcoming_jump = Some(PipelineJump { target_addr: *target_addr, delay_slot });
            }
        }

        // If the status register (SR) was modified, we need to apply the changes before returning
        // control back to the caller.
        if let Some(sr) = reg.altered_sr() {
            mem.cache_mut().i.set_isolated(sr.is_c());

            if sr.sw_c() {
                // This doesn't do anything in the PSX as far as I know. The I-cache is more-or-less
                // configured to function as a D-cache, so it is basically already 'swapped'.
            }

            // TODO: Process other SR fields.
        }

        Executed { fetched, pc_behavior }
    }
}

/// Defines [`Instr`], [`opx`], and [`opn`].
macro_rules! def_instr_and_op_kind {
    (
        $(
            {
                name: $variant_name:tt,
                type: $ty:tt,
                asm: [
                    $display_name:literal
                    $(
                        $kind:tt($($arg:tt)?)
                    ),* $(,)?
                ],
                fn: $fn:expr $(,)?
            } $(,)?
        ),*
    ) => {
        /// A CPU instruction.
        #[derive(Clone, Copy, Debug, Eq, PartialEq)]
        pub enum Instr {
            $(
                #[doc = concat!("A `", stringify!($variant_name), "` instruction.")]
                $variant_name(opn::$ty::Operands),
            )*
        }

        impl Instr {
            /// Decodes a CPU instrucion from its encoded, machine-readable form.
            ///
            /// This function may fail and return `None` if the code does not correspond to a valid
            /// instruction.
            pub fn decode(mach: u32) -> Option<Self> {
                match try_decode_opcode(mach)? {
                    $(
                        Operation::$variant_name => {
                            Some(Self::$variant_name(opn::$ty::Operands::decode(mach)))
                        }
                    )*
                }
            }
        }

        fn try_decode_opcode(mach: u32) -> Option<Operation> {
            const SPECIAL_OPCODE: u8 = 0;

            let opcode = decode_opcode(mach);

            if opcode == SPECIAL_OPCODE {
                let funct = decode_funct(mach);

                Operation::try_decode_special(funct)
            } else {
                Operation::try_decode_normal(opcode)
            }
        }

        macro_rules! def_decode_mach_part {
            ($fn_name:ident, $range:tt, $part_ty:ty) => {
                #[inline(always)]
                fn $fn_name(mach: u32) -> $part_ty {
                    ((mach & (!0 >> (32 - mach_bits::$range.end))) >> mach_bits::$range.start) as $part_ty
                }
            };
        }

        def_decode_mach_part!(decode_opcode, OPCODE, u8);
        def_decode_mach_part!(decode_target, TARGET, u32);
        def_decode_mach_part!(decode_imm, IMM, u16);
        def_decode_mach_part!(decode_rs, RS, u8);
        def_decode_mach_part!(decode_rt, RT, u8);
        def_decode_mach_part!(decode_rd, RD, u8);
        def_decode_mach_part!(decode_shamt, SHAMT, u8);
        def_decode_mach_part!(decode_funct, FUNCT, u8);

        mod mach_bits {
            use std::ops::Range;

            pub const FUNCT:    Range<usize> = 0..6;
            pub const SHAMT:    Range<usize> = 6..11;
            pub const RD:       Range<usize> = 11..16;
            pub const RT:       Range<usize> = 16..21;
            pub const RS:       Range<usize> = 21..26;
            pub const IMM:      Range<usize> = 0..16;
            pub const TARGET:   Range<usize> = 0..26;
            pub const OPCODE:   Range<usize> = 26..32;
        }

        impl Operation {
            fn try_decode_normal(code: u8) -> Option<Self> {
                match code {
                    0b000_000 => {
                        // Use [`Self::try_decode_special`] instead!
                        None
                    }
                    0b000_001 => Some(Self::BCond),
                    0b000_010 => Some(Self::J),
                    0b000_011 => Some(Self::Jal),
                    0b000_100 => Some(Self::Beq),
                    0b000_101 => Some(Self::Bne),
                    0b000_110 => Some(Self::Blez),
                    0b000_111 => Some(Self::Bgtz),

                    0b001_000 => Some(Self::Addi),
                    0b001_001 => Some(Self::Addiu),
                    0b001_010 => Some(Self::Slti),
                    0b001_011 => Some(Self::Sltiu),
                    0b001_100 => Some(Self::Andi),
                    0b001_101 => Some(Self::Ori),
                    0b001_110 => Some(Self::Xori),
                    0b001_111 => Some(Self::Lui),

                    0b010_000 => Some(Self::Cop0),
                    0b010_001 => Some(Self::Cop1),
                    0b010_010 => Some(Self::Cop2),
                    0b010_011 => Some(Self::Cop3),
                    0b010_100..=0b010_111 => None,

                    0b011_000..=0b011_111 => None,

                    0b100_000 => Some(Self::Lb),
                    0b100_001 => Some(Self::Lh),
                    0b100_010 => Some(Self::Lwl),
                    0b100_011 => Some(Self::Lw),
                    0b100_100 => Some(Self::Lbu),
                    0b100_101 => Some(Self::Lhu),
                    0b100_110 => Some(Self::Lwr),
                    0b100_111 => None,

                    0b101_000 => Some(Self::Sb),
                    0b101_001 => Some(Self::Sh),
                    0b101_010 => Some(Self::Swl),
                    0b101_011 => Some(Self::Sw),
                    0b101_100..=0b101_101 => None,
                    0b101_110 => Some(Self::Swr),
                    0b101_111 => None,

                    0b110_000 => Some(Self::Lwc0),
                    0b110_001 => Some(Self::Lwc1),
                    0b110_010 => Some(Self::Lwc2),
                    0b110_011 => Some(Self::Lwc3),
                    0b110_100..=0b110_111 => None,

                    0b111_000 => Some(Self::Swc0),
                    0b111_001 => Some(Self::Swc1),
                    0b111_010 => Some(Self::Swc2),
                    0b111_011 => Some(Self::Swc3),
                    0b111_100..=0b111_111 => None,

                    0b01_000_000.. => None,
                }
            }

            fn try_decode_special(code: u8) -> Option<Self> {
                match code {
                    0b000_000 => Some(Self::Sll),
                    0b000_001 => None,
                    0b000_010 => Some(Self::Srl),
                    0b000_011 => Some(Self::Sra),
                    0b000_100 => Some(Self::Sllv),
                    0b000_101 => None,
                    0b000_110 => Some(Self::Srlv),
                    0b000_111 => Some(Self::Srav),

                    0b001_000 => Some(Self::Jr),
                    0b001_001 => Some(Self::Jalr),
                    0b001_010..=0b001_011 => None,
                    0b001_100 => Some(Self::Syscall),
                    0b001_101 => Some(Self::Break),
                    0b001_110..=0b001_111 => None,

                    0b010_000 => Some(Self::Mfhi),
                    0b010_001 => Some(Self::Mthi),
                    0b010_010 => Some(Self::Mflo),
                    0b010_011 => Some(Self::Mtlo),
                    0b010_100..=0b010_111 => None,

                    0b011_000 => Some(Self::Mult),
                    0b011_001 => Some(Self::Multu),
                    0b011_010 => Some(Self::Div),
                    0b011_011 => Some(Self::Divu),
                    0b011_100..=0b011_111 => None,

                    0b100_000 => Some(Self::Add),
                    0b100_001 => Some(Self::Addu),
                    0b100_010 => Some(Self::Sub),
                    0b100_011 => Some(Self::Subu),
                    0b100_100 => Some(Self::And),
                    0b100_101 => Some(Self::Or),
                    0b100_110 => Some(Self::Xor),
                    0b100_111 => Some(Self::Nor),

                    0b101_000..=0b101_001 => None,
                    0b101_010 => Some(Self::Slt),
                    0b101_011 => Some(Self::Sltu),
                    0b101_100..=0b101_111 => None,

                    0b110_000.. => None,
                }
            }
        }

        impl opn::i::Operands {
            /// Decodes a machine instruction into its `Operands`.
            fn decode(mach: u32) -> Self {
                Self {
                    rs: decode_rs(mach),
                    rt: decode_rt(mach),
                    imm: decode_imm(mach),
                }
            }
        }

        impl opn::j::Operands {
            /// Decodes a machine instruction into its `Operands`.
            fn decode(mach: u32) -> Self {
                Self {
                    target: decode_target(mach),
                }
            }
        }

        impl opn::r::Operands {
            /// Decodes a machine instruction into its `Operands`.
            fn decode(mach: u32) -> Self {
                Self {
                    rs: decode_rs(mach),
                    rt: decode_rt(mach),
                    rd: decode_rd(mach),
                    shamt: decode_shamt(mach),
                    funct: decode_funct(mach),
                }
            }
        }

        impl Instr {
            /// Generates assembly information for this instruction.
            pub fn asm(&self) -> Asm {
                match *self {
                    $(
                        #[allow(unused_variables)]
                        Self::$variant_name(inner) => {
                            Asm {
                                op_name: $display_name.to_string(),
                                operands: vec![
                                    $(
                                        parse_operand!(inner, $kind($($arg)?))
                                    ),*
                                ],
                            }
                        }
                    )*
                }
            }
        }

        /// Generates an assembly operand.
        macro_rules! parse_operand {
            ($src:expr, %($field:tt)) => {
                asm::Operand::Reg($src.$field)
            };
            ($src:expr, *()) => {
                asm::Operand::SInt($src.target as i32)
            };
            ($src:expr, #(s)) => {
                asm::Operand::SInt($src.imm as i32)
            };
            ($src:expr, #(u)) => {
                asm::Operand::UInt($src.imm.into())
            };
            ($src:expr, ^()) => {
                asm::Operand::UInt($src.shamt.into())
            };
        }

        use parse_operand;

        pub mod opx {
            //! Operations.

            use std::fmt;

            use super::{Instr, JumpKind, Memory, PcBehavior, exc, opn, reg};

            /// An operation.
            ///
            /// Unlike [`Instr`], this type does not store any operand information.
            #[derive(Clone, Copy, Debug, Eq, PartialEq)]
            pub enum Operation {
                $(
                    #[doc = concat!("A `", stringify!($variant_name), "` operation.")]
                    $variant_name,
                )*
            }

            impl State {
                /// Creates a `State`.
                pub fn read(instr: Instr, reg: &reg::File) -> Self {
                    match instr {
                        $(
                            Instr::$variant_name(it) => {
                                Self::$variant_name(opn::$ty::State::read(reg, it))
                            }
                        )*
                    }
                }
            }

            /// The state of an operation.
            ///
            /// This type *does* store operand state.
            pub enum State {
                $(
                    #[doc = concat!("The state of a `", stringify!($variant_name), "` operation.")]
                    $variant_name(opn::$ty::State),
                )*
            }

            impl fmt::Display for State {
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                    match self {
                        $(
                            Self::$variant_name(_) => {
                                f.write_str(stringify!($variant_name))
                            }
                        )*
                    }
                }
            }

            impl State {
                /// Executes the operation represented by this state.
                pub fn process(
                    &mut self,
                    reg: &mut reg::File,
                    mem: &mut Memory,
                ) -> PcBehavior {
                    match self {
                        $(
                            #[allow(dead_code)]
                            State::$variant_name(ref mut opn) => {
                                $fn(Context { opn, reg, mem })
                            }
                        )*
                    }
                }
            }

            /// Necessary context for operation execution.
            struct Context<'a, 'c, 'b, S> {
                /// The state of the operands.
                opn: &'a mut S,
                /// The register file.
                reg: &'a mut reg::File,
                /// CPU memory.
                mem: &'a mut Memory<'c, 'b>,
            }

            impl<S> Context<'_, '_, '_, S> {
                /// Calculates the target of a branch.
                ///
                /// `value` is usually an immediate operand that may be shifted an sign-extended to
                /// form an offset from the program counter.
                fn calc_branch_target(&self, value: u16) -> u32 {
                    let base = self.reg.pc();
                    let offset = sign_extend_16(value << 2);

                    base.wrapping_add(offset)
                }

                /// Calculates the target of a jump.
                ///
                /// `value` is usually an immediate operand.
                #[inline(always)]
                fn calc_jump_target(&self, value: u32) -> u32 {
                    (self.reg.pc() & !((1 << 28) - 1)) | (value << 2)
                }

                /// Calculates the return address of a function to be executed.
                #[inline(always)]
                fn calc_ret_addr(&self) -> u32 {
                    self.reg.pc().wrapping_add(4)
                }

                /// Raises an exception with the given code.
                fn raise_exc(&mut self, code: u32) -> PcBehavior {
                    self.reg.raise_exception(code);

                    PcBehavior::Jumps {
                        kind: JumpKind::Exception,
                        // At this point, the program counter has already been updated to the
                        // correct exception vector.
                        target_addr: self.reg.pc(),
                    }
                }
            }

            /// Sign-extends a 16-bit value.
            #[inline(always)]
            fn sign_extend_16(value: u16) -> u32 {
                ((value as i16) as i32) as u32
            }

            /// Sign-extends an 8-bit value.
            #[inline(always)]
            fn sign_extend_8(value: u8) -> u32 {
                ((value as i8) as i32) as u32
            }

            /// Calculates a virtual, or 'program', address.
            fn calc_vaddr(base: u32, offset: u16) -> u32 {
                base.wrapping_add(sign_extend_16(offset))
            }
        }

        pub mod opn {
            //! Instruction operands.

            use super::reg;

            impl Register {
                /// Creates a `Register`.
                pub fn read(reg: &reg::File, index: usize) -> Self {
                    Self {
                        index: index,
                        gpr_value: reg.gpr(index),
                        cpr_value: reg.cpr(index),
                    }
                }
            }

            /// A register operand.
            #[derive(Clone, Copy, Debug, Eq, PartialEq)]
            pub struct Register {
                /// The zero-based index of this register.
                pub index: usize,
                /// The value of this register if it is interpreted as a general-purpose register
                /// (GPR).
                pub gpr_value: u32,
                /// The value of this register if it is interpreted as a control register.
                pub cpr_value: u32,
            }

            pub mod i {
                //! Immediate-type (I-type) operands.

                use super::{Register, reg};

                /// I-type operands, where 'I' stands for 'immediate'.
                #[derive(Clone, Copy, Debug, Eq, PartialEq)]
                pub struct Operands {
                    /// The *rs* operand.
                    pub rs: u8,
                    /// The *rt* operand.
                    pub rt: u8,
                    /// The *imm* operand.
                    pub imm: u16,
                }

                impl State {
                    /// Creates a new `State`.
                    pub fn read(reg: &reg::File, opers: Operands) -> Self {
                        Self {
                            rs: Register::read(reg, opers.rs.into()),
                            rt: Register::read(reg, opers.rt.into()),
                            imm: opers.imm,
                        }
                    }
                }

                /// The working state of I-type operands.
                #[derive(Debug)]
                pub struct State {
                    /// The register described by the *rs* operand.
                    pub rs: Register,
                    /// The register described by the *rt* operand.
                    pub rt: Register,
                    /// The *imm* operand.
                    pub imm: u16,
                }
            }

            pub mod j {
                //! Jump-type (J-type) operands.

                use super::reg;

                /// J-type operands, where 'J' stands for 'jump'.
                #[derive(Clone, Copy, Debug, Eq, PartialEq)]
                pub struct Operands {
                    /// The target program address of this jump.
                    pub target: u32,
                }

                impl State {
                    /// Creates a new `State`.
                    pub fn read(_: &reg::File, opers: Operands) -> Self {
                        // If we don't even use the [`reg::File`] that was passed to us, then what's the point
                        // of it being in the function signature? The reason is due to macro repetition
                        // generalizing the function signature of `_::State::read` methods; it's not really
                        // enough of an issue for me to care enough to fix it.
                        Self {
                            target: opers.target,
                        }
                    }
                }

                /// The working state of J-type operands.
                #[derive(Debug)]
                pub struct State {
                    /// The target program address of this jump.
                    pub target: u32,
                }
            }

            pub mod r {
                //! Register-type (R-type) operands.

                use super::{Register, reg};

                /// R-type operands, where 'R' stands for 'register'.
                #[derive(Clone, Copy, Debug, Eq, PartialEq)]
                pub struct Operands {
                    /// The *rs* operand.
                    pub rs: u8,
                    /// The *rt* operand.
                    pub rt: u8,
                    /// The *rd* operand.
                    ///
                    /// This is commonly used as a destination register.
                    pub rd: u8,
                    /// The *shamt* operand, which stands for 'shift amount'.
                    pub shamt: u8,
                    /// The *funct* operand.
                    pub funct: u8,
                }

                impl State {
                    /// Creates a new `State`.
                    pub fn read(reg: &reg::File, opers: Operands) -> Self {
                        Self {
                            rs: Register::read(reg, opers.rs.into()),
                            rt: Register::read(reg, opers.rt.into()),
                            rd: Register::read(reg, opers.rd.into()),
                            shamt: opers.shamt,
                            funct: opers.funct,
                        }
                    }
                }

                /// The working state of R-type operands.
                #[derive(Debug)]
                pub struct State {
                    /// The register described by the *rs* operand.
                    pub rs: Register,
                    /// The register described by the *rt* operand.
                    pub rt: Register,
                    /// The register described by the *rd* operand.
                    ///
                    /// This is commonly used as a destination register.
                    pub rd: Register,
                    /// The *shamt* operand, which stands for 'shift amount'.
                    pub shamt: u8,
                    /// The *funct* operand.
                    pub funct: u8,
                }
            }
        }
    };
}

// TODO: Write documentation for each instruction.
def_instr_and_op_kind!(
    {
        name: Add,
        type: r,
        asm: ["add" %(rd), %(rs), %(rt)],
        fn: |mut ctx: Context<opn::r::State>| {
            let (result, overflowed) = (ctx.opn.rs.gpr_value as i32)
                .overflowing_add(ctx.opn.rt.gpr_value as i32);

            if overflowed {
                ctx.raise_exc(exc::code::INTEGER_OVERFLOW)
            } else {
                ctx.reg.set_gpr(ctx.opn.rd.index, result as u32);

                PcBehavior::Increments
            }
        },
    },
    {
        name: Addi,
        type: i,
        asm: ["addi" %(rt), %(rs), #(s)],
        fn: |mut ctx: Context<opn::i::State>| {
            let (result, overflowed) = (ctx.opn.rs.gpr_value as i32)
                .overflowing_add(sign_extend_16(ctx.opn.imm) as i32);

            if overflowed {
                ctx.raise_exc(exc::code::INTEGER_OVERFLOW)
            } else {
                ctx.reg.set_gpr(ctx.opn.rt.index, result as u32);

                PcBehavior::Increments
            }
        },
    },
    {
        name: Addiu,
        type: i,
        asm: ["addiu" %(rt), %(rs), #(s)],
        fn: |ctx: Context<opn::i::State>| {
            // This operation is unsigned, so no need to test for overflow.
            ctx.reg.set_gpr(ctx.opn.rt.index, ctx.opn.rs.gpr_value.wrapping_add(sign_extend_16(ctx.opn.imm)));

            PcBehavior::Increments
        },
    },
    {
        name: Addu,
        type: r,
        asm: ["addu" %(rd), %(rs), %(rt)],
        fn: |ctx: Context<opn::r::State>| {
            // This operation is unsigned, so no need to test for overflow.
            ctx.reg.set_gpr(ctx.opn.rd.index, ctx.opn.rs.gpr_value.wrapping_add(ctx.opn.rt.gpr_value));

            PcBehavior::Increments
        },
    },
    {
        name: And,
        type: r,
        asm: ["and" %(rd), %(rs), %(rt)],
        fn: |ctx: Context<opn::r::State>| {
            ctx.reg.set_gpr(ctx.opn.rd.index, ctx.opn.rs.gpr_value & ctx.opn.rt.gpr_value);

            PcBehavior::Increments
        },
    },
    {
        name: Andi,
        type: i,
        asm: ["andi" %(rt), %(rs), #(u)],
        fn: |ctx: Context<opn::i::State>| {
            ctx.reg.set_gpr(ctx.opn.rt.index, ctx.opn.rs.gpr_value & u32::from(ctx.opn.imm));

            PcBehavior::Increments
        },
    },
    {
        name: BCond,
        type: i,
        asm: ["b.cond" %(rs), #(s)],
        fn: |mut ctx: Context<opn::i::State>| {
            match ctx.opn.rt.index {
                0 => {
                    tracing::trace!("bltz");

                    if (ctx.opn.rs.gpr_value as i32) < 0 {
                        PcBehavior::jumps_without_return(ctx.calc_branch_target(ctx.opn.imm))
                    } else {
                        PcBehavior::Increments
                    }
                }
                1 => {
                    tracing::trace!("bgez");

                    if (ctx.opn.rs.gpr_value as i32) >= 0 {
                        PcBehavior::jumps_without_return(ctx.calc_branch_target(ctx.opn.imm))
                    } else {
                        PcBehavior::Increments
                    }
                }
                16 => {
                    tracing::trace!("bltzal");
                    todo!();
                }
                17 => {
                    tracing::trace!("bgezal");
                    todo!();
                }
                _ => ctx.raise_exc(exc::code::RESERVED_INSTR),
            }
        },
    },
    {
        name: Beq,
        type: i,
        asm: ["beq" %(rs), %(rt), #(s)],
        fn: |ctx: Context<opn::i::State>| {
            if ctx.opn.rs.gpr_value == ctx.opn.rt.gpr_value {
                PcBehavior::jumps_without_return(ctx.calc_branch_target(ctx.opn.imm))
            } else {
                PcBehavior::Increments
            }
        },
    },
    {
        name: Bgtz,
        type: i,
        asm: ["bgtz" %(rs), #(s)],
        fn: |ctx: Context<opn::i::State>| {
            if (ctx.opn.rs.gpr_value as i32) > 0 {
                PcBehavior::jumps_without_return(ctx.calc_branch_target(ctx.opn.imm))
            } else {
                PcBehavior::Increments
            }
        },
    },
    {
        name: Blez,
        type: i,
        asm: ["blez" %(rs), #(s)],
        fn: |ctx: Context<opn::i::State>| {
            if (ctx.opn.rs.gpr_value as i32) <= 0 {
                PcBehavior::jumps_without_return(ctx.calc_branch_target(ctx.opn.imm))
            } else {
                PcBehavior::Increments
            }
        },
    },
    {
        name: Bne,
        type: i,
        asm: ["bne" %(rs), %(rt), #(s)],
        fn: |ctx: Context<opn::i::State>| {
            if ctx.opn.rs.gpr_value != ctx.opn.rt.gpr_value {
                PcBehavior::jumps_without_return(ctx.calc_branch_target(ctx.opn.imm))
            } else {
                PcBehavior::Increments
            }
        },
    },
    {
        name: Break,
        type: i,
        asm: ["break"],
        fn: |mut ctx: Context<opn::i::State>| {
            ctx.raise_exc(exc::code::BREAKPOINT)
        },
    },
    {
        name: Cop0,
        type: i,
        asm: ["cop0"],
        fn: |mut ctx: Context<opn::i::State>| {
            match ctx.opn.imm & 0b111111 {
                0 => match ctx.opn.rs.index {
                    0 => {
                        tracing::trace!("mfc0");
                        ctx.reg.set_gpr(ctx.opn.rt.index, ctx.reg.cpr((ctx.opn.imm >> 11).into()));

                        PcBehavior::Increments
                    }
                    2 => {
                        tracing::trace!("cfc0");

                        // COP0 doesn't support this instruction.
                        ctx.raise_exc(exc::code::RESERVED_INSTR)
                    }
                    4 => {
                        tracing::trace!("mtc0");
                        ctx.reg.set_cpr(usize::from(ctx.opn.imm >> 11), ctx.opn.rt.gpr_value);

                        PcBehavior::Increments
                    }
                    6 => {
                        tracing::trace!("ctc0");

                        // COP0 doesn't support this instruction.
                        ctx.raise_exc(exc::code::RESERVED_INSTR)
                    }
                    _ => {
                        ctx.raise_exc(exc::code::RESERVED_INSTR)
                    }
                }
                // The PSX CPU doesn't implement an MMU... so page table instructions are reserved.
                1 => {
                    tracing::trace!("tlbr");

                    ctx.raise_exc(exc::code::RESERVED_INSTR)
                }
                2 => {
                    tracing::trace!("tlbwi");

                    ctx.raise_exc(exc::code::RESERVED_INSTR)
                }
                8 => {
                    tracing::trace!("tlbp");

                    ctx.raise_exc(exc::code::RESERVED_INSTR)
                }
                16 => {
                    tracing::trace!("rfe");

                    let mut sr = reg::cpr::Status(ctx.reg.cpr(reg::cpr::STATUS_IDX));
                    sr.set_ie_c(sr.ie_p());
                    sr.set_ku_c(sr.ku_p());
                    sr.set_ie_p(sr.ie_o());
                    sr.set_ku_p(sr.ku_o());
                    ctx.reg.set_cpr(reg::cpr::STATUS_IDX, sr.0);

                    PcBehavior::Increments
                }
                _ => {
                    ctx.raise_exc(exc::code::RESERVED_INSTR)
                }
            }
        },
    },
    {
        name: Cop1,
        type: i,
        asm: ["cop1"],
        fn: |mut ctx: Context<opn::i::State>| {
            // The PSX CPU lacks a coprocessor #1.
            ctx.raise_exc(exc::code::COP_UNUSABLE)
        },
    },
    {
        name: Cop2,
        type: i,
        asm: ["cop2"],
        fn: |_| {
            // GTE.
            todo!()
        },
    },
    {
        name: Cop3,
        type: i,
        asm: ["cop3"],
        fn: |mut ctx: Context<opn::i::State>| {
            // The PSX CPU lacks a coprocessor #3.
            ctx.raise_exc(exc::code::COP_UNUSABLE)
        },
    },
    {
        name: Div,
        type: r,
        asm: ["div"  %(rs), %(rt)],
        fn: |ctx: Context<opn::r::State>| {
            // Note: MIPS I specifies that division by 0 is undefined, but to be safe, we'll
            // hardcode it to the fairly-reasonable value of 0.
            *ctx.reg.lo_mut() = ctx.opn.rs.gpr_value.checked_div(ctx.opn.rt.gpr_value).unwrap_or(0);
            *ctx.reg.hi_mut() = ctx.opn.rs.gpr_value % ctx.opn.rt.gpr_value;

            PcBehavior::Increments
        },
    },
    {
        name: Divu,
        type: r,
        asm: ["divu"  %(rs), %(rt)],
        fn: |ctx: Context<opn::r::State>| {
            // See `Div`.
            *ctx.reg.lo_mut() = ctx.opn.rs.gpr_value.checked_div(ctx.opn.rt.gpr_value).unwrap_or(0);
            *ctx.reg.hi_mut() = ctx.opn.rs.gpr_value % ctx.opn.rt.gpr_value;

            PcBehavior::Increments
        },
    },
    {
        name: J,
        type: j,
        asm: ["j" *()],
        fn: |ctx: Context<opn::j::State>| {
            PcBehavior::jumps_without_return(ctx.calc_jump_target(ctx.opn.target))
        },
    },
    {
        name: Jal,
        type: j,
        asm: ["jal" *()],
        fn: |ctx: Context<opn::j::State>| {
            let target_addr = ctx.calc_jump_target(ctx.opn.target);
            ctx.reg.set_gpr(31, ctx.calc_ret_addr());

            PcBehavior::calls(target_addr)
        },
    },
    {
        name: Jalr,
        type: r,
        asm: ["jalr" %(rd), %(rs)],
        fn: |ctx: Context<opn::r::State>| {
            let target_addr = ctx.opn.rs.gpr_value;
            ctx.reg.set_gpr(31, ctx.calc_ret_addr());

            PcBehavior::calls(target_addr)
        },
    },
    {
        name: Jr,
        type: r,
        asm: ["jr" %(rs)],
        fn: |ctx: Context<opn::r::State>| {
            let target_addr = ctx.opn.rs.gpr_value;

            if ctx.opn.rs.index == 31 {
                PcBehavior::returns(target_addr)
            } else {
                PcBehavior::jumps_without_return(target_addr)
            }
        },
    },
    {
        name: Lb,
        type: i,
        asm: ["lb" %(rt), #(s), %(rs)],
        fn: |mut ctx: Context<opn::i::State>| {
            if let Ok(value) = ctx.mem.read_data_8(calc_vaddr(ctx.opn.rs.gpr_value, ctx.opn.imm)) {
                ctx.reg.set_gpr(ctx.opn.rt.index, sign_extend_8(value));

                PcBehavior::Increments
            } else {
                ctx.raise_exc(exc::code::ADDRESS_LOAD)
            }
        },
    },
    {
        name: Lbu,
        type: i,
        asm: ["lbu" %(rt), #(s), %(rs)],
        fn: |mut ctx: Context<opn::i::State>| {
            if let Ok(value) = ctx.mem.read_data_8(calc_vaddr(ctx.opn.rs.gpr_value, ctx.opn.imm)) {
                ctx.reg.set_gpr(ctx.opn.rt.index, value.into());

                PcBehavior::Increments
            } else {
                ctx.raise_exc(exc::code::ADDRESS_LOAD)
            }
        },
    },
    {
        name: Lh,
        type: i,
        asm: ["lh" %(rt), #(s), %(rs)],
        fn: |mut ctx: Context<opn::i::State>| {
            if let Ok(value) = ctx.mem.read_data_16(calc_vaddr(ctx.opn.rs.gpr_value, ctx.opn.imm)) {
                ctx.reg.set_gpr(ctx.opn.rt.index, sign_extend_16(value));

                PcBehavior::Increments
            } else {
                ctx.raise_exc(exc::code::ADDRESS_LOAD)
            }
        },
    },
    {
        name: Lhu,
        type: i,
        asm: ["lhu" %(rt), #(s), %(rs)],
        fn: |mut ctx: Context<opn::i::State>| {
            if let Ok(value) = ctx.mem.read_data_16(calc_vaddr(ctx.opn.rs.gpr_value, ctx.opn.imm)) {
                ctx.reg.set_gpr(ctx.opn.rt.index, value.into());

                PcBehavior::Increments
            } else {
                ctx.raise_exc(exc::code::ADDRESS_LOAD)
            }
        },
    },
    {
        name: Lui,
        type: i,
        asm: ["lui" %(rt), #(s)],
        fn: |ctx: Context<opn::i::State>| {
            ctx.reg.set_gpr(ctx.opn.rt.index, u32::from(ctx.opn.imm) << 16);

            PcBehavior::Increments
        },
    },
    {
        name: Lw,
        type: i,
        asm: ["lw" %(rt), #(s), %(rs)],
        fn: |mut ctx: Context<opn::i::State>| {
            if let Ok(value) = ctx.mem.read_data_32(calc_vaddr(ctx.opn.rs.gpr_value, ctx.opn.imm)) {
                ctx.reg.set_gpr(ctx.opn.rt.index, value);

                PcBehavior::Increments
            } else {
                ctx.raise_exc(exc::code::ADDRESS_LOAD)
            }
        },
    },
    {
        name: Lwc0,
        type: i,
        asm: ["lwc0" %(rt), #(s)],
        fn: |_: Context<opn::i::State>| todo!(),
    },
    {
        name: Lwc1,
        type: i,
        asm: ["lwc1" %(rt), #(s)],
        fn: |mut ctx: Context<opn::i::State>| {
            // The PSX CPU lacks a coprocessor #1.
            ctx.raise_exc(exc::code::COP_UNUSABLE)
        },
    },
    {
        name: Lwc2,
        type: i,
        asm: ["lwc2" %(rt), #(s)],
        fn: |_| {
            // GTE.
            todo!()
        },
    },
    {
        name: Lwc3,
        type: i,
        asm: ["lwc3" %(rt), #(s)],
        fn: |mut ctx: Context<opn::i::State>| {
            // The PSX CPU lacks a coprocessor #3.
            ctx.raise_exc(exc::code::COP_UNUSABLE)
        },
    },
    {
        name: Lwl,
        type: i,
        asm: ["lwl" %(rt), #(s)],
        fn: |_| todo!(),
    },
    {
        name: Lwr,
        type: i,
        asm: ["lwr" %(rt), #(s)],
        fn: |_| todo!(),
    },
    {
        name: Mfhi,
        type: r,
        asm: ["mfhi" %(rd)],
        fn: |ctx: Context<opn::r::State>| {
            ctx.reg.set_gpr(ctx.opn.rd.index, ctx.reg.hi());

            PcBehavior::Increments
        },
    },
    {
        name: Mflo,
        type: r,
        asm: ["mflo" %(rd)],
        fn: |ctx: Context<opn::r::State>| {
            ctx.reg.set_gpr(ctx.opn.rd.index, ctx.reg.lo());

            PcBehavior::Increments
        },
    },
    {
        name: Mthi,
        type: r,
        asm: ["mthi" %(rd)],
        fn: |ctx: Context<opn::r::State>| {
            *ctx.reg.hi_mut() = ctx.opn.rs.gpr_value;

            PcBehavior::Increments
        },
    },
    {
        name: Mtlo,
        type: r,
        asm: ["mtlo" %(rd)],
        fn: |ctx: Context<opn::r::State>| {
            *ctx.reg.lo_mut() = ctx.opn.rs.gpr_value;

            PcBehavior::Increments
        },
    },
    {
        name: Mult,
        type: r,
        asm: ["mult" %(rs), %(rt)],
        fn: |ctx: Context<opn::r::State>| {
            // SAFETY: Overflow is contained within `value`; this is currently how
            // [`u32::widening_mul`] is implemented.
            let value: u64 = unsafe {
                (ctx.opn.rs.gpr_value as i64).unchecked_mul(ctx.opn.rt.gpr_value as i64) as u64
            };
            *ctx.reg.lo_mut() = value as u32;
            *ctx.reg.hi_mut() = (value >> 32) as u32;

            PcBehavior::Increments
        },
    },
    {
        name: Multu,
        type: r,
        asm: ["multu"  %(rs), %(rt)],
        fn: |ctx: Context<opn::r::State>| {
            let (lo, hi) = ctx.opn.rs.gpr_value.widening_mul(ctx.opn.rt.gpr_value);
            *ctx.reg.lo_mut() = lo;
            *ctx.reg.hi_mut() = hi;

            PcBehavior::Increments
        },
    },
    {
        name: Nor,
        type: r,
        asm: ["nor" %(rd), %(rs), %(rt)],
        fn: |ctx: Context<opn::r::State>| {
            ctx.reg.set_gpr(ctx.opn.rd.index, !(ctx.opn.rs.gpr_value | ctx.opn.rt.gpr_value));

            PcBehavior::Increments
        },
    },
    {
        name: Or,
        type: r,
        asm: ["or" %(rd), %(rs), %(rt)],
        fn: |ctx: Context<opn::r::State>| {
            ctx.reg.set_gpr(ctx.opn.rd.index, ctx.opn.rs.gpr_value | ctx.opn.rt.gpr_value);

            PcBehavior::Increments
        },
    },
    {
        name: Ori,
        type: i,
        asm: ["ori" %(rt), %(rs), #(u)],
        fn: |ctx: Context<opn::i::State>| {
            ctx.reg.set_gpr(ctx.opn.rt.index, ctx.opn.rs.gpr_value | u32::from(ctx.opn.imm));

            PcBehavior::Increments
        },
    },
    {
        name: Sb,
        type: i,
        asm: ["sb" %(rt), #(s), %(rs)],
        fn: |mut ctx: Context<opn::i::State>| {
            let vaddr = calc_vaddr(ctx.opn.rs.gpr_value, ctx.opn.imm);

            if ctx.mem.write_data_8(vaddr, ctx.opn.rt.gpr_value as u8).is_err() {
                ctx.raise_exc(exc::code::ADDRESS_STORE)
            } else {
                PcBehavior::Increments
            }
        },
    },
    {
        name: Sh,
        type: i,
        asm: ["sh" %(rt), #(s), %(rs)],
        fn: |mut ctx: Context<opn::i::State>| {
            let vaddr = calc_vaddr(ctx.opn.rs.gpr_value, ctx.opn.imm);

            if ctx.mem.write_data_16(vaddr, ctx.opn.rt.gpr_value as u16).is_err() {
                ctx.raise_exc(exc::code::ADDRESS_STORE)
            } else {
                PcBehavior::Increments
            }
        },
    },
    {
        name: Sll,
        type: r,
        asm: ["sll" %(rd), %(rt), ^()],
        fn: |ctx: Context<opn::r::State>| {
            ctx.reg.set_gpr(ctx.opn.rd.index, ctx.opn.rt.gpr_value << ctx.opn.shamt);

            PcBehavior::Increments
        },
    },
    {
        name: Sllv,
        type: r,
        asm: ["sllv" %(rd), %(rt), %(rs)],
        fn: |ctx: Context<opn::r::State>| {
            ctx.reg.set_gpr(ctx.opn.rd.index, ctx.opn.rt.gpr_value << (ctx.opn.rs.gpr_value & 0b11111));

            PcBehavior::Increments
        },
    },
    {
        name: Slt,
        type: r,
        asm: ["slt" %(rd), %(rs), %(rt)],
        fn: |ctx: Context<opn::r::State>| {
            ctx.reg.set_gpr(ctx.opn.rd.index, ((ctx.opn.rs.gpr_value as i32) < (ctx.opn.rt.gpr_value as i32)) as u32);

            PcBehavior::Increments
        },
    },
    {
        name: Slti,
        type: i,
        asm: ["slti" %(rt), %(rs), #(s)],
        fn: |ctx: Context<opn::i::State>| {
            ctx.reg.set_gpr(ctx.opn.rt.index, ((ctx.opn.rs.gpr_value as i32) < (sign_extend_16(ctx.opn.imm) as i32)) as u32);

            PcBehavior::Increments
        },
    },
    {
        name: Sltiu,
        type: i,
        asm: ["sltiu" %(rt), %(rs), #(s)],
        fn: |ctx: Context<opn::i::State>| {
            ctx.reg.set_gpr(ctx.opn.rt.index, (ctx.opn.rs.gpr_value < sign_extend_16(ctx.opn.imm)) as u32);

            PcBehavior::Increments
        },
    },
    {
        name: Sltu,
        type: r,
        asm: ["sltu" %(rd), %(rs), %(rt)],
        fn: |ctx: Context<opn::r::State>| {
            ctx.reg.set_gpr(ctx.opn.rd.index, (ctx.opn.rs.gpr_value < ctx.opn.rt.gpr_value) as u32);

            PcBehavior::Increments
        },
    },
    {
        name: Sra,
        type: r,
        asm: ["sra" %(rd), %(rt), ^()],
        fn: |ctx: Context<opn::r::State>| {
            ctx.reg.set_gpr(ctx.opn.rd.index, ((ctx.opn.rt.gpr_value as i32) >> ctx.opn.shamt) as u32);

            PcBehavior::Increments
        },
    },
    {
        name: Srav,
        type: r,
        asm: ["srav"  %(rd), %(rt), %(rs)],
        fn: |ctx: Context<opn::r::State>| {
            ctx.reg.set_gpr(ctx.opn.rd.index, ((ctx.opn.rt.gpr_value as i32) >> (ctx.opn.rs.gpr_value & 0b11111)) as u32);

            PcBehavior::Increments
        },
    },
    {
        name: Srl,
        type: r,
        asm: ["srl" %(rd), %(rt), ^()],
        fn: |ctx: Context<opn::r::State>| {
            ctx.reg.set_gpr(ctx.opn.rd.index, ctx.opn.rt.gpr_value >> ctx.opn.shamt);

            PcBehavior::Increments
        },
    },
    {
        name: Srlv,
        type: r,
        asm: ["srlv"  %(rd), %(rt), %(rs)],
        fn: |ctx: Context<opn::r::State>| {
            ctx.reg.set_gpr(ctx.opn.rd.index, ctx.opn.rt.gpr_value >> (ctx.opn.rs.gpr_value & 0b11111));

            PcBehavior::Increments
        },
    },
    {
        name: Sub,
        type: r,
        asm: ["sub" %(rd), %(rs), %(rt)],
        fn: |mut ctx: Context<opn::r::State>| {
            let (result, overflowed) = (ctx.opn.rs.gpr_value as i32)
                .overflowing_sub(ctx.opn.rt.gpr_value as i32);

            if overflowed {
                ctx.raise_exc(exc::code::INTEGER_OVERFLOW)
            } else {
                ctx.reg.set_gpr(ctx.opn.rd.index, result as u32);

                PcBehavior::Increments
            }
        },
    },
    {
        name: Subu,
        type: r,
        asm: ["subu" %(rd), %(rs), %(rt)],
        fn: |ctx: Context<opn::r::State>| {
            ctx.reg.set_gpr(ctx.opn.rd.index, ctx.opn.rs.gpr_value.wrapping_sub(ctx.opn.rt.gpr_value));

            PcBehavior::Increments
        },
    },
    {
        name: Sw,
        type: i,
        asm: ["sw" %(rt), #(s), %(rs)],
        fn: |mut ctx: Context<opn::i::State>| {
            let vaddr = calc_vaddr(ctx.opn.rs.gpr_value, ctx.opn.imm);

            if ctx.mem.write_data_32(vaddr, ctx.opn.rt.gpr_value).is_err() {
                ctx.raise_exc(exc::code::ADDRESS_STORE)
            } else {
                PcBehavior::Increments
            }
        },
    },
    {
        name: Swc0,
        type: i,
        asm: ["swc0" %(rt), #(s)],
        fn: |_| todo!(),
    },
    {
        name: Swc1,
        type: i,
        asm: ["swc1" %(rt), #(s)],
        fn: |mut ctx: Context<opn::i::State>| {
            // The PSX CPU lacks a coprocessor #1.
            ctx.raise_exc(exc::code::COP_UNUSABLE)
        },
    },
    {
        name: Swc2,
        type: i,
        asm: ["swc2" %(rt), #(s)],
        fn: |_| {
            // GTE.
            todo!()
        },
    },
    {
        name: Swc3,
        type: i,
        asm: ["swc3" %(rt), #(s)],
        fn: |mut ctx: Context<opn::i::State>| {
            // The PSX CPU lacks a coprocessor #3.
            ctx.raise_exc(exc::code::COP_UNUSABLE)
        },
    },
    {
        name: Swl,
        type: i,
        asm: ["swl" %(rt), #(s)],
        fn: |_| todo!(),
    },
    {
        name: Swr,
        type: i,
        asm: ["swr" %(rt), #(s)],
        fn: |_| todo!(),
    },
    {
        name: Syscall,
        type: i,
        asm: ["syscall"],
        fn: |mut ctx: Context<opn::i::State>| {
            ctx.raise_exc(exc::code::SYSCALL)
        },
    },
    {
        name: Xor,
        type: r,
        asm: ["xor" %(rd), %(rs), %(rt)],
        fn: |ctx: Context<opn::r::State>| {
            ctx.reg.set_gpr(ctx.opn.rd.index, ctx.opn.rs.gpr_value ^ ctx.opn.rt.gpr_value);

            PcBehavior::Increments
        },
    },
    {
        name: Xori,
        type: i,
        asm: ["xori" %(rt), %(rs), #(u)],
        fn: |ctx: Context<opn::i::State>| {
            ctx.reg.set_gpr(ctx.opn.rt.index, ctx.opn.rs.gpr_value ^ u32::from(ctx.opn.imm));

            PcBehavior::Increments
        },
    },
);
