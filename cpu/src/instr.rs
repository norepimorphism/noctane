// SPDX-License-Identifier: MPL-2.0

//! CPU instructions.
//!
//! The PSX CPU is compatible with the 32-bit MIPS ISA. The exact composition of an instruction is
//! explained in-depth in the [`decode`] module, but briefly, each instruction is tagged with an
//! identifying opcode, and the remaining bits are used to store parameters.
//!
//! When the [intruction pipeline](Pipeline) is advanced, the following occurs:
//! - A new instruction is fetched at the program address represented by the program counter (PC)
//!   register.
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
pub mod decode;

pub use asm::Asm;

use crate::{exc, reg, Memory};

pub mod i {
    //! Immediate-type (I-type) instructions.

    use super::{opn::Register, reg};

    /// An I-type instruction, where 'I' stands for 'immediate'.
    #[derive(Clone, Copy, Debug, Eq, PartialEq)]
    pub struct Instr {
        /// The *rs* operand.
        pub rs: u8,
        /// The *rt* operand.
        pub rt: u8,
        /// The *imm* operand.
        pub imm: u16,
    }

    impl State {
        /// Creates a new `State`.
        pub fn read(reg: &reg::File, instr: Instr) -> Self {
            Self {
                rs: Register::read(reg, instr.rs.into()),
                rt: Register::read(reg, instr.rt.into()),
                imm: instr.imm,
            }
        }
    }

    /// The working state of an I-type instruction.
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
    //! Jump-type (J-type) instructions.

    use super::reg;

    /// A J-type instruction, where 'J' stands for 'jump'.
    #[derive(Clone, Copy, Debug, Eq, PartialEq)]
    pub struct Instr {
        /// The target program address of this jump.
        pub target: u32,
    }

    impl State {
        /// Creates a new `State`.
        pub fn read(_: &reg::File, instr: Instr) -> Self {
            // If we don't even use the [`reg::File`] that was passed to us, then what's the point
            // of it being in the function signature? The reason is due to macro repetition
            // generalizing the function signature of `_::State::read` methods; it's not really
            // enough of an issue for me to care enough to fix it.
            Self {
                target: instr.target,
            }
        }
    }

    /// The working state of a J-type instruction.
    #[derive(Debug)]
    pub struct State {
        /// The target program address of this jump.
        pub target: u32,
    }
}

pub mod r {
    //! Register-type (R-type) instructions.

    use super::{opn::Register, reg};

    /// An R-type instruction, where 'R' stands for 'register'.
    #[derive(Clone, Copy, Debug, Eq, PartialEq)]
    pub struct Instr {
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
        pub fn read(reg: &reg::File, instr: Instr) -> Self {
            Self {
                rs: Register::read(reg, instr.rs.into()),
                rt: Register::read(reg, instr.rt.into()),
                rd: Register::read(reg, instr.rd.into()),
                shamt: instr.shamt,
                funct: instr.funct,
            }
        }
    }

    /// The working state of an R-type instruction.
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
    /// Creates a new [`PcBehavior`] that jumps without return to the given target address.
    fn jumps_without_return(target_addr: u32) -> Self {
        Self::Jumps { kind: JumpKind::WithoutReturn, target_addr }
    }

    /// Creates a new [`PcBehavior`] that jumps to the given target address with the intention of
    /// returning.
    fn calls(target_addr: u32) -> Self {
        Self::Jumps { kind: JumpKind::Call, target_addr }
    }

    /// Creates a new [`PcBehavior`] that returns from a function.
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

        // We begin by processing interrupts if the SR permits us to do so. This may trigger an
        // interrupt; if it does, it will update the PC, which will be used throughout the rest of
        // this function.
        if reg.status().ie_c() {
            self.process_interrupts(reg);
        }

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

    /// Processes through all interrupts, generating exceptions as necessary.
    fn process_interrupts(&mut self, reg: &mut reg::File) {
        for idx in 0..8 {
            self.process_interrupt(reg, idx);
        }
    }

    /// Processes the interrupt at the given index, generating an exception if necessary.
    fn process_interrupt(&mut self, reg: &mut reg::File, index: usize) {
        let mut cause = reg.cause();
        if cause.is_interrupt_set(index) {
            // Raise an interrupt exception.
            reg.raise_exception(exc::code::INTERRUPT);
            // Indicate that this interrupt has been processed.
            cause.clear_interrupt(index);
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
            if let Ok(op) = mem.read_32(pc) {
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
        let pc_behavior = state.execute(reg, mem);
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
                $variant_name($ty::Instr),
            )*
        }

        impl Instr {
            /// Decodes a CPU instrucion from its encoded form.
            ///
            /// This function may fail and return `None` if the code does not correspond to a valid
            /// instruction.
            pub fn decode(code: u32) -> Option<Self> {
                match Self::try_decode_op_kind(code)? {
                    $(
                        opx::Kind::$variant_name => {
                            Some(Self::$variant_name($ty::Instr::decode(code)))
                        }
                    )*
                }
            }

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

            use super::{Instr, JumpKind, Memory, PcBehavior, exc::self, i, j, r, reg};

            /// The kind of an operation.
            ///
            /// Unlike [`Instr`], this type does not store any operand information.
            #[derive(Clone, Copy, Debug, Eq, PartialEq)]
            pub enum Kind {
                $(
                    #[doc = concat!("A `", stringify!($variant_name), "` operation.")]
                    $variant_name,
                )*
            }

            impl State {
                /// Creates a [`State`].
                pub fn read(instr: Instr, reg: &reg::File) -> Self {
                    match instr {
                        $(
                            Instr::$variant_name(it) => {
                                Self::$variant_name(super::$ty::State::read(reg, it))
                            }
                        )*
                    }
                }
            }

            /// The state of an operation.
            pub enum State {
                $(
                    #[doc = concat!("The state of a `", stringify!($variant_name), "` operation.")]
                    $variant_name(super::$ty::State),
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
                pub fn execute(
                    &mut self,
                    reg: &mut reg::File,
                    mem: &mut Memory,
                ) -> PcBehavior {
                    match self {
                        $(
                            #[allow(dead_code)]
                            State::$variant_name(ref mut opx) => {
                                $fn(Context { opx, reg, mem })
                            }
                        )*
                    }
                }
            }

            /// Necessary context for operation execution.
            struct Context<'a, 'c, 'b, S> {
                /// The state of the operation.
                opx: &'a mut S,
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
                /// Creates a [`Register`].
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
        }
    };
}

// TODO: Write documentation for each instruction.
def_instr_and_op_kind!(
    {
        name: Add,
        type: r,
        asm: ["add" %(rd), %(rs), %(rt)],
        fn: |mut ctx: Context<r::State>| {
            let (result, overflowed) = (ctx.opx.rs.gpr_value as i32)
                .overflowing_add(ctx.opx.rt.gpr_value as i32);

            if overflowed {
                ctx.raise_exc(exc::code::INTEGER_OVERFLOW)
            } else {
                ctx.reg.set_gpr(ctx.opx.rd.index, result as u32);

                PcBehavior::Increments
            }
        },
    },
    {
        name: Addi,
        type: i,
        asm: ["addi" %(rt), %(rs), #(s)],
        fn: |mut ctx: Context<i::State>| {
            let (result, overflowed) = (ctx.opx.rs.gpr_value as i32)
                .overflowing_add(sign_extend_16(ctx.opx.imm) as i32);

            if overflowed {
                ctx.raise_exc(exc::code::INTEGER_OVERFLOW)
            } else {
                ctx.reg.set_gpr(ctx.opx.rt.index, result as u32);

                PcBehavior::Increments
            }
        },
    },
    {
        name: Addiu,
        type: i,
        asm: ["addiu" %(rt), %(rs), #(s)],
        fn: |ctx: Context<i::State>| {
            // This operation is unsigned, so no need to test for overflow.
            ctx.reg.set_gpr(ctx.opx.rt.index, ctx.opx.rs.gpr_value.wrapping_add(sign_extend_16(ctx.opx.imm)));

            PcBehavior::Increments
        },
    },
    {
        name: Addu,
        type: r,
        asm: ["addu" %(rd), %(rs), %(rt)],
        fn: |ctx: Context<r::State>| {
            // This operation is unsigned, so no need to test for overflow.
            ctx.reg.set_gpr(ctx.opx.rd.index, ctx.opx.rs.gpr_value.wrapping_add(ctx.opx.rt.gpr_value));

            PcBehavior::Increments
        },
    },
    {
        name: And,
        type: r,
        asm: ["and" %(rd), %(rs), %(rt)],
        fn: |ctx: Context<r::State>| {
            ctx.reg.set_gpr(ctx.opx.rd.index, ctx.opx.rs.gpr_value & ctx.opx.rt.gpr_value);

            PcBehavior::Increments
        },
    },
    {
        name: Andi,
        type: i,
        asm: ["andi" %(rt), %(rs), #(u)],
        fn: |ctx: Context<i::State>| {
            ctx.reg.set_gpr(ctx.opx.rt.index, ctx.opx.rs.gpr_value & u32::from(ctx.opx.imm));

            PcBehavior::Increments
        },
    },
    {
        name: BCond,
        type: i,
        asm: ["b.cond" %(rs), #(s)],
        fn: |mut ctx: Context<i::State>| {
            match ctx.opx.rt.index {
                0 => {
                    tracing::trace!("bltz");

                    if (ctx.opx.rs.gpr_value as i32) < 0 {
                        PcBehavior::jumps_without_return(ctx.calc_branch_target(ctx.opx.imm))
                    } else {
                        PcBehavior::Increments
                    }
                }
                1 => {
                    tracing::trace!("bgez");

                    if (ctx.opx.rs.gpr_value as i32) >= 0 {
                        PcBehavior::jumps_without_return(ctx.calc_branch_target(ctx.opx.imm))
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
        fn: |ctx: Context<i::State>| {
            if ctx.opx.rs.gpr_value == ctx.opx.rt.gpr_value {
                PcBehavior::jumps_without_return(ctx.calc_branch_target(ctx.opx.imm))
            } else {
                PcBehavior::Increments
            }
        },
    },
    {
        name: Bgtz,
        type: i,
        asm: ["bgtz" %(rs), #(s)],
        fn: |ctx: Context<i::State>| {
            if (ctx.opx.rs.gpr_value as i32) > 0 {
                PcBehavior::jumps_without_return(ctx.calc_branch_target(ctx.opx.imm))
            } else {
                PcBehavior::Increments
            }
        },
    },
    {
        name: Blez,
        type: i,
        asm: ["blez" %(rs), #(s)],
        fn: |ctx: Context<i::State>| {
            if (ctx.opx.rs.gpr_value as i32) <= 0 {
                PcBehavior::jumps_without_return(ctx.calc_branch_target(ctx.opx.imm))
            } else {
                PcBehavior::Increments
            }
        },
    },
    {
        name: Bne,
        type: i,
        asm: ["bne" %(rs), %(rt), #(s)],
        fn: |ctx: Context<i::State>| {
            if ctx.opx.rs.gpr_value != ctx.opx.rt.gpr_value {
                PcBehavior::jumps_without_return(ctx.calc_branch_target(ctx.opx.imm))
            } else {
                PcBehavior::Increments
            }
        },
    },
    {
        name: Break,
        type: i,
        asm: ["break"],
        fn: |mut ctx: Context<i::State>| {
            ctx.raise_exc(exc::code::BREAKPOINT)
        },
    },
    {
        name: Cop0,
        type: i,
        asm: ["cop0"],
        fn: |mut ctx: Context<i::State>| {
            match ctx.opx.imm & 0b111111 {
                0 => match ctx.opx.rs.index {
                    0 => {
                        tracing::trace!("mfc0");
                        ctx.reg.set_gpr(ctx.opx.rt.index, ctx.reg.cpr((ctx.opx.imm >> 11).into()));

                        PcBehavior::Increments
                    }
                    2 => {
                        tracing::trace!("cfc0");

                        // COP0 doesn't support this instruction.
                        ctx.raise_exc(exc::code::RESERVED_INSTR)
                    }
                    4 => {
                        tracing::trace!("mtc0");
                        ctx.reg.set_cpr(usize::from(ctx.opx.imm >> 11), ctx.opx.rt.gpr_value);

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
        fn: |mut ctx: Context<i::State>| {
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
        fn: |mut ctx: Context<i::State>| {
            // The PSX CPU lacks a coprocessor #3.
            ctx.raise_exc(exc::code::COP_UNUSABLE)
        },
    },
    {
        name: Div,
        type: r,
        asm: ["div"  %(rs), %(rt)],
        fn: |ctx: Context<r::State>| {
            // Note: MIPS I specifies that division by 0 is undefined, but to be safe, we'll
            // hardcode it to the fairly-reasonable value of 0.
            *ctx.reg.lo_mut() = ctx.opx.rs.gpr_value.checked_div(ctx.opx.rt.gpr_value).unwrap_or(0);
            *ctx.reg.hi_mut() = ctx.opx.rs.gpr_value % ctx.opx.rt.gpr_value;

            PcBehavior::Increments
        },
    },
    {
        name: Divu,
        type: r,
        asm: ["divu"  %(rs), %(rt)],
        fn: |ctx: Context<r::State>| {
            // See `Div`.
            *ctx.reg.lo_mut() = ctx.opx.rs.gpr_value.checked_div(ctx.opx.rt.gpr_value).unwrap_or(0);
            *ctx.reg.hi_mut() = ctx.opx.rs.gpr_value % ctx.opx.rt.gpr_value;

            PcBehavior::Increments
        },
    },
    {
        name: J,
        type: j,
        asm: ["j" *()],
        fn: |ctx: Context<j::State>| {
            PcBehavior::jumps_without_return(ctx.calc_jump_target(ctx.opx.target))
        },
    },
    {
        name: Jal,
        type: j,
        asm: ["jal" *()],
        fn: |ctx: Context<j::State>| {
            let target_addr = ctx.calc_jump_target(ctx.opx.target);
            ctx.reg.set_gpr(31, ctx.calc_ret_addr());

            PcBehavior::calls(target_addr)
        },
    },
    {
        name: Jalr,
        type: r,
        asm: ["jalr" %(rd), %(rs)],
        fn: |ctx: Context<r::State>| {
            let target_addr = ctx.opx.rs.gpr_value;
            ctx.reg.set_gpr(31, ctx.calc_ret_addr());

            PcBehavior::calls(target_addr)
        },
    },
    {
        name: Jr,
        type: r,
        asm: ["jr" %(rs)],
        fn: |ctx: Context<r::State>| {
            let target_addr = ctx.opx.rs.gpr_value;

            if ctx.opx.rs.index == 31 {
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
        fn: |mut ctx: Context<i::State>| {
            if let Ok(value) = ctx.mem.read_8(calc_vaddr(ctx.opx.rs.gpr_value, ctx.opx.imm)) {
                ctx.reg.set_gpr(ctx.opx.rt.index, sign_extend_8(value));

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
        fn: |mut ctx: Context<i::State>| {
            if let Ok(value) = ctx.mem.read_8(calc_vaddr(ctx.opx.rs.gpr_value, ctx.opx.imm)) {
                ctx.reg.set_gpr(ctx.opx.rt.index, value.into());

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
        fn: |mut ctx: Context<i::State>| {
            if let Ok(value) = ctx.mem.read_16(calc_vaddr(ctx.opx.rs.gpr_value, ctx.opx.imm)) {
                ctx.reg.set_gpr(ctx.opx.rt.index, sign_extend_16(value));

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
        fn: |mut ctx: Context<i::State>| {
            if let Ok(value) = ctx.mem.read_16(calc_vaddr(ctx.opx.rs.gpr_value, ctx.opx.imm)) {
                ctx.reg.set_gpr(ctx.opx.rt.index, value.into());

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
        fn: |ctx: Context<i::State>| {
            ctx.reg.set_gpr(ctx.opx.rt.index, u32::from(ctx.opx.imm) << 16);

            PcBehavior::Increments
        },
    },
    {
        name: Lw,
        type: i,
        asm: ["lw" %(rt), #(s), %(rs)],
        fn: |mut ctx: Context<i::State>| {
            if let Ok(value) = ctx.mem.read_32(calc_vaddr(ctx.opx.rs.gpr_value, ctx.opx.imm)) {
                ctx.reg.set_gpr(ctx.opx.rt.index, value);

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
        fn: |_: Context<i::State>| todo!(),
    },
    {
        name: Lwc1,
        type: i,
        asm: ["lwc1" %(rt), #(s)],
        fn: |mut ctx: Context<i::State>| {
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
        fn: |mut ctx: Context<i::State>| {
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
        fn: |ctx: Context<r::State>| {
            ctx.reg.set_gpr(ctx.opx.rd.index, ctx.reg.hi());

            PcBehavior::Increments
        },
    },
    {
        name: Mflo,
        type: r,
        asm: ["mflo" %(rd)],
        fn: |ctx: Context<r::State>| {
            ctx.reg.set_gpr(ctx.opx.rd.index, ctx.reg.lo());

            PcBehavior::Increments
        },
    },
    {
        name: Mthi,
        type: r,
        asm: ["mthi" %(rd)],
        fn: |ctx: Context<r::State>| {
            *ctx.reg.hi_mut() = ctx.opx.rs.gpr_value;

            PcBehavior::Increments
        },
    },
    {
        name: Mtlo,
        type: r,
        asm: ["mtlo" %(rd)],
        fn: |ctx: Context<r::State>| {
            *ctx.reg.lo_mut() = ctx.opx.rs.gpr_value;

            PcBehavior::Increments
        },
    },
    {
        name: Mult,
        type: r,
        asm: ["mult" %(rs), %(rt)],
        fn: |ctx: Context<r::State>| {
            // SAFETY: Overflow is contained within `value`; this is currently how
            // [`u32::widening_mul`] is implemented.
            let value: u64 = unsafe {
                (ctx.opx.rs.gpr_value as i64).unchecked_mul(ctx.opx.rt.gpr_value as i64) as u64
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
        fn: |ctx: Context<r::State>| {
            let (lo, hi) = ctx.opx.rs.gpr_value.widening_mul(ctx.opx.rt.gpr_value);
            *ctx.reg.lo_mut() = lo;
            *ctx.reg.hi_mut() = hi;

            PcBehavior::Increments
        },
    },
    {
        name: Nor,
        type: r,
        asm: ["nor" %(rd), %(rs), %(rt)],
        fn: |ctx: Context<r::State>| {
            ctx.reg.set_gpr(ctx.opx.rd.index, !(ctx.opx.rs.gpr_value | ctx.opx.rt.gpr_value));

            PcBehavior::Increments
        },
    },
    {
        name: Or,
        type: r,
        asm: ["or" %(rd), %(rs), %(rt)],
        fn: |ctx: Context<r::State>| {
            ctx.reg.set_gpr(ctx.opx.rd.index, ctx.opx.rs.gpr_value | ctx.opx.rt.gpr_value);

            PcBehavior::Increments
        },
    },
    {
        name: Ori,
        type: i,
        asm: ["ori" %(rt), %(rs), #(u)],
        fn: |ctx: Context<i::State>| {
            ctx.reg.set_gpr(ctx.opx.rt.index, ctx.opx.rs.gpr_value | u32::from(ctx.opx.imm));

            PcBehavior::Increments
        },
    },
    {
        name: Sb,
        type: i,
        asm: ["sb" %(rt), #(s), %(rs)],
        fn: |mut ctx: Context<i::State>| {
            let vaddr = calc_vaddr(ctx.opx.rs.gpr_value, ctx.opx.imm);

            if ctx.mem.write_8(vaddr, ctx.opx.rt.gpr_value as u8).is_err() {
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
        fn: |mut ctx: Context<i::State>| {
            let vaddr = calc_vaddr(ctx.opx.rs.gpr_value, ctx.opx.imm);

            if ctx.mem.write_16(vaddr, ctx.opx.rt.gpr_value as u16).is_err() {
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
        fn: |ctx: Context<r::State>| {
            ctx.reg.set_gpr(ctx.opx.rd.index, ctx.opx.rt.gpr_value << ctx.opx.shamt);

            PcBehavior::Increments
        },
    },
    {
        name: Sllv,
        type: r,
        asm: ["sllv" %(rd), %(rt), %(rs)],
        fn: |ctx: Context<r::State>| {
            ctx.reg.set_gpr(ctx.opx.rd.index, ctx.opx.rt.gpr_value << (ctx.opx.rs.gpr_value & 0b11111));

            PcBehavior::Increments
        },
    },
    {
        name: Slt,
        type: r,
        asm: ["slt" %(rd), %(rs), %(rt)],
        fn: |ctx: Context<r::State>| {
            ctx.reg.set_gpr(ctx.opx.rd.index, ((ctx.opx.rs.gpr_value as i32) < (ctx.opx.rt.gpr_value as i32)) as u32);

            PcBehavior::Increments
        },
    },
    {
        name: Slti,
        type: i,
        asm: ["slti" %(rt), %(rs), #(s)],
        fn: |ctx: Context<i::State>| {
            ctx.reg.set_gpr(ctx.opx.rt.index, ((ctx.opx.rs.gpr_value as i32) < (sign_extend_16(ctx.opx.imm) as i32)) as u32);

            PcBehavior::Increments
        },
    },
    {
        name: Sltiu,
        type: i,
        asm: ["sltiu" %(rt), %(rs), #(s)],
        fn: |ctx: Context<i::State>| {
            ctx.reg.set_gpr(ctx.opx.rt.index, (ctx.opx.rs.gpr_value < sign_extend_16(ctx.opx.imm)) as u32);

            PcBehavior::Increments
        },
    },
    {
        name: Sltu,
        type: r,
        asm: ["sltu" %(rd), %(rs), %(rt)],
        fn: |ctx: Context<r::State>| {
            ctx.reg.set_gpr(ctx.opx.rd.index, (ctx.opx.rs.gpr_value < ctx.opx.rt.gpr_value) as u32);

            PcBehavior::Increments
        },
    },
    {
        name: Sra,
        type: r,
        asm: ["sra" %(rd), %(rt), ^()],
        fn: |ctx: Context<r::State>| {
            ctx.reg.set_gpr(ctx.opx.rd.index, ((ctx.opx.rt.gpr_value as i32) >> ctx.opx.shamt) as u32);

            PcBehavior::Increments
        },
    },
    {
        name: Srav,
        type: r,
        asm: ["srav"  %(rd), %(rt), %(rs)],
        fn: |ctx: Context<r::State>| {
            ctx.reg.set_gpr(ctx.opx.rd.index, ((ctx.opx.rt.gpr_value as i32) >> (ctx.opx.rs.gpr_value & 0b11111)) as u32);

            PcBehavior::Increments
        },
    },
    {
        name: Srl,
        type: r,
        asm: ["srl" %(rd), %(rt), ^()],
        fn: |ctx: Context<r::State>| {
            ctx.reg.set_gpr(ctx.opx.rd.index, ctx.opx.rt.gpr_value >> ctx.opx.shamt);

            PcBehavior::Increments
        },
    },
    {
        name: Srlv,
        type: r,
        asm: ["srlv"  %(rd), %(rt), %(rs)],
        fn: |ctx: Context<r::State>| {
            ctx.reg.set_gpr(ctx.opx.rd.index, ctx.opx.rt.gpr_value >> (ctx.opx.rs.gpr_value & 0b11111));

            PcBehavior::Increments
        },
    },
    {
        name: Sub,
        type: r,
        asm: ["sub" %(rd), %(rs), %(rt)],
        fn: |mut ctx: Context<r::State>| {
            let (result, overflowed) = (ctx.opx.rs.gpr_value as i32)
                .overflowing_sub(ctx.opx.rt.gpr_value as i32);

            if overflowed {
                ctx.raise_exc(exc::code::INTEGER_OVERFLOW)
            } else {
                ctx.reg.set_gpr(ctx.opx.rd.index, result as u32);

                PcBehavior::Increments
            }
        },
    },
    {
        name: Subu,
        type: r,
        asm: ["subu" %(rd), %(rs), %(rt)],
        fn: |ctx: Context<r::State>| {
            ctx.reg.set_gpr(ctx.opx.rd.index, ctx.opx.rs.gpr_value.wrapping_sub(ctx.opx.rt.gpr_value));

            PcBehavior::Increments
        },
    },
    {
        name: Sw,
        type: i,
        asm: ["sw" %(rt), #(s), %(rs)],
        fn: |mut ctx: Context<i::State>| {
            let vaddr = calc_vaddr(ctx.opx.rs.gpr_value, ctx.opx.imm);

            if ctx.mem.write_32(vaddr, ctx.opx.rt.gpr_value).is_err() {
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
        fn: |mut ctx: Context<i::State>| {
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
        fn: |mut ctx: Context<i::State>| {
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
        fn: |mut ctx: Context<i::State>| {
            ctx.raise_exc(exc::code::SYSCALL)
        },
    },
    {
        name: Xor,
        type: r,
        asm: ["xor" %(rd), %(rs), %(rt)],
        fn: |ctx: Context<r::State>| {
            ctx.reg.set_gpr(ctx.opx.rd.index, ctx.opx.rs.gpr_value ^ ctx.opx.rt.gpr_value);

            PcBehavior::Increments
        },
    },
    {
        name: Xori,
        type: i,
        asm: ["xori" %(rt), %(rs), #(u)],
        fn: |ctx: Context<i::State>| {
            ctx.reg.set_gpr(ctx.opx.rt.index, ctx.opx.rs.gpr_value ^ u32::from(ctx.opx.imm));

            PcBehavior::Increments
        },
    },
);
