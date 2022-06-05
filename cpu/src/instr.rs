// SPDX-License-Identifier: MPL-2.0

pub mod asm;
pub mod decode;

pub use asm::Asm;

use crate::{exc::{self, Exception}, reg, Memory};

pub mod i {
    use super::{opn::Gpr, reg};

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
                rs: Gpr::read(reg, instr.rs.into()),
                rt: Gpr::read(reg, instr.rt.into()),
                imm: instr.imm,
            }
        }
    }

    /// The working state of an I-type instruction.
    #[derive(Debug)]
    pub struct State {
        /// The register described by the *rs* operand.
        pub rs: Gpr,
        /// The register described by the *rt* operand.
        pub rt: Gpr,
        /// The *imm* operand.
        pub imm: u16,
    }
}

pub mod j {
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
    use super::{opn::Gpr, reg};

    /// An R-type instruction.
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
                rs: Gpr::read(reg, instr.rs.into()),
                rt: Gpr::read(reg, instr.rt.into()),
                rd: Gpr::read(reg, instr.rd.into()),
                shamt: instr.shamt,
                funct: instr.funct,
            }
        }
    }

    /// The working state of an R-type instruction.
    #[derive(Debug)]
    pub struct State {
        /// The register described by the *rs* operand.
        pub rs: Gpr,
        /// The register described by the *rt* operand.
        pub rt: Gpr,
        /// The register described by the *rd* operand.
        pub rd: Gpr,
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
    jump_info: Option<JumpInfo>,
}

pub struct JumpInfo {
    pub target_addr: u32,
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

#[derive(Clone, Debug)]
pub struct Execution {
    pub fetched: Fetched,
    pub behavior: Result<Behavior, Exception>,
}

#[derive(Clone, Copy, Debug)]
pub enum Behavior {
    /// This instruction calls a function.
    Calls(u32),
    /// The instruction jumps to a new program address.
    Jumps(u32),
    /// This instruction returns from the current function.
    Returns,
    Normal,
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
    ) -> Execution {
        if let Some(jump_info) = self.jump_info.take() {
            let exec = self.execute_fetched(
                jump_info.delay_slot,
                mem,
                reg,
                decode_instr,
            );
            *reg.pc_mut() = jump_info.target_addr;

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

    fn fetch_next_instr(
        &mut self,
        mem: &mut Memory,
        reg: &mut reg::File,
        decode_instr: &impl Fn(u32) -> Instr,
    ) -> Fetched {
        let pc = reg.pc();
        let op = mem.read_32(pc);
        let instr = decode_instr(op);
        // Increment the PC.
        *reg.pc_mut() = pc.wrapping_add(4);

        Fetched { addr: pc, op, instr }
    }

    fn execute_fetched(
        &mut self,
        fetched: Fetched,
        mem: &mut Memory,
        reg: &mut reg::File,
        decode_instr: &impl Fn(u32) -> Instr,
    ) -> Execution {
        let mut state = opx::State::read(fetched.instr, reg);
        let behavior = state.execute(reg, mem, fetched.addr);
        if let Ok(Behavior::Calls(target_addr)) | Ok(Behavior::Jumps(target_addr)) = &behavior {
            // This is a jump/call instruction, and it is requesting to jump to the program address
            // identified by `target_addr`. Per the MIPS I architecture, the instruction in the
            // delay slot, or the instruction following the jump instruction, *must* be executed
            // before the branch is taken (if it is taken) (RM[1-7]).
            let delay_slot = self.fetch_next_instr(mem, reg, decode_instr);
            self.jump_info = Some(JumpInfo { target_addr: *target_addr, delay_slot });
        }

        Execution { fetched, behavior }
    }
}

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
        #[derive(Clone, Copy, Debug, Eq, PartialEq)]
        pub enum Instr {
            $(
                $variant_name($ty::Instr),
            )*
        }

        impl Instr {
            pub fn decode(code: u32) -> Option<Self> {
                match Self::try_decode_op_kind(code)? {
                    $(
                        opx::Kind::$variant_name => {
                            Some(Self::$variant_name($ty::Instr::decode(code)))
                        }
                    )*
                }
            }

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

            use super::{Behavior, Instr, Memory, exc::{self, Exception}, reg};

            #[derive(Clone, Copy, Debug, Eq, PartialEq)]
            pub enum Kind {
                $(
                    $variant_name,
                )*
            }

            impl State {
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

            pub enum State {
                $(
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
                pub fn execute(
                    &mut self,
                    reg: &mut reg::File,
                    mem: &mut Memory,
                    instr_addr: u32,
                ) -> Result<Behavior, Exception> {
                    #[inline(always)]
                    fn sign_extend_16(value: u16) -> u32 {
                        ((value as i16) as i32) as u32
                    }

                    #[inline(always)]
                    fn sign_extend_8(value: u8) -> u32 {
                        ((value as i8) as i32) as u32
                    }

                    fn calc_vaddr(base: u32, offset: u16) -> u32 {
                        let offset = sign_extend_16(offset);
                        tracing::trace!(
                            "Calc. virtual address (base={:#010x}, offset={:#010x})",
                            base,
                            offset,
                        );

                        base.wrapping_add(offset)
                    }

                    #[inline(always)]
                    fn log_enter_function(target_addr: u32, ret_addr: u32, sp: u32) {
                        tracing::debug!(
                            "Entering function `sub_{:08X}` (ra={:#010x}, sp={:#010x})",
                            target_addr,
                            ret_addr,
                            sp,
                        );
                    }

                    match self {
                        $(
                            #[allow(dead_code)]
                            State::$variant_name(ref mut opx) => {
                                struct Context<'a, 'c, 'b> {
                                    opx: &'a mut super::$ty::State,
                                    reg: &'a mut reg::File,
                                    mem: &'a mut Memory<'c, 'b>,
                                    instr_addr: u32,
                                }

                                impl Context<'_, '_, '_> {
                                    fn calc_branch_target(&self, value: u16) -> u32 {
                                        let base = self.reg.pc();
                                        let offset = sign_extend_16(value << 2);
                                        tracing::trace!(
                                            "Calc. branch target (base={:#010x}, offset={:#010x})",
                                            base,
                                            offset,
                                        );

                                        base.wrapping_add(offset)
                                    }

                                    #[inline(always)]
                                    fn calc_jump_target(&self, value: u32) -> u32 {
                                        (self.reg.pc() & !((1 << 28) - 1)) | (value << 2)
                                    }

                                    #[inline(always)]
                                    fn calc_ret_addr(&self) -> u32 {
                                        self.reg.pc().wrapping_add(4)
                                    }

                                    fn raise_exc(&self, code: u32) -> Exception {
                                        Exception::new(code, self.reg.pc())
                                    }
                                }

                                // tracing::trace!("{:?}", opx);

                                $fn(Context { opx, reg, mem, instr_addr })
                            }
                        )*
                    }
                }
            }
        }

        pub mod opn {
            //! Instruction operands.

            use super::reg;

            impl Gpr {
                pub fn read(reg: &reg::File, index: usize) -> Self {
                    Self {
                        index: index,
                        gpr_value: reg.gpr(index),
                        cpr_value: reg.cpr(index),
                    }
                }
            }

            /// A general-purpose register (GPR) operand.
            #[derive(Clone, Copy, Debug, Eq, PartialEq)]
            pub struct Gpr {
                /// The zero-based index of this GPR.
                ///
                /// An index of 0 represents `r1` while an index of 30 represents `r31`.
                pub index: usize,
                pub gpr_value: u32,
                pub cpr_value: u32,
            }
        }
    };
}

def_instr_and_op_kind!(
    {
        name: Add,
        type: r,
        asm: ["add" %(rd), %(rs), %(rt)],
        fn: |ctx: Context| {
            let (result, overflowed) = (ctx.opx.rs.gpr_value as i32)
                .overflowing_add(ctx.opx.rt.gpr_value as i32);

            if overflowed {
                Err(ctx.raise_exc(exc::code::INTEGER_OVERFLOW))
            } else {
                ctx.reg.set_gpr(ctx.opx.rd.index, result as u32);

                Ok(Behavior::Normal)
            }
        },
    },
    {
        name: Addi,
        type: i,
        asm: ["addi" %(rt), %(rs), #(s)],
        fn: |ctx: Context| {
            let (result, overflowed) = (ctx.opx.rs.gpr_value as i32)
                .overflowing_add(sign_extend_16(ctx.opx.imm) as i32);

            if overflowed {
                Err(ctx.raise_exc(exc::code::INTEGER_OVERFLOW))
            } else {
                ctx.reg.set_gpr(ctx.opx.rt.index, result as u32);

                Ok(Behavior::Normal)
            }
        },
    },
    {
        name: Addiu,
        type: i,
        asm: ["addiu" %(rt), %(rs), #(s)],
        fn: |ctx: Context| {
            // This operation is unsigned, so no need to test for overflow.
            ctx.reg.set_gpr(ctx.opx.rt.index, ctx.opx.rs.gpr_value.wrapping_add(sign_extend_16(ctx.opx.imm)));

            Ok(Behavior::Normal)
        },
    },
    {
        name: Addu,
        type: r,
        asm: ["addu" %(rd), %(rs), %(rt)],
        fn: |ctx: Context| {
            // This operation is unsigned, so no need to test for overflow.
            ctx.reg.set_gpr(ctx.opx.rd.index, ctx.opx.rs.gpr_value.wrapping_add(ctx.opx.rt.gpr_value));

            Ok(Behavior::Normal)
        },
    },
    {
        name: And,
        type: r,
        asm: ["and" %(rd), %(rs), %(rt)],
        fn: |ctx: Context| {
            ctx.reg.set_gpr(ctx.opx.rd.index, ctx.opx.rs.gpr_value & ctx.opx.rt.gpr_value);

            Ok(Behavior::Normal)
        },
    },
    {
        name: Andi,
        type: i,
        asm: ["andi" %(rt), %(rs), #(u)],
        fn: |ctx: Context| {
            ctx.reg.set_gpr(ctx.opx.rt.index, ctx.opx.rs.gpr_value & u32::from(ctx.opx.imm));

            Ok(Behavior::Normal)
        },
    },
    {
        name: BCond,
        type: i,
        asm: ["b.cond" %(rs), #(s)],
        fn: |ctx: Context| {
            match ctx.opx.rt.index {
                0 => {
                    tracing::trace!("bltz");

                    if (ctx.opx.rs.gpr_value as i32) < 0 {
                        Ok(Behavior::Jumps(ctx.calc_branch_target(ctx.opx.imm)))
                    } else {
                        Ok(Behavior::Normal)
                    }
                }
                1 => {
                    tracing::trace!("bgez");

                    if (ctx.opx.rs.gpr_value as i32) >= 0 {
                        Ok(Behavior::Jumps(ctx.calc_branch_target(ctx.opx.imm)))
                    } else {
                        Ok(Behavior::Normal)
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
                _ => Err(ctx.raise_exc(exc::code::RESERVED_INSTR)),
            }
        },
    },
    {
        name: Beq,
        type: i,
        asm: ["beq" %(rs), %(rt), #(s)],
        fn: |ctx: Context| {
            if ctx.opx.rs.gpr_value == ctx.opx.rt.gpr_value {
                Ok(Behavior::Jumps(ctx.calc_branch_target(ctx.opx.imm)))
            } else {
                Ok(Behavior::Normal)
            }
        },
    },
    {
        name: Bgtz,
        type: i,
        asm: ["bgtz" %(rs), #(s)],
        fn: |ctx: Context| {
            if (ctx.opx.rs.gpr_value as i32) > 0 {
                Ok(Behavior::Jumps(ctx.calc_branch_target(ctx.opx.imm)))
            } else {
                Ok(Behavior::Normal)
            }
        },
    },
    {
        name: Blez,
        type: i,
        asm: ["blez" %(rs), #(s)],
        fn: |ctx: Context| {
            if (ctx.opx.rs.gpr_value as i32) <= 0 {
                Ok(Behavior::Jumps(ctx.calc_branch_target(ctx.opx.imm)))
            } else {
                Ok(Behavior::Normal)
            }
        },
    },
    {
        name: Bne,
        type: i,
        asm: ["bne" %(rs), %(rt), #(s)],
        fn: |ctx: Context| {
            if ctx.opx.rs.gpr_value != ctx.opx.rt.gpr_value {
                Ok(Behavior::Jumps(ctx.calc_branch_target(ctx.opx.imm)))
            } else {
                Ok(Behavior::Normal)
            }
        },
    },
    {
        name: Break,
        type: i,
        asm: ["break"],
        fn: |ctx: Context| {
            Err(ctx.raise_exc(exc::code::BREAKPOINT))
        },
    },
    {
        name: Cop0,
        type: i,
        asm: ["cop0"],
        fn: |ctx: Context| {
            match ctx.opx.imm & 0b111111 {
                0 => match ctx.opx.rs.index {
                    0 => {
                        tracing::trace!("mfc0");
                        ctx.reg.set_gpr(ctx.opx.rt.index, ctx.reg.cpr((ctx.opx.imm >> 11).into()));

                        Ok(Behavior::Normal)
                    }
                    2 => {
                        tracing::trace!("cfc0");

                        // COP0 doesn't support this instruction.
                        Err(ctx.raise_exc(exc::code::RESERVED_INSTR))
                    }
                    4 => {
                        tracing::trace!("mtc0");
                        ctx.reg.set_cpr(usize::from(ctx.opx.imm >> 11), ctx.opx.rt.gpr_value);

                        Ok(Behavior::Normal)
                    }
                    6 => {
                        tracing::trace!("ctc0");

                        // COP0 doesn't support this instruction.
                        Err(ctx.raise_exc(exc::code::RESERVED_INSTR))
                    }
                    _ => {
                        Err(ctx.raise_exc(exc::code::RESERVED_INSTR))
                    }
                }
                // The PSX CPU doesn't implement an MMU... so page table instructions are reserved.
                1 => {
                    tracing::trace!("tlbr");

                    Err(ctx.raise_exc(exc::code::RESERVED_INSTR))
                }
                2 => {
                    tracing::trace!("tlbwi");

                    Err(ctx.raise_exc(exc::code::RESERVED_INSTR))
                }
                8 => {
                    tracing::trace!("tlbp");

                    Err(ctx.raise_exc(exc::code::RESERVED_INSTR))
                }
                16 => {
                    tracing::trace!("rfe");

                    let mut sr = reg::cpr::Status(ctx.reg.cpr(reg::cpr::STATUS_IDX));
                    sr.set_ie_c(sr.ie_p());
                    sr.set_ku_c(sr.ku_p());
                    sr.set_ie_p(sr.ie_o());
                    sr.set_ku_p(sr.ku_o());
                    ctx.reg.set_cpr(reg::cpr::STATUS_IDX, sr.0);

                    Ok(Behavior::Normal)
                }
                _ => {
                    Err(ctx.raise_exc(exc::code::RESERVED_INSTR))
                }
            }
        },
    },
    {
        name: Cop1,
        type: i,
        asm: ["cop1"],
        fn: |ctx: Context| {
            // The PSX CPU lacks a coprocessor #1.
            Err(ctx.raise_exc(exc::code::COP_UNUSABLE))
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
        fn: |ctx: Context| {
            // The PSX CPU lacks a coprocessor #3.
            Err(ctx.raise_exc(exc::code::COP_UNUSABLE))
        },
    },
    {
        name: Div,
        type: r,
        asm: ["div"  %(rs), %(rt)],
        fn: |ctx: Context| {
            // Note: MIPS I specifies that division by 0 is undefined, but to be safe, we'll
            // hardcode it to the fairly-reasonable value of 0.
            *ctx.reg.lo_mut() = ctx.opx.rs.gpr_value.checked_div(ctx.opx.rt.gpr_value).unwrap_or(0);
            *ctx.reg.hi_mut() = ctx.opx.rs.gpr_value % ctx.opx.rt.gpr_value;

            Ok(Behavior::Normal)
        },
    },
    {
        name: Divu,
        type: r,
        asm: ["divu"  %(rs), %(rt)],
        fn: |ctx: Context| {
            // See `Div`.
            *ctx.reg.lo_mut() = ctx.opx.rs.gpr_value.checked_div(ctx.opx.rt.gpr_value).unwrap_or(0);
            *ctx.reg.hi_mut() = ctx.opx.rs.gpr_value % ctx.opx.rt.gpr_value;

            Ok(Behavior::Normal)
        },
    },
    {
        name: J,
        type: j,
        asm: ["j" *()],
        fn: |ctx: Context| {
            Ok(Behavior::Jumps(ctx.calc_jump_target(ctx.opx.target)))
        },
    },
    {
        name: Jal,
        type: j,
        asm: ["jal" *()],
        fn: |ctx: Context| {
            let target_addr = ctx.calc_jump_target(ctx.opx.target);
            let ret_addr = ctx.calc_ret_addr();

            ctx.reg.set_gpr(31, ret_addr);
            log_enter_function(target_addr, ret_addr, ctx.reg.gpr(29));

            Ok(Behavior::Calls(target_addr))
        },
    },
    {
        name: Jalr,
        type: r,
        asm: ["jalr" %(rd), %(rs)],
        fn: |ctx: Context| {
            let target_addr = ctx.opx.rs.gpr_value;
            let ret_addr = ctx.calc_ret_addr();

            ctx.reg.set_gpr(31, ret_addr);
            log_enter_function(target_addr, ret_addr, ctx.reg.gpr(29));

            Ok(Behavior::Calls(target_addr))
        },
    },
    {
        name: Jr,
        type: r,
        asm: ["jr" %(rs)],
        fn: |ctx: Context| {
            if ctx.opx.rs.index == 31 {
                tracing::debug!("Leaving function");
            }

            Ok(Behavior::Jumps(ctx.opx.rs.gpr_value))
        },
    },
    {
        name: Lb,
        type: i,
        asm: ["lb" %(rt), #(s), %(rs)],
        fn: |ctx: Context| {
            let value = ctx.mem.read_8(calc_vaddr(ctx.opx.rs.gpr_value, ctx.opx.imm));
            ctx.reg.set_gpr(ctx.opx.rt.index, sign_extend_8(value));

            Ok(Behavior::Normal)
        },
    },
    {
        name: Lbu,
        type: i,
        asm: ["lbu" %(rt), #(s), %(rs)],
        fn: |ctx: Context| {
            let value = ctx.mem.read_8(calc_vaddr(ctx.opx.rs.gpr_value, ctx.opx.imm));
            ctx.reg.set_gpr(ctx.opx.rt.index, value.into());

            Ok(Behavior::Normal)
        },
    },
    {
        name: Lh,
        type: i,
        asm: ["lh" %(rt), #(s), %(rs)],
        fn: |ctx: Context| {
            let value = ctx.mem.read_16(calc_vaddr(ctx.opx.rs.gpr_value, ctx.opx.imm));
            ctx.reg.set_gpr(ctx.opx.rt.index, sign_extend_16(value));

            Ok(Behavior::Normal)
        },
    },
    {
        name: Lhu,
        type: i,
        asm: ["lhu" %(rt), #(s), %(rs)],
        fn: |ctx: Context| {
            let value = ctx.mem.read_16(calc_vaddr(ctx.opx.rs.gpr_value, ctx.opx.imm));
            ctx.reg.set_gpr(ctx.opx.rt.index, value.into());

            Ok(Behavior::Normal)
        },
    },
    {
        name: Lui,
        type: i,
        asm: ["lui" %(rt), #(s)],
        fn: |ctx: Context| {
            ctx.reg.set_gpr(ctx.opx.rt.index, u32::from(ctx.opx.imm) << 16);

            Ok(Behavior::Normal)
        },
    },
    {
        name: Lw,
        type: i,
        asm: ["lw" %(rt), #(s), %(rs)],
        fn: |ctx: Context| {
            let value = ctx.mem.read_32(calc_vaddr(ctx.opx.rs.gpr_value, ctx.opx.imm));
            ctx.reg.set_gpr(ctx.opx.rt.index, value);

            Ok(Behavior::Normal)
        },
    },
    {
        name: Lwc0,
        type: i,
        asm: ["lwc0" %(rt), #(s)],
        fn: |_: Context| todo!(),
    },
    {
        name: Lwc1,
        type: i,
        asm: ["lwc1" %(rt), #(s)],
        fn: |ctx: Context| {
            // The PSX CPU lacks a coprocessor #1.
            Err(ctx.raise_exc(exc::code::COP_UNUSABLE))
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
        fn: |ctx: Context| {
            // The PSX CPU lacks a coprocessor #3.
            Err(ctx.raise_exc(exc::code::COP_UNUSABLE))
        },
    },
    {
        name: Lwl,
        type: i,
        asm: ["lwl" %(rt), #(s)],
        fn: |_: Context| todo!(),
    },
    {
        name: Lwr,
        type: i,
        asm: ["lwr" %(rt), #(s)],
        fn: |_: Context| todo!(),
    },
    {
        name: Mfhi,
        type: r,
        asm: ["mfhi" %(rd)],
        fn: |ctx: Context| {
            ctx.reg.set_gpr(ctx.opx.rd.index, ctx.reg.hi());

            Ok(Behavior::Normal)
        },
    },
    {
        name: Mflo,
        type: r,
        asm: ["mflo" %(rd)],
        fn: |ctx: Context| {
            ctx.reg.set_gpr(ctx.opx.rd.index, ctx.reg.lo());

            Ok(Behavior::Normal)
        },
    },
    {
        name: Mthi,
        type: r,
        asm: ["mthi" %(rd)],
        fn: |ctx: Context| {
            *ctx.reg.hi_mut() = ctx.opx.rs.gpr_value;

            Ok(Behavior::Normal)
        },
    },
    {
        name: Mtlo,
        type: r,
        asm: ["mtlo" %(rd)],
        fn: |ctx: Context| {
            *ctx.reg.lo_mut() = ctx.opx.rs.gpr_value;

            Ok(Behavior::Normal)
        },
    },
    {
        name: Mult,
        type: r,
        asm: ["mult" %(rs), %(rt)],
        fn: |ctx: Context| {
            // SAFETY: Overflow is contained within `value`; this is currently how
            // [`u32::widening_mul`] is implemented.
            let value: u64 = unsafe {
                (ctx.opx.rs.gpr_value as i64).unchecked_mul(ctx.opx.rt.gpr_value as i64) as u64
            };
            *ctx.reg.lo_mut() = value as u32;
            *ctx.reg.hi_mut() = (value >> 32) as u32;

            Ok(Behavior::Normal)
        },
    },
    {
        name: Multu,
        type: r,
        asm: ["multu"  %(rs), %(rt)],
        fn: |ctx: Context| {
            let (lo, hi) = ctx.opx.rs.gpr_value.widening_mul(ctx.opx.rt.gpr_value);
            *ctx.reg.lo_mut() = lo;
            *ctx.reg.hi_mut() = hi;

            Ok(Behavior::Normal)
        },
    },
    {
        name: Nor,
        type: r,
        asm: ["nor" %(rd), %(rs), %(rt)],
        fn: |ctx: Context| {
            ctx.reg.set_gpr(ctx.opx.rd.index, !(ctx.opx.rs.gpr_value | ctx.opx.rt.gpr_value));

            Ok(Behavior::Normal)
        },
    },
    {
        name: Or,
        type: r,
        asm: ["or" %(rd), %(rs), %(rt)],
        fn: |ctx: Context| {
            ctx.reg.set_gpr(ctx.opx.rd.index, ctx.opx.rs.gpr_value | ctx.opx.rt.gpr_value);

            Ok(Behavior::Normal)
        },
    },
    {
        name: Ori,
        type: i,
        asm: ["ori" %(rt), %(rs), #(u)],
        fn: |ctx: Context| {
            ctx.reg.set_gpr(ctx.opx.rt.index, ctx.opx.rs.gpr_value | u32::from(ctx.opx.imm));

            Ok(Behavior::Normal)
        },
    },
    {
        name: Sb,
        type: i,
        asm: ["sb" %(rt), #(s), %(rs)],
        fn: |ctx: Context| {
            let vaddr = calc_vaddr(ctx.opx.rs.gpr_value, ctx.opx.imm);
            ctx.mem.write_8(vaddr, ctx.opx.rt.gpr_value as u8);

            Ok(Behavior::Normal)
        },
    },
    {
        name: Sh,
        type: i,
        asm: ["sh" %(rt), #(s), %(rs)],
        fn: |ctx: Context| {
            let vaddr = calc_vaddr(ctx.opx.rs.gpr_value, ctx.opx.imm);
            ctx.mem.write_16(vaddr, ctx.opx.rt.gpr_value as u16);

            Ok(Behavior::Normal)
        },
    },
    {
        name: Sll,
        type: r,
        asm: ["sll" %(rd), %(rt), ^()],
        fn: |ctx: Context| {
            ctx.reg.set_gpr(ctx.opx.rd.index, ctx.opx.rt.gpr_value << ctx.opx.shamt);

            Ok(Behavior::Normal)
        },
    },
    {
        name: Sllv,
        type: r,
        asm: ["sllv" %(rd), %(rt), %(rs)],
        fn: |ctx: Context| {
            ctx.reg.set_gpr(ctx.opx.rd.index, ctx.opx.rt.gpr_value << (ctx.opx.rs.gpr_value & 0b11111));

            Ok(Behavior::Normal)
        },
    },
    {
        name: Slt,
        type: r,
        asm: ["slt" %(rd), %(rs), %(rt)],
        fn: |ctx: Context| {
            ctx.reg.set_gpr(ctx.opx.rd.index, ((ctx.opx.rs.gpr_value as i32) < (ctx.opx.rt.gpr_value as i32)) as u32);

            Ok(Behavior::Normal)
        },
    },
    {
        name: Slti,
        type: i,
        asm: ["slti" %(rt), %(rs), #(s)],
        fn: |ctx: Context| {
            ctx.reg.set_gpr(ctx.opx.rt.index, ((ctx.opx.rs.gpr_value as i32) < (sign_extend_16(ctx.opx.imm) as i32)) as u32);

            Ok(Behavior::Normal)
        },
    },
    {
        name: Sltiu,
        type: i,
        asm: ["sltiu" %(rt), %(rs), #(s)],
        fn: |ctx: Context| {
            ctx.reg.set_gpr(ctx.opx.rt.index, (ctx.opx.rs.gpr_value < sign_extend_16(ctx.opx.imm)) as u32);

            Ok(Behavior::Normal)
        },
    },
    {
        name: Sltu,
        type: r,
        asm: ["sltu" %(rd), %(rs), %(rt)],
        fn: |ctx: Context| {
            ctx.reg.set_gpr(ctx.opx.rd.index, (ctx.opx.rs.gpr_value < ctx.opx.rt.gpr_value) as u32);

            Ok(Behavior::Normal)
        },
    },
    {
        name: Sra,
        type: r,
        asm: ["sra" %(rd), %(rt), ^()],
        fn: |ctx: Context| {
            ctx.reg.set_gpr(ctx.opx.rd.index, ((ctx.opx.rt.gpr_value as i32) >> ctx.opx.shamt) as u32);

            Ok(Behavior::Normal)
        },
    },
    {
        name: Srav,
        type: r,
        asm: ["srav"  %(rd), %(rt), %(rs)],
        fn: |ctx: Context| {
            ctx.reg.set_gpr(ctx.opx.rd.index, ((ctx.opx.rt.gpr_value as i32) >> (ctx.opx.rs.gpr_value & 0b11111)) as u32);

            Ok(Behavior::Normal)
        },
    },
    {
        name: Srl,
        type: r,
        asm: ["srl" %(rd), %(rt), ^()],
        fn: |ctx: Context| {
            ctx.reg.set_gpr(ctx.opx.rd.index, ctx.opx.rt.gpr_value >> ctx.opx.shamt);

            Ok(Behavior::Normal)
        },
    },
    {
        name: Srlv,
        type: r,
        asm: ["srlv"  %(rd), %(rt), %(rs)],
        fn: |ctx: Context| {
            ctx.reg.set_gpr(ctx.opx.rd.index, ctx.opx.rt.gpr_value >> (ctx.opx.rs.gpr_value & 0b11111));

            Ok(Behavior::Normal)
        },
    },
    {
        name: Sub,
        type: r,
        asm: ["sub" %(rd), %(rs), %(rt)],
        fn: |ctx: Context| {
            let (result, overflowed) = (ctx.opx.rs.gpr_value as i32)
                .overflowing_sub(ctx.opx.rt.gpr_value as i32);

            if overflowed {
                Err(ctx.raise_exc(exc::code::INTEGER_OVERFLOW))
            } else {
                ctx.reg.set_gpr(ctx.opx.rd.index, result as u32);

                Ok(Behavior::Normal)
            }
        },
    },
    {
        name: Subu,
        type: r,
        asm: ["subu" %(rd), %(rs), %(rt)],
        fn: |ctx: Context| {
            ctx.reg.set_gpr(ctx.opx.rd.index, ctx.opx.rs.gpr_value.wrapping_sub(ctx.opx.rt.gpr_value));

            Ok(Behavior::Normal)
        },
    },
    {
        name: Sw,
        type: i,
        asm: ["sw" %(rt), #(s), %(rs)],
        fn: |ctx: Context| {
            let vaddr = calc_vaddr(ctx.opx.rs.gpr_value, ctx.opx.imm);
            ctx.mem.write_32(vaddr, ctx.opx.rt.gpr_value);

            Ok(Behavior::Normal)
        },
    },
    {
        name: Swc0,
        type: i,
        asm: ["swc0" %(rt), #(s)],
        fn: |_: Context| todo!(),
    },
    {
        name: Swc1,
        type: i,
        asm: ["swc1" %(rt), #(s)],
        fn: |ctx: Context| {
            // The PSX CPU lacks a coprocessor #1.
            Err(ctx.raise_exc(exc::code::COP_UNUSABLE))
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
        fn: |ctx: Context| {
            // The PSX CPU lacks a coprocessor #3.
            Err(ctx.raise_exc(exc::code::COP_UNUSABLE))
        },
    },
    {
        name: Swl,
        type: i,
        asm: ["swl" %(rt), #(s)],
        fn: |_: Context| todo!(),
    },
    {
        name: Swr,
        type: i,
        asm: ["swr" %(rt), #(s)],
        fn: |_: Context| todo!(),
    },
    {
        name: Syscall,
        type: i,
        asm: ["syscall"],
        fn: |ctx: Context| {
            Err(ctx.raise_exc(exc::code::SYSCALL))
        },
    },
    {
        name: Xor,
        type: r,
        asm: ["xor" %(rd), %(rs), %(rt)],
        fn: |ctx: Context| {
            ctx.reg.set_gpr(ctx.opx.rd.index, ctx.opx.rs.gpr_value ^ ctx.opx.rt.gpr_value);

            Ok(Behavior::Normal)
        },
    },
    {
        name: Xori,
        type: i,
        asm: ["xori" %(rt), %(rs), #(u)],
        fn: |ctx: Context| {
            ctx.reg.set_gpr(ctx.opx.rt.index, ctx.opx.rs.gpr_value ^ u32::from(ctx.opx.imm));

            Ok(Behavior::Normal)
        },
    },
);
