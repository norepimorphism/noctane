// SPDX-License-Identifier: MPL-2.0

pub mod asm;
pub mod decode;

use std::fmt;

pub use asm::Asm;

use crate::{exc, reg, Memory};

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
    // The slot for instructions which have been fetched and are ready for the "Read and Decode"
    // pipestage.
    //
    // "Lies!", you say. "How can there be 5 pipestages when there is only one field?" As it turns
    // out, only one field is needed to represent all 5 pipestages. See the [`Self::advance`] method
    // for details.
    rd: Option<Fetch>,
}

impl fmt::Display for Pipeline {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            self.rd.as_ref().map_or_else(
                || String::from("(none)"),
                |slot| slot.instr.asm().to_string(),
            ),
        )
    }
}

/// The result of an instruction fetch.
#[derive(Clone, Copy, Debug)]
pub struct Fetch {
    /// The program address at which the fetched instruction is located.
    pub addr: u32,
    /// The instruction fetched.
    pub instr: Instr,
}

impl Pipeline {
    /// Advances each instruction contained within this pipeline to the next stage of execution.
    ///
    /// The `decode_instr` argument decodes a given opcode into an instruction.
    pub fn advance(
        &mut self,
        exc: &mut exc::Queue,
        mem: &mut Memory,
        reg: &mut reg::File,
        decode_instr: &impl Fn(u32) -> Instr,
    ) -> Fetch {
        self.execute_queued_instr(exc, mem, reg, decode_instr);
        let fetch = self.fetch_next_instr(mem, reg, decode_instr);
        self.rd = Some(fetch);

        fetch
    }

    fn execute_queued_instr(
        &mut self,
        exc: &mut exc::Queue,
        mem: &mut Memory,
        reg: &mut reg::File,
        decode_instr: &impl Fn(u32) -> Instr,
    ) {
        if let Some(slot) = self.rd.take() {
            let mut state = opx::State::read(slot.instr, reg);

            let mut target = None;
            let exec = state.execute(reg, mem, slot.addr, &mut target);
            if let Err(e) = exec {
                // An exception occurred, so we push it to the back of the exception queue.
                exc.push_back(e);
            } else if let Some(target) = target {
                // This is a jump instruction, and it is requesting to jump to the program address
                // identified by `target`. Per the MIPS I architecture, the instruction in the delay
                // slot, or the instruction following the jump instruction, *must* be executed
                // before the branch is taken (if it is taken) (RM[1-7]). So, we want to do the
                // following:
                // - fetch the instruction in the delay slot
                // - execute the instruction in the delay slot
                // - fetch the instruction located at `target`
                //
                // The last step already occurs at the end of [`Self::advance`], so we only need to
                // do the first two ourselves.

                let fetch = self.fetch_next_instr(mem, reg, decode_instr);
                self.rd = Some(fetch);
                self.execute_queued_instr(exc, mem, reg, decode_instr);
                *reg.pc_mut() = target;
            }
        }
    }

    fn fetch_next_instr(
        &mut self,
        mem: &mut Memory,
        reg: &mut reg::File,
        decode_instr: &impl Fn(u32) -> Instr,
    ) -> Fetch {
        let pc = reg.pc();
        let op = mem.read_32(pc);
        let instr = decode_instr(op);
        // Increment the PC.
        *reg.pc_mut() = pc.wrapping_add(4);

        Fetch { addr: pc, instr }
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

            use super::{Instr, Memory, exc::{self, Exception}, reg};

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
                    target: &mut Option<u32>,
                ) -> Result<(), Exception> {
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
                                    target: &'a mut Option<u32>,
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
                                }

                                // tracing::trace!("{:?}", opx);

                                $fn(Context { opx, reg, mem, instr_addr, target })
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
                Err(Exception::new(exc::code::INTEGER_OVERFLOW, ctx.instr_addr))
            } else {
                ctx.reg.set_gpr(ctx.opx.rd.index, result as u32);

                Ok(())
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
                Err(Exception::new(exc::code::INTEGER_OVERFLOW, ctx.instr_addr))
            } else {
                ctx.reg.set_gpr(ctx.opx.rt.index, result as u32);

                Ok(())
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

            Ok(())
        },
    },
    {
        name: Addu,
        type: r,
        asm: ["addu" %(rd), %(rs), %(rt)],
        fn: |ctx: Context| {
            // This operation is unsigned, so no need to test for overflow.
            ctx.reg.set_gpr(ctx.opx.rd.index, ctx.opx.rs.gpr_value.wrapping_add(ctx.opx.rt.gpr_value));

            Ok(())
        },
    },
    {
        name: And,
        type: r,
        asm: ["and" %(rd), %(rs), %(rt)],
        fn: |ctx: Context| {
            ctx.reg.set_gpr(ctx.opx.rd.index, ctx.opx.rs.gpr_value & ctx.opx.rt.gpr_value);

            Ok(())
        },
    },
    {
        name: Andi,
        type: i,
        asm: ["andi" %(rt), %(rs), #(s)],
        fn: |ctx: Context| {
            ctx.reg.set_gpr(ctx.opx.rt.index, ctx.opx.rs.gpr_value & u32::from(ctx.opx.imm));

            Ok(())
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
                        *ctx.target = Some(ctx.calc_branch_target(ctx.opx.imm));
                    }

                    Ok(())
                }
                1 => {
                    tracing::trace!("bgez");
                    if (ctx.opx.rs.gpr_value as i32) >= 0 {
                        *ctx.target = Some(ctx.calc_branch_target(ctx.opx.imm));
                    }

                    Ok(())
                }
                16 => {
                    tracing::trace!("bltzal");
                    todo!();
                }
                17 => {
                    tracing::trace!("bgezal");
                    todo!();
                }
                _ => Err(Exception::new(exc::code::RESERVED_INSTR, ctx.instr_addr)),
            }
        },
    },
    {
        name: Beq,
        type: i,
        asm: ["beq" %(rs), %(rt), #(s)],
        fn: |ctx: Context| {
            if ctx.opx.rs.gpr_value == ctx.opx.rt.gpr_value {
                *ctx.target = Some(ctx.calc_branch_target(ctx.opx.imm));
            }

            Ok(())
        },
    },
    {
        name: Bgtz,
        type: i,
        asm: ["bgtz" %(rs), #(s)],
        fn: |ctx: Context| {
            if (ctx.opx.rs.gpr_value as i32) > 0 {
                *ctx.target = Some(ctx.calc_branch_target(ctx.opx.imm));
            }

            Ok(())
        },
    },
    {
        name: Blez,
        type: i,
        asm: ["blez" %(rs), #(s)],
        fn: |ctx: Context| {
            if (ctx.opx.rs.gpr_value as i32) <= 0 {
                *ctx.target = Some(ctx.calc_branch_target(ctx.opx.imm));
            }

            Ok(())
        },
    },
    {
        name: Bne,
        type: i,
        asm: ["bne" %(rs), %(rt), #(s)],
        fn: |ctx: Context| {
            if ctx.opx.rs.gpr_value != ctx.opx.rt.gpr_value {
                *ctx.target = Some(ctx.calc_branch_target(ctx.opx.imm));
            }

            Ok(())
        },
    },
    {
        name: Break,
        type: i,
        asm: ["break"],
        fn: |ctx: Context| {
            Err(Exception::new(exc::code::BREAKPOINT, ctx.instr_addr))
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

                        Ok(())
                    }
                    2 => {
                        tracing::trace!("cfc0");

                        // COP0 doesn't support this instruction.
                        Err(Exception::new(exc::code::RESERVED_INSTR, ctx.instr_addr))
                    }
                    4 => {
                        tracing::trace!("mtc0");
                        ctx.reg.set_cpr(usize::from(ctx.opx.imm >> 11), ctx.opx.rt.gpr_value);

                        Ok(())
                    }
                    6 => {
                        tracing::trace!("ctc0");

                        // COP0 doesn't support this instruction.
                        Err(Exception::new(exc::code::RESERVED_INSTR, ctx.instr_addr))
                    }
                    _ => {
                        Err(Exception::new(exc::code::RESERVED_INSTR, ctx.instr_addr))
                    }
                }
                // The PSX CPU doesn't implement an MMU... so page table instructions are reserved.
                1 => {
                    tracing::trace!("tlbr");

                    Err(Exception::new(exc::code::RESERVED_INSTR, ctx.instr_addr))
                }
                2 => {
                    tracing::trace!("tlbwi");

                    Err(Exception::new(exc::code::RESERVED_INSTR, ctx.instr_addr))
                }
                8 => {
                    tracing::trace!("tlbp");

                    Err(Exception::new(exc::code::RESERVED_INSTR, ctx.instr_addr))
                }
                16 => {
                    tracing::trace!("rfe");

                    let mut sr = reg::cpr::Status(ctx.reg.cpr(reg::cpr::STATUS_IDX));
                    sr.set_ie_c(sr.ie_p());
                    sr.set_ku_c(sr.ku_p());
                    sr.set_ie_p(sr.ie_o());
                    sr.set_ku_p(sr.ku_o());
                    ctx.reg.set_cpr(reg::cpr::STATUS_IDX, sr.0);

                    Ok(())
                }
                _ => {
                    Err(Exception::new(exc::code::RESERVED_INSTR, ctx.instr_addr))
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
            Err(Exception::new(exc::code::COP_UNUSABLE, ctx.instr_addr))
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
            Err(Exception::new(exc::code::COP_UNUSABLE, ctx.instr_addr))
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

            Ok(())
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

            Ok(())
        },
    },
    {
        name: J,
        type: j,
        asm: ["j" *()],
        fn: |ctx: Context| {
            *ctx.target = Some(ctx.calc_jump_target(ctx.opx.target));

            Ok(())
        },
    },
    {
        name: Jal,
        type: j,
        asm: ["jal" *()],
        fn: |ctx: Context| {
            let target_addr = ctx.calc_jump_target(ctx.opx.target);
            let ret_addr = ctx.calc_ret_addr();

            *ctx.target = Some(target_addr);
            ctx.reg.set_gpr(31, ret_addr);
            log_enter_function(target_addr, ret_addr, ctx.reg.gpr(29));

            Ok(())
        },
    },
    {
        name: Jalr,
        type: r,
        asm: ["jalr" %(rd), %(rs)],
        fn: |ctx: Context| {
            let target_addr = ctx.opx.rs.gpr_value;
            let ret_addr = ctx.calc_ret_addr();

            *ctx.target = Some(target_addr);
            ctx.reg.set_gpr(31, ret_addr);
            log_enter_function(target_addr, ret_addr, ctx.reg.gpr(29));

            Ok(())
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

            *ctx.target = Some(ctx.opx.rs.gpr_value);

            Ok(())
        },
    },
    {
        name: Lb,
        type: i,
        asm: ["lb" %(rt), #(s)],
        fn: |ctx: Context| {
            let value = ctx.mem.read_8(calc_vaddr(ctx.opx.rs.gpr_value, ctx.opx.imm));
            ctx.reg.set_gpr(ctx.opx.rt.index, sign_extend_8(value));

            Ok(())
        },
    },
    {
        name: Lbu,
        type: i,
        asm: ["lbu" %(rt), #(s)],
        fn: |ctx: Context| {
            let value = ctx.mem.read_8(calc_vaddr(ctx.opx.rs.gpr_value, ctx.opx.imm));
            ctx.reg.set_gpr(ctx.opx.rt.index, value.into());

            Ok(())
        },
    },
    {
        name: Lh,
        type: i,
        asm: ["lh" %(rt), #(s)],
        fn: |ctx: Context| {
            let value = ctx.mem.read_16(calc_vaddr(ctx.opx.rs.gpr_value, ctx.opx.imm));
            ctx.reg.set_gpr(ctx.opx.rt.index, sign_extend_16(value));

            Ok(())
        },
    },
    {
        name: Lhu,
        type: i,
        asm: ["lhu" %(rt), #(s)],
        fn: |ctx: Context| {
            let value = ctx.mem.read_16(calc_vaddr(ctx.opx.rs.gpr_value, ctx.opx.imm));
            ctx.reg.set_gpr(ctx.opx.rt.index, value.into());

            Ok(())
        },
    },
    {
        name: Lui,
        type: i,
        asm: ["lui" %(rt), #(s)],
        fn: |ctx: Context| {
            ctx.reg.set_gpr(ctx.opx.rt.index, u32::from(ctx.opx.imm) << 16);

            Ok(())
        },
    },
    {
        name: Lw,
        type: i,
        asm: ["lw" %(rt), #(s)],
        fn: |ctx: Context| {
            let value = ctx.mem.read_32(calc_vaddr(ctx.opx.rs.gpr_value, ctx.opx.imm));
            ctx.reg.set_gpr(ctx.opx.rt.index, value);

            Ok(())
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
            Err(Exception::new(exc::code::COP_UNUSABLE, ctx.instr_addr))
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
            Err(Exception::new(exc::code::COP_UNUSABLE, ctx.instr_addr))
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

            Ok(())
        },
    },
    {
        name: Mflo,
        type: r,
        asm: ["mflo" %(rd)],
        fn: |ctx: Context| {
            ctx.reg.set_gpr(ctx.opx.rd.index, ctx.reg.lo());

            Ok(())
        },
    },
    {
        name: Mthi,
        type: r,
        asm: ["mthi" %(rd)],
        fn: |ctx: Context| {
            *ctx.reg.hi_mut() = ctx.opx.rs.gpr_value;

            Ok(())
        },
    },
    {
        name: Mtlo,
        type: r,
        asm: ["mtlo" %(rd)],
        fn: |ctx: Context| {
            *ctx.reg.lo_mut() = ctx.opx.rs.gpr_value;

            Ok(())
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

            Ok(())
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

            Ok(())
        },
    },
    {
        name: Nor,
        type: r,
        asm: ["nor" %(rd), %(rs), %(rt)],
        fn: |ctx: Context| {
            ctx.reg.set_gpr(ctx.opx.rd.index, !(ctx.opx.rs.gpr_value | ctx.opx.rt.gpr_value));

            Ok(())
        },
    },
    {
        name: Or,
        type: r,
        asm: ["or" %(rd), %(rs), %(rt)],
        fn: |ctx: Context| {
            ctx.reg.set_gpr(ctx.opx.rd.index, ctx.opx.rs.gpr_value | ctx.opx.rt.gpr_value);

            Ok(())
        },
    },
    {
        name: Ori,
        type: i,
        asm: ["ori" %(rt), %(rs), #(s)],
        fn: |ctx: Context| {
            ctx.reg.set_gpr(ctx.opx.rt.index, ctx.opx.rs.gpr_value | u32::from(ctx.opx.imm));

            Ok(())
        },
    },
    {
        name: Sb,
        type: i,
        asm: ["sb" %(rt), #(s)],
        fn: |ctx: Context| {
            let vaddr = calc_vaddr(ctx.opx.rs.gpr_value, ctx.opx.imm);
            ctx.mem.write_8(vaddr, ctx.opx.rt.gpr_value as u8);

            Ok(())
        },
    },
    {
        name: Sh,
        type: i,
        asm: ["sh" %(rt), #(s)],
        fn: |ctx: Context| {
            let vaddr = calc_vaddr(ctx.opx.rs.gpr_value, ctx.opx.imm);
            ctx.mem.write_16(vaddr, ctx.opx.rt.gpr_value as u16);

            Ok(())
        },
    },
    {
        name: Sll,
        type: r,
        asm: ["sll" %(rd), %(rt), ^()],
        fn: |ctx: Context| {
            ctx.reg.set_gpr(ctx.opx.rd.index, ctx.opx.rt.gpr_value << ctx.opx.shamt);

            Ok(())
        },
    },
    {
        name: Sllv,
        type: r,
        asm: ["sllv" %(rd), %(rt), %(rs)],
        fn: |ctx: Context| {
            ctx.reg.set_gpr(ctx.opx.rd.index, ctx.opx.rt.gpr_value << (ctx.opx.rs.gpr_value & 0b11111));

            Ok(())
        },
    },
    {
        name: Slt,
        type: r,
        asm: ["slt" %(rd), %(rs), %(rt)],
        fn: |ctx: Context| {
            ctx.reg.set_gpr(ctx.opx.rd.index, ((ctx.opx.rs.gpr_value as i32) < (ctx.opx.rt.gpr_value as i32)) as u32);

            Ok(())
        },
    },
    {
        name: Slti,
        type: i,
        asm: ["slti" %(rt), %(rs), #(s)],
        fn: |ctx: Context| {
            ctx.reg.set_gpr(ctx.opx.rt.index, ((ctx.opx.rs.gpr_value as i32) < (sign_extend_16(ctx.opx.imm) as i32)) as u32);

            Ok(())
        },
    },
    {
        name: Sltiu,
        type: i,
        asm: ["sltiu" %(rt), %(rs), #(s)],
        fn: |ctx: Context| {
            ctx.reg.set_gpr(ctx.opx.rt.index, (ctx.opx.rs.gpr_value < sign_extend_16(ctx.opx.imm)) as u32);

            Ok(())
        },
    },
    {
        name: Sltu,
        type: r,
        asm: ["sltu" %(rd), %(rs), %(rt)],
        fn: |ctx: Context| {
            ctx.reg.set_gpr(ctx.opx.rd.index, (ctx.opx.rs.gpr_value < ctx.opx.rt.gpr_value) as u32);

            Ok(())
        },
    },
    {
        name: Sra,
        type: r,
        asm: ["sra" %(rd), %(rt), ^()],
        fn: |ctx: Context| {
            ctx.reg.set_gpr(ctx.opx.rd.index, ((ctx.opx.rt.gpr_value as i32) >> ctx.opx.shamt) as u32);

            Ok(())
        },
    },
    {
        name: Srav,
        type: r,
        asm: ["srav"  %(rd), %(rt), %(rs)],
        fn: |ctx: Context| {
            ctx.reg.set_gpr(ctx.opx.rd.index, ((ctx.opx.rt.gpr_value as i32) >> (ctx.opx.rs.gpr_value & 0b11111)) as u32);

            Ok(())
        },
    },
    {
        name: Srl,
        type: r,
        asm: ["srl" %(rd), %(rt), ^()],
        fn: |ctx: Context| {
            ctx.reg.set_gpr(ctx.opx.rd.index, ctx.opx.rt.gpr_value >> ctx.opx.shamt);

            Ok(())
        },
    },
    {
        name: Srlv,
        type: r,
        asm: ["srlv"  %(rd), %(rt), %(rs)],
        fn: |ctx: Context| {
            ctx.reg.set_gpr(ctx.opx.rd.index, ctx.opx.rt.gpr_value >> (ctx.opx.rs.gpr_value & 0b11111));

            Ok(())
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
                Err(Exception::new(exc::code::INTEGER_OVERFLOW, ctx.instr_addr))
            } else {
                ctx.reg.set_gpr(ctx.opx.rd.index, result as u32);

                Ok(())
            }
        },
    },
    {
        name: Subu,
        type: r,
        asm: ["subu" %(rd), %(rs), %(rt)],
        fn: |ctx: Context| {
            ctx.reg.set_gpr(ctx.opx.rd.index, ctx.opx.rs.gpr_value.wrapping_sub(ctx.opx.rt.gpr_value));

            Ok(())
        },
    },
    {
        name: Sw,
        type: i,
        asm: ["sw" %(rt), #(s)],
        fn: |ctx: Context| {
            let vaddr = calc_vaddr(ctx.opx.rs.gpr_value, ctx.opx.imm);
            ctx.mem.write_32(vaddr, ctx.opx.rt.gpr_value);

            Ok(())
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
            Err(Exception::new(exc::code::COP_UNUSABLE, ctx.instr_addr))
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
            Err(Exception::new(exc::code::COP_UNUSABLE, ctx.instr_addr))
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
            Err(Exception::new(exc::code::SYSCALL, ctx.instr_addr))
        },
    },
    {
        name: Xor,
        type: r,
        asm: ["xor" %(rd), %(rs), %(rt)],
        fn: |ctx: Context| {
            ctx.reg.set_gpr(ctx.opx.rd.index, ctx.opx.rs.gpr_value ^ ctx.opx.rt.gpr_value);

            Ok(())
        },
    },
    {
        name: Xori,
        type: i,
        asm: ["xori" %(rt), %(rs), #(s)],
        fn: |ctx: Context| {
            ctx.reg.set_gpr(ctx.opx.rt.index, ctx.opx.rs.gpr_value ^ u32::from(ctx.opx.imm));

            Ok(())
        },
    },
);
