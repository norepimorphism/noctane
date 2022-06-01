// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

pub mod asm;
pub mod decode;

use std::fmt;

pub use asm::Asm;
use crate::{Mmu, exc, reg};

pub mod i {
    use super::{opn::Gpr, reg};

    /// An I-type instruction.
    #[derive(Clone, Copy, Debug, Eq, PartialEq)]
    pub struct Instr {
        pub rs: u8,
        pub rt: u8,
        pub imm: u16,
    }

    impl State {
        pub fn read(reg: &reg::File, instr: Instr) -> Self {
            Self {
                rs: Gpr::read(reg, instr.rs.into()),
                rt: Gpr::read(reg, instr.rt.into()),
                imm: instr.imm,
            }
        }
    }

    #[derive(Debug)]
    pub struct State {
        pub rs: Gpr,
        pub rt: Gpr,
        pub imm: u16,
    }
}

pub mod j {
    use super::reg;

    /// A J-type instruction.
    #[derive(Clone, Copy, Debug, Eq, PartialEq)]
    pub struct Instr {
        pub target: u32,
    }

    impl State {
        pub fn read(_: &reg::File, instr: Instr) -> Self {
            Self {
                target: instr.target,
            }
        }
    }

    #[derive(Debug)]
    pub struct State {
        pub target: u32,
    }
}

pub mod r {
    use super::{opn::Gpr, reg};

    /// An R-type instruction.
    #[derive(Clone, Copy, Debug, Eq, PartialEq)]
    pub struct Instr {
        pub rs: u8,
        pub rt: u8,
        pub rd: u8,
        pub shamt: u8,
        pub funct: u8,
    }

    impl State {
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

    #[derive(Debug)]
    pub struct State {
        pub rs: Gpr,
        pub rt: Gpr,
        pub rd: Gpr,
        pub shamt: u8,
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
#[derive(Default)]
pub struct Pipeline {
    rd: Option<Instr>,
}

impl fmt::Display for Pipeline {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            self.rd.as_ref().map_or_else(
                || String::from("(none)"),
                |instr| instr.asm().to_string(),
            ),
        )
    }
}

pub struct Execution {
    pub pc: u32,
    pub instr: Instr,
}

impl Pipeline {
    pub fn advance(
        &mut self,
        exc: &mut exc::Queue,
        mmu: &mut Mmu,
        reg: &mut reg::File,
        decode_instr: &impl Fn(u32) -> Instr,
    ) -> Option<Execution> {
        let mut pc = reg.pc();
        self.execute_queued_instr(exc, mmu, reg, decode_instr, &mut pc);

        let op = mmu.read_virt_32(pc);
        // Increment PC.
        *reg.pc_mut() = pc.wrapping_add(4);

        let instr = decode_instr(op);
        self.rd = Some(instr);

        Some(Execution { pc, instr })
    }

    fn execute_queued_instr(
        &mut self,
        exc: &mut exc::Queue,
        mmu: &mut Mmu,
        reg: &mut reg::File,
        decode_instr: &impl Fn(u32) -> Instr,
        pc: &mut u32,
    ) {
        if let Some(instr) = self.rd.take() {
            let mut state = opx::State::read(instr, reg);

            let mut target = None;
            if let Err(e) = state.execute(reg, mmu, *pc, &mut target) {
                tracing::error!("Failed to execute instruction: {:?}", e);
                exc.push(e).unwrap();
            } else if let Some(target) = target {
                self.advance(exc, mmu, reg, decode_instr);
                self.execute_queued_instr(exc, mmu, reg, decode_instr, &mut 0);

                *pc = target;
            }
        }
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

            use super::{Instr, Mmu, exc, reg};

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
                    mmu: &mut Mmu,
                    pc: u32,
                    target: &mut Option<u32>,
                ) -> Result<(), exc::Kind> {
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

                    fn calc_branch_target(pc: u32, value: u16) -> u32 {
                        let base = pc;
                        let offset = sign_extend_16(value << 2);
                        tracing::trace!(
                            "Calc. branch target (base={:#010x}, offset={:#010x})",
                            base,
                            offset,
                        );

                        base.wrapping_add(offset)
                    }

                    #[inline(always)]
                    fn calc_jump_target(pc: u32, value: u32) -> u32 {
                        (pc & !((1 << 28) - 1)) | (value << 2)
                    }

                    #[inline(always)]
                    fn calc_ret_addr(pc: u32) -> u32 {
                        pc.wrapping_add(4)
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
                                    mmu: &'a mut Mmu<'c, 'b>,
                                    pc: u32,
                                    target: &'a mut Option<u32>,
                                }

                                // tracing::trace!("{:?}", opx);

                                $fn(Context { opx, reg, mmu, pc, target })
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
                Err(exc::Kind::IntegerOverflow)
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
                Err(exc::Kind::IntegerOverflow)
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
                        *ctx.target = Some(calc_branch_target(ctx.pc, ctx.opx.imm));
                    }

                    Ok(())
                }
                1 => {
                    tracing::trace!("bgez");
                    if (ctx.opx.rs.gpr_value as i32) >= 0 {
                        *ctx.target = Some(calc_branch_target(ctx.pc, ctx.opx.imm));
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
                _ => Err(exc::Kind::ReservedInstr),
            }
        },
    },
    {
        name: Beq,
        type: i,
        asm: ["beq" %(rs), %(rt), #(s)],
        fn: |ctx: Context| {
            if ctx.opx.rs.gpr_value == ctx.opx.rt.gpr_value {
                *ctx.target = Some(calc_branch_target(ctx.pc, ctx.opx.imm));
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
                *ctx.target = Some(calc_branch_target(ctx.pc, ctx.opx.imm));
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
                *ctx.target = Some(calc_branch_target(ctx.pc, ctx.opx.imm));
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
                *ctx.target = Some(calc_branch_target(ctx.pc, ctx.opx.imm));
            }

            Ok(())
        },
    },
    {
        name: Break,
        type: i,
        asm: ["break"],
        fn: |_: Context| {
            Err(exc::Kind::Breakpoint)
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
                        Err(exc::Kind::CopUnusable)
                    }
                    4 => {
                        tracing::trace!("mtc0");
                        ctx.reg.set_cpr(usize::from(ctx.opx.imm >> 11), ctx.opx.rt.gpr_value);

                        Ok(())
                    }
                    6 => {
                        tracing::trace!("ctc0");

                        // COP0 doesn't support this instruction.
                        Err(exc::Kind::CopUnusable)
                    }
                    _ => {
                        Err(exc::Kind::ReservedInstr)
                    }
                }
                1 => {
                    tracing::trace!("tlbr");
                    todo!();
                }
                2 => {
                    tracing::trace!("tlbwi");
                    todo!();
                }
                8 => {
                    tracing::trace!("tlbp");
                    todo!();
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
                    Err(exc::Kind::ReservedInstr)
                }
            }
        },
    },
    {
        name: Cop1,
        type: i,
        asm: ["cop1"],
        fn: |_| {
            // The PSX CPU lacks a coprocessor #1.
            Err(exc::Kind::CopUnusable)
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
        fn: |_| {
            // The PSX CPU lacks a coprocessor #3.
            Err(exc::Kind::CopUnusable)
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
            *ctx.target = Some(calc_jump_target(ctx.pc, ctx.opx.target));

            Ok(())
        },
    },
    {
        name: Jal,
        type: j,
        asm: ["jal" *()],
        fn: |ctx: Context| {
            let target_addr = calc_jump_target(ctx.pc, ctx.opx.target);
            let ret_addr = calc_ret_addr(ctx.pc);

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
            let ret_addr = calc_ret_addr(ctx.pc);

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
            let value = ctx.mmu.read_virt_8(calc_vaddr(ctx.opx.rs.gpr_value, ctx.opx.imm));
            ctx.reg.set_gpr(ctx.opx.rt.index, sign_extend_8(value));

            Ok(())
        },
    },
    {
        name: Lbu,
        type: i,
        asm: ["lbu" %(rt), #(s)],
        fn: |ctx: Context| {
            let value = ctx.mmu.read_virt_8(calc_vaddr(ctx.opx.rs.gpr_value, ctx.opx.imm));
            ctx.reg.set_gpr(ctx.opx.rt.index, value.into());

            Ok(())
        },
    },
    {
        name: Lh,
        type: i,
        asm: ["lh" %(rt), #(s)],
        fn: |ctx: Context| {
            let value = ctx.mmu.read_virt_16(calc_vaddr(ctx.opx.rs.gpr_value, ctx.opx.imm));
            ctx.reg.set_gpr(ctx.opx.rt.index, sign_extend_16(value));

            Ok(())
        },
    },
    {
        name: Lhu,
        type: i,
        asm: ["lhu" %(rt), #(s)],
        fn: |ctx: Context| {
            let value = ctx.mmu.read_virt_16(calc_vaddr(ctx.opx.rs.gpr_value, ctx.opx.imm));
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
            let value = ctx.mmu.read_virt_32(calc_vaddr(ctx.opx.rs.gpr_value, ctx.opx.imm));
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
        fn: |_| {
            // The PSX CPU lacks a coprocessor #1.
            Err(exc::Kind::CopUnusable)
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
        fn: |_| {
            // The PSX CPU lacks a coprocessor #3.
            Err(exc::Kind::CopUnusable)
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
            ctx.mmu.write_virt_8(vaddr, ctx.opx.rt.gpr_value as u8);

            Ok(())
        },
    },
    {
        name: Sh,
        type: i,
        asm: ["sh" %(rt), #(s)],
        fn: |ctx: Context| {
            let vaddr = calc_vaddr(ctx.opx.rs.gpr_value, ctx.opx.imm);
            ctx.mmu.write_virt_16(vaddr, ctx.opx.rt.gpr_value as u16);

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
                Err(exc::Kind::IntegerOverflow)
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
            ctx.mmu.write_virt_32(vaddr, ctx.opx.rt.gpr_value);

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
        fn: |_| {
            // The PSX CPU lacks a coprocessor #1.
            Err(exc::Kind::CopUnusable)
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
        fn: |_| {
            // The PSX CPU lacks a coprocessor #3.
            Err(exc::Kind::CopUnusable)
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
        fn: |_: Context| {
            todo!()
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
