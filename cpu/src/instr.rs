// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

pub mod asm;
pub mod decode;

use std::fmt;

use const_queue::ConstQueue;

pub use asm::Asm;
use crate::cpu::{ExceptionKind, reg};

pub type ExcQueue = ConstQueue::<ExceptionKind, 5>;

pub mod i {
    /// An I-type instruction.
    #[derive(Clone, Copy, Debug, Eq, PartialEq)]
    pub struct Instr {
        pub rs: u8,
        pub rt: u8,
        pub imm: u16,
    }
}

pub mod j {
    /// A J-type instruction.
    #[derive(Clone, Copy, Debug, Eq, PartialEq)]
    pub struct Instr {
        pub target: u32,
    }
}

pub mod r {
    /// An R-type instruction.
    #[derive(Clone, Copy, Debug, Eq, PartialEq)]
    pub struct Instr {
        pub rs: u8,
        pub rt: u8,
        pub rd: u8,
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
    slots: [Option<State>; Self::SLOT_COUNT],
    needle: usize,
}

impl fmt::Display for Pipeline {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        const NONE: &str = "(none)";

        writeln!(
            f,
            "1. {}",
            self.rd.as_ref().map_or_else(
                || String::from(NONE),
                |instr| instr.asm().to_string(),
            ),
        )?;

        for slot in 1..=Self::SLOT_COUNT {
            let idx = Self::needle_plus(self.needle, slot);
            writeln!(
                f,
                "{}. {}",
                slot + 1,
                self.slots[idx].as_ref().map_or_else(
                    || String::from(NONE),
                    |state| state.to_string(),
                ),
            )?;
        }

        Ok(())
    }
}

impl Pipeline {
    /// The number of instruction state slots.
    const SLOT_COUNT: usize = 3;

    pub fn advance(
        &mut self,
        reg: &mut reg::File,
        fetch_instr: impl FnOnce() -> Instr,
    ) {
        let needle = self.needle;
        let slots = self.slots.as_mut_ptr();

        let rd = &mut self.rd;

        // SAFETY: These exclusive borrows are not aliasing, but Rust doesn't know that.
        let alu = unsafe { &mut *slots.add(Self::needle_plus(needle, 2)) };
        let mem = unsafe { &mut *slots.add(Self::needle_plus(needle, 1)) };
        let wb = unsafe { &mut *slots.add(needle) };

        let mut should_fetch = true;

        if let Some(mut state) = wb.take() {
            state.write_back(reg);
        }
        if let Some(mut state) = mem.take() {
            state.access_mem();

            if state.will_jump {
                // The instruction represented by `state` prepares to jump; it is expected to do so
                // in the write-back stage. However, as this is the third stage, there is already
                // one instruction queued after this jump, and another will be queued shortly. MIPS
                // I specifies that we must allow the first of these two instructions, and only the
                // first, to execute fully before jumping. We can do this by simply never fetching
                // the second.
                //
                // TODO: This may not be exactly specification-compilant. In particular, this leaves
                // a gap in the pipeline, which is inefficient and is probably not the correct
                // behavior. Probably, MIPS immediately fetches from the new target.
                should_fetch = false;
            }

            *wb = Some(state);
        }
        if let Some(mut state) = alu.take() {
            state.operate();
            *mem = Some(state);
        }
        if let Some(instr) = rd.take() {
            *alu = Some(State::read(instr, reg));
        }
        if should_fetch {
            *rd = Some(fetch_instr());
        }

        self.advance_needle();
    }

    fn needle_plus(needle: usize, index: usize) -> usize {
        (needle + index) % Self::SLOT_COUNT
    }

    fn advance_needle(&mut self) {
        self.needle = Self::needle_plus(self.needle, 1);
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
                ]
                $(
                    , operate: $alu_fn:expr
                )?
                $(
                    , access_mem: $mem_fn:expr
                )?
                $(
                    , write_back: $wb_fn:expr
                )? $(,)?
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

        impl State {
            pub fn read(instr: Instr, reg: &reg::File) -> Self {
                Self {
                    opx: opx::State::read(instr, reg),
                    exc: ConstQueue::new(),
                    will_jump: false,
                }
            }
        }

        pub struct State {
            opx: opx::State,
            exc: ExcQueue,
            will_jump: bool,
        }

        impl fmt::Display for State {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "{}", self.opx)?;

                if !self.exc.empty() {
                    write!(
                        f,
                        " ![]",
                        // TODO: List exceptions in brackets.
                    )?;
                }

                Ok(())
            }
        }

        impl State {
            pub fn operate(&mut self) {
                match self.opx {
                    $(
                        #[allow(unused_variables)]
                        opx::State::$variant_name(ref mut opx) => {
                            $(
                                $alu_fn(&mut self.exc, opx);
                            )?
                        }
                    )*
                }
            }

            pub fn access_mem(&mut self) {
                match self.opx {
                    $(
                        #[allow(unused_assignments, unused_mut, unused_variables)]
                        opx::State::$variant_name(ref mut opx) => {
                            $(
                                $mem_fn(&mut self.exc, opx, &mut self.will_jump);
                            )?
                        }
                    )*
                }
            }

            pub fn write_back(&mut self, reg: &mut reg::File) {
                match self.opx {
                    $(
                        opx::State::$variant_name(ref mut opx) => {
                            opx.write(reg);
                            $(
                                $wb_fn(&mut self.exc, opx, reg);
                            )?
                        }
                    )*
                }
            }
        }

        pub mod opx {
            //! Operations.

            use std::fmt;

            use super::{Instr, reg, opn::Gpr};

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
                                Self::$variant_name($variant_name::read(reg, it))
                            }
                        )*
                    }
                }
            }

            pub enum State {
                $(
                    $variant_name($variant_name),
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

            $(
                gen_opx_state!($ty, $variant_name);
            )*
        }

        pub mod opn {
            //! Instruction operands.

            use super::reg;

            impl Gpr {
                pub fn read(reg: &reg::File, index: usize) -> Self {
                    Self {
                        index: index,
                        value: reg.gpr(index),
                        pending: None,
                    }
                }
            }

            /// A general-purpose register (GPR) operand.
            #[derive(Clone, Copy, Debug, Eq, PartialEq)]
            pub struct Gpr {
                /// The zero-based index of this GPR.
                ///
                /// An index of 0 represents `r1` while an index of 30 represents `r31`.
                index: usize,
                value: u32,
                /// The value, if any, to be written to this GPR.
                pending: Option<u32>,
            }

            impl Gpr {
                /// The current value of this GPR.
                pub fn value(&self) -> u32 {
                    self.value
                }

                pub fn update(&mut self, value: u32) {
                    self.pending = Some(value);
                }

                pub fn write(&self, reg: &mut reg::File) {
                    if let Some(pending) = self.pending {
                        reg.set_gpr(self.index, pending);
                    }
                }
            }
        }
    };
}

macro_rules! gen_opx_state {
    (i, $variant_name:ident) => {
        impl $variant_name {
            pub fn read(reg: &reg::File, instr: super::i::Instr) -> Self {
                Self {
                    rs: Gpr::read(reg, instr.rs.into()),
                    rt: Gpr::read(reg, instr.rt.into()),
                    imm: instr.imm,
                }
            }
        }

        pub struct $variant_name {
            pub rs: Gpr,
            pub rt: Gpr,
            pub imm: u16,
        }

        impl $variant_name {
            pub fn write(&self, reg: &mut reg::File) {
                self.rt.write(reg);
            }
        }
    };
    (j, $variant_name:ident) => {
        impl $variant_name {
            pub fn read(_: &reg::File, instr: super::j::Instr) -> Self {
                Self {
                    target: instr.target,
                }
            }
        }

        pub struct $variant_name {
            pub target: u32,
        }

        impl $variant_name {
            pub fn write(&self, _: &mut reg::File) {

            }
        }
    };
    (r, $variant_name:ident) => {
        impl $variant_name {
            pub fn read(reg: &reg::File, instr: super::r::Instr) -> Self {
                Self {
                    rs: Gpr::read(reg, instr.rs.into()),
                    rt: Gpr::read(reg, instr.rt.into()),
                    rd: Gpr::read(reg, instr.rd.into()),
                    shamt: instr.shamt,
                    funct: instr.funct,
                }
            }
        }

        pub struct $variant_name {
            pub rs: Gpr,
            pub rt: Gpr,
            pub rd: Gpr,
            pub shamt: u8,
            pub funct: u8,
        }

        impl $variant_name {
            pub fn write(&self, reg: &mut reg::File) {
                self.rd.write(reg);
            }
        }
    };
}

fn sign_extend(value: u16) -> u32 {
    ((value as i16) as i32) as u32
}

def_instr_and_op_kind!(
    {
        name: Add,
        type: r,
        asm: ["add" %(rd), %(rs), %(rt)],
        operate: |exc: &mut ExcQueue, opx: &mut opx::Add| {
            let (result, overflowed) = opx.rs.value().overflowing_add(opx.rt.value());
            if overflowed {
                exc.push(ExceptionKind::IntegerOverflow).unwrap();
            } else {
                // `rd` is only modified when an exception doesn't occur.
                opx.rd.update(result);
            }
        },
    },
    {
        name: Addi,
        type: i,
        asm: ["addi" %(rt), %(rs), #(s)],
        operate: |exc: &mut ExcQueue, opx: &mut opx::Addi| {
            let (result, overflowed) = opx.rs.value().overflowing_add(sign_extend(opx.imm));
            if overflowed {
                exc.push(ExceptionKind::IntegerOverflow).unwrap();
            } else {
                // `rt` is only modified when an exception doesn't occur.
                opx.rt.update(result);
            }
        },
    },
    {
        name: Addiu,
        type: i,
        asm: ["addiu" %(rt), %(rs), #(s)],
        operate: |_, opx: &mut opx::Addiu| {
            // This operation is unsigned, so no need to test for overflow.
            opx.rt.update(opx.rs.value().wrapping_add(sign_extend(opx.imm)));
        },
    },
    {
        name: Addu,
        type: r,
        asm: ["addu" %(rd), %(rs), %(rt)],
        operate: |_, opx: &mut opx::Addu| {
            // This operation is unsigned, so no need to test for overflow.
            opx.rd.update(opx.rs.value().wrapping_add(opx.rt.value()));
        },
    },
    {
        name: And,
        type: r,
        asm: ["and" %(rd), %(rs), %(rt)],
        operate: |_, opx: &mut opx::And| {
            opx.rd.update(opx.rs.value() & opx.rt.value());
        },
    },
    {
        name: Andi,
        type: i,
        asm: ["andi" %(rt), %(rs), #(s)],
        operate: |_, opx: &mut opx::Andi| {
            opx.rt.update(opx.rs.value() & u32::from(opx.imm));
        },
    },
    {
        name: BCond,
        type: i,
        asm: ["bcond" %(rs), #(s)],
    },
    {
        name: Beq,
        type: i,
        asm: ["beq" %(rs), %(rt), #(s)],
    },
    {
        name: Bgtz,
        type: i,
        asm: ["bgtz" %(rs), #(s)],
    },
    {
        name: Blez,
        type: i,
        asm: ["blez" %(rs), #(s)],
    },
    {
        name: Bne,
        type: i,
        asm: ["bne" %(rs), %(rt), #(s)],
        operate: |_, opx: &mut opx::Bne| {

        },
        access_mem: |_, opx: &mut opx::Bne, will_jump: &mut bool| {
            *will_jump = opx.rs.value() != opx.rt.value();
        },
    },
    {
        name: Break,
        type: i,
        asm: ["break"],
        operate: |exc: &mut ExcQueue, _| {
            exc.push(ExceptionKind::Breakpoint).unwrap();
        },
    },
    {
        name: Cop0,
        type: i,
        asm: ["cop0"],
    },
    {
        name: Cop1,
        type: i,
        asm: ["cop1"],
    },
    {
        name: Cop2,
        type: i,
        asm: ["cop2"],
    },
    {
        name: Cop3,
        type: i,
        asm: ["cop3"],
    },
    {
        name: Div,
        type: r,
        asm: ["div"  %(rs), %(rt)],
        write_back: |_, opx: &opx::Div, reg: &mut reg::File| {
            // Note: the ALU operations should technically be performed in the ALU stage, but it's
            // OK to do it here as there are no programmer-visible effects (e.g., exceptions) in
            // doing so.

            // Note: MIPS I specifies that division by 0 is undefined, but to be safe, we'll
            // hardcode it to the fairly-reasonable value of 0.
            *reg.lo_mut() = opx.rs.value().checked_div(opx.rt.value()).unwrap_or(0);
            *reg.hi_mut() = opx.rs.value() % opx.rt.value();
        },
    },
    {
        name: Divu,
        type: r,
        asm: ["divu"  %(rs), %(rt)],
        write_back: |_, opx: &opx::Divu, reg: &mut reg::File| {
            // See `Div`.
            *reg.lo_mut() = opx.rs.value().checked_div(opx.rt.value()).unwrap_or(0);
            *reg.hi_mut() = opx.rs.value() % opx.rt.value();
        },
    },
    {
        name: J,
        type: j,
        asm: ["j" *()],
        access_mem: |_, _, will_jump: &mut bool| {
            *will_jump = true;
        },
        write_back: |_, opx: &mut opx::J, reg: &mut reg::File| {
            *reg.pc_mut() = opx.target;
        },
    },
    {
        name: Jal,
        type: j,
        asm: ["jal" *()],
        access_mem: |_, _, will_jump: &mut bool| {
            *will_jump = true;
        },
        write_back: |_, opx: &mut opx::Jal, reg: &mut reg::File| {
            reg.set_gpr(31, opx.target);
        },
    },
    {
        name: Jalr,
        type: r,
        asm: ["jalr" %(rs), %(rd)],
        access_mem: |_, _, will_jump: &mut bool| {
            *will_jump = true;
        },
        write_back: |_, opx: &mut opx::Jalr, reg: &mut reg::File| {
            reg.set_gpr(31, opx.rs.value());
        },
    },
    {
        name: Jr,
        type: r,
        asm: ["jr" %(rs)],
        access_mem: |_, _, will_jump: &mut bool| {
            *will_jump = true;
        },
        write_back: |_, opx: &mut opx::Jr, reg: &mut reg::File| {
            *reg.pc_mut() = opx.rs.value();
        },
    },
    {
        name: Lb,
        type: i,
        asm: ["lb" %(rt), #(s)],
    },
    {
        name: Lbu,
        type: i,
        asm: ["lbu" %(rt), #(s)],
    },
    {
        name: Lh,
        type: i,
        asm: ["lh" %(rt), #(s)],
    },
    {
        name: Lhu,
        type: i,
        asm: ["lhu" %(rt), #(s)],
    },
    {
        name: Lui,
        type: i,
        asm: ["lui" %(rt), #(s)],
    },
    {
        name: Lw,
        type: i,
        asm: ["lw" %(rt), #(s)],
    },
    {
        name: Lwc0,
        type: i,
        asm: ["lwc0" %(rt), #(s)],
    },
    {
        name: Lwc1,
        type: i,
        asm: ["lwc1" %(rt), #(s)],
    },
    {
        name: Lwc2,
        type: i,
        asm: ["lwc2" %(rt), #(s)],
    },
    {
        name: Lwc3,
        type: i,
        asm: ["lwc3" %(rt), #(s)],
    },
    {
        name: Lwl,
        type: i,
        asm: ["lwl" %(rt), #(s)],
    },
    {
        name: Lwr,
        type: i,
        asm: ["lwr" %(rt), #(s)],
    },
    {
        name: Mfhi,
        type: r,
        asm: ["mfhi" %(rd)],
    },
    {
        name: Mflo,
        type: r,
        asm: ["mflo" %(rd)],
    },
    {
        name: Mthi,
        type: r,
        asm: ["mthi" %(rd)],
    },
    {
        name: Mtlo,
        type: r,
        asm: ["mtlo" %(rd)],
    },
    {
        name: Mult,
        type: r,
        asm: ["mult" %(rs), %(rt)],
    },
    {
        name: Multu,
        type: r,
        asm: ["multu"  %(rs), %(rt)],
    },
    {
        name: Nor,
        type: r,
        asm: ["nor" %(rd), %(rs), %(rt)],
        operate: |_, opx: &mut opx::Nor| {
            opx.rd.update(!(opx.rs.value() | opx.rt.value()));
        },
    },
    {
        name: Or,
        type: r,
        asm: ["or" %(rd), %(rs), %(rt)],
        operate: |_, opx: &mut opx::Or| {
            opx.rd.update(opx.rs.value() | opx.rt.value());
        },
    },
    {
        name: Ori,
        type: i,
        asm: ["ori" %(rt), %(rs), #(s)],
        operate: |_, opx: &mut opx::Ori| {
            opx.rt.update(opx.rs.value() | u32::from(opx.imm));
        },
    },
    {
        name: Sb,
        type: i,
        asm: ["sb" %(rt), #(s)],
    },
    {
        name: Sh,
        type: i,
        asm: ["sh" %(rt), #(s)],
    },
    {
        name: Sll,
        type: r,
        asm: ["sll" %(rd), %(rt), ^()],
        operate: |_, opx: &mut opx::Sll| {
            opx.rd.update(opx.rt.value() << opx.shamt);
        },
    },
    {
        name: Sllv,
        type: r,
        asm: ["sllv" %(rd), %(rt), %(rs)],
        operate: |_, opx: &mut opx::Sllv| {
            opx.rd.update(opx.rt.value() << (opx.rs.value() & 0b11111));
        },
    },
    {
        name: Slt,
        type: r,
        asm: ["slt" %(rd), %(rs), %(rt)],
        operate: |_, opx: &mut opx::Slt| {
            opx.rd.update(((opx.rs.value() as i32) < (opx.rt.value() as i32)) as u32);
        },
    },
    {
        name: Slti,
        type: i,
        asm: ["slti" %(rt), %(rs), #(s)],
        operate: |_, opx: &mut opx::Slti| {
            opx.rt.update(((opx.rs.value() as i32) < (sign_extend(opx.imm) as i32)) as u32);
        },
    },
    {
        name: Sltiu,
        type: i,
        asm: ["sltiu" %(rt), %(rs), #(s)],
        operate: |_, opx: &mut opx::Sltiu| {
            opx.rt.update((opx.rs.value() < sign_extend(opx.imm)) as u32);
        },
    },
    {
        name: Sltu,
        type: r,
        asm: ["sltu" %(rd), %(rs), %(rt)],
        operate: |_, opx: &mut opx::Sltu| {
            opx.rd.update((opx.rs.value() < opx.rt.value()) as u32);
        },
    },
    {
        name: Sra,
        type: r,
        asm: ["sra" %(rd), %(rt), ^()],
    },
    {
        name: Srav,
        type: r,
        asm: ["srav"  %(rd), %(rt), %(rs)],
    },
    {
        name: Srl,
        type: r,
        asm: ["srl" %(rd), %(rt), ^()],
        operate: |_, opx: &mut opx::Srl| {
            opx.rd.update(opx.rt.value() >> opx.shamt);
        },
    },
    {
        name: Srlv,
        type: r,
        asm: ["srlv"  %(rd), %(rt), %(rs)],
        operate: |_, opx: &mut opx::Srlv| {
            opx.rd.update(opx.rt.value() >> (opx.shamt & 0b11111));
        },
    },
    {
        name: Sub,
        type: r,
        asm: ["sub" %(rd), %(rs), %(rt)],
    },
    {
        name: Subu,
        type: r,
        asm: ["subu" %(rd), %(rs), %(rt)],
        operate: |_, opx: &mut opx::Subu| {
            opx.rd.update(opx.rs.value().wrapping_sub(opx.rt.value()));
        },
    },
    {
        name: Sw,
        type: i,
        asm: ["sw" %(rt), #(s)],
    },
    {
        name: Swc0,
        type: i,
        asm: ["swc0" %(rt), #(s)],
    },
    {
        name: Swc1,
        type: i,
        asm: ["swc1" %(rt), #(s)],
    },
    {
        name: Swc2,
        type: i,
        asm: ["swc2" %(rt), #(s)],
    },
    {
        name: Swc3,
        type: i,
        asm: ["swc3" %(rt), #(s)],
    },
    {
        name: Swl,
        type: i,
        asm: ["swl" %(rt), #(s)],
    },
    {
        name: Swr,
        type: i,
        asm: ["swr" %(rt), #(s)],
    },
    {
        name: Syscall,
        type: i,
        asm: ["syscall"],
        operate: |exc: &mut ExcQueue, _| {
            exc.push(ExceptionKind::Syscall).unwrap();
        },
    },
    {
        name: Xor,
        type: r,
        asm: ["xor" %(rd), %(rs), %(rt)],
        operate: |_, opx: &mut opx::Xor| {
            opx.rd.update(opx.rs.value() ^ opx.rt.value());
        },
    },
    {
        name: Xori,
        type: i,
        asm: ["xori" %(rt), %(rs), #(s)],
        operate: |_, opx: &mut opx::Xori| {
            opx.rt.update(opx.rs.value() ^ u32::from(opx.imm));
        },
    },
);
