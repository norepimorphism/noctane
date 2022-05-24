// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

pub mod asm;
pub mod decode;

use std::fmt;

use const_queue::ConstQueue;

pub use asm::Asm;
use crate::cpu::{ExceptionKind, reg};

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
        writeln!(
            f,
            "1. {}",
            self.rd.map_or_else(
                || String::from("(none)"),
                |state| state.asm().to_string(),
            ),
        )?;

        for slot in 1..=Self::SLOT_COUNT {
            let idx = Self::needle_plus(self.needle, slot);
            writeln!(
                f,
                "{}. {}",
                slot + 1,
                self.slots[idx].as_ref().map_or_else(
                    || String::from("(none)"),
                    |state| state.to_string(),
                ),
            )?;
        }

        Ok(())
    }
}

impl Pipeline {
    /// The number of unique pipestages.
    const STAGE_COUNT: usize = 4;
    /// The number of instruction state slots.
    const SLOT_COUNT: usize = Self::STAGE_COUNT - 1;

    pub fn advance(
        &mut self,
        reg: &mut reg::File,
        fetch_instr: impl FnOnce() -> Instr,
    ) {
        let needle = self.needle;
        let slots = self.slots.as_mut_ptr();

        let rd = &mut self.rd;
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
                type: $type:tt,
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
        pub enum OpKind {
            $(
                $variant_name,
            )*
        }

        #[derive(Clone, Copy, Debug, Eq, PartialEq)]
        pub enum Instr {
            $(
                $variant_name(concat_idents!($type, Type)),
            )*
        }

        impl Instr {
            pub fn decode(code: u32) -> Option<Self> {
                match Self::try_decode_op_kind(code)? {
                    $(
                        OpKind::$variant_name => {
                            Some(Self::$variant_name(
                                <concat_idents!($type, Type)>::decode(code)
                            ))
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

        impl State {
            pub fn read(instr: Instr, reg: &reg::File) -> Self {
                Self {
                    exc: ConstQueue::new(),
                    op: OperandState::read(instr, reg),
                    will_jump: false,
                }
            }
        }

        impl OperandState {
            fn read(instr: Instr, reg: &reg::File) -> Self {
                match instr {
                    $(
                        Instr::$variant_name(it) => {
                            Self::$variant_name(<concat_idents!($type, State)>::read(it, reg))
                        }
                    )*
                }
            }
        }

        pub struct State {
            exc: ConstQueue::<ExceptionKind, 5>,
            op: OperandState,
            will_jump: bool,
        }

        impl fmt::Display for State {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "{}", self.op)?;

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

        impl fmt::Display for OperandState {
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
            pub fn operate(&mut self) {
                match self.op {
                    $(
                        #[allow(unused_variables)]
                        OperandState::$variant_name(ref mut op) => {
                            $(
                                $alu_fn(&mut self.exc, op);
                            )?
                        }
                    )*
                }
            }

            pub fn access_mem(&mut self) {
                match self.op {
                    $(
                        #[allow(unused_assignments, unused_mut, unused_variables)]
                        OperandState::$variant_name(ref mut op) => {
                            $(
                                $mem_fn(&mut self.exc, op, &mut self.will_jump);
                            )?
                        }
                    )*
                }
            }

            pub fn write_back(&mut self, reg: &mut reg::File) {
                match self.op {
                    $(
                        OperandState::$variant_name(ref mut op) => {
                            op.write(reg);
                            $(
                                $wb_fn(&mut self.exc, op, reg);
                            )?
                        }
                    )*
                }
            }
        }

        #[derive(Clone, Copy, Debug, Eq, PartialEq)]
        pub enum OperandState {
            $(
                $variant_name(concat_idents!($type, State)),
            )*
        }
    };
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

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct IType {
    pub rs: u8,
    pub rt: u8,
    pub imm: u16,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct JType {
    pub target: u32,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct RType {
    pub rs: u8,
    pub rt: u8,
    pub rd: u8,
    pub shamt: u8,
    pub funct: u8,
}

impl IState {
    pub fn read(instr: IType, reg: &reg::File) -> Self {
        Self {
            rs: reg.gpr(instr.rs.into()),
            rt: DestReg::new(instr.rt),
            imm: instr.imm,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct IState {
    rs: u32,
    rt: DestReg,
    imm: u16,
}

impl IState {
    fn write(&self, reg: &mut reg::File) {
        self.rt.write(reg);
    }
}

impl JState {
    pub fn read(instr: JType, _: &reg::File) -> Self {
        Self { target: instr.target }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct JState {
    target: u32,
}

impl JState {
    fn write(&self, _: &mut reg::File) {}
}

impl RState {
    pub fn read(instr: RType, reg: &reg::File) -> Self {
        Self {
            rs: reg.gpr(instr.rs.into()),
            rt: reg.gpr(instr.rt.into()),
            rd: DestReg::new(instr.rd),
            shamt: instr.shamt,
            funct: instr.funct,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct RState {
    rs: u32,
    rt: u32,
    rd: DestReg,
    shamt: u8,
    funct: u8,
}

impl RState {
    fn write(&self, reg: &mut reg::File) {
        self.rd.write(reg);
    }
}

impl DestReg {
    fn new(index: u8) -> Self {
        Self { index: index.into(), pending: None }
    }
}

/// A general-purpose register (GPR) that serves as an operand destination.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct DestReg {
    /// The zero-based index of this GPR (0 -> `r1`).
    index: usize,
    /// The value, if any, to be written to this GPR.
    pending: Option<u32>,
}

impl DestReg {
    fn update(&mut self, value: u32) {
        self.pending = Some(value);
    }

    fn write(&self, reg: &mut reg::File) {
        if let Some(value) = self.pending {
            reg.set_gpr(self.index, value);
        }
    }
}

fn sign_extend(value: u16) -> u32 {
    ((value as i16) as i32) as u32
}

def_instr_and_op_kind!(
    {
        name: Add,
        type: R,
        asm: ["add" %(rd), %(rs), %(rt)],
        operate: |exc: &mut ConstQueue<ExceptionKind, 5>, op: &mut RState| {
            let (result, overflowed) = op.rs.overflowing_add(op.rt);
            if overflowed {
                exc.push(ExceptionKind::IntegerOverflow).unwrap();
            } else {
                // `rd` is only modified when an exception doesn't occur.
                op.rd.update(result);
            }
        },
    },
    {
        name: Addi,
        type: I,
        asm: ["addi" %(rt), %(rs), #(s)],
        operate: |exc: &mut ConstQueue<ExceptionKind, 5>, op: &mut IState| {
            let (result, overflowed) = op.rs.overflowing_add(sign_extend(op.imm));
            if overflowed {
                exc.push(ExceptionKind::IntegerOverflow).unwrap();
            } else {
                // `rt` is only modified when an exception doesn't occur.
                op.rt.update(result);
            }
        },
    },
    {
        name: Addiu,
        type: I,
        asm: ["addiu" %(rt), %(rs), #(s)],
        operate: |_, op: &mut IState| {
            // This operation is unsigned, so no need to test for overflow.
            op.rt.update(op.rs.wrapping_add(sign_extend(op.imm)));
        },
    },
    {
        name: Addu,
        type: R,
        asm: ["addu" %(rd), %(rs), %(rt)],
        operate: |_, op: &mut RState| {
            // This operation is unsigned, so no need to test for overflow.
            op.rd.update(op.rs.wrapping_add(op.rt));
        },
    },
    {
        name: And,
        type: R,
        asm: ["and" %(rd), %(rs), %(rt)],
        operate: |_, op: &mut RState| {
            op.rd.update(op.rs & op.rt);
        },
    },
    {
        name: Andi,
        type: I,
        asm: ["andi" %(rt), %(rs), #(s)],
        operate: |_, op: &mut IState| {
            op.rt.update(op.rs & u32::from(op.imm));
        },
    },
    {
        name: BCond,
        type: I,
        asm: ["bcond" %(rs), #(s)],
    },
    {
        name: Beq,
        type: I,
        asm: ["beq" %(rs), %(rt), #(s)],
    },
    {
        name: Bgtz,
        type: I,
        asm: ["bgtz" %(rs), #(s)],
    },
    {
        name: Blez,
        type: I,
        asm: ["blez" %(rs), #(s)],
    },
    {
        name: Bne,
        type: I,
        asm: ["bne" %(rs), %(rt), #(s)],
    },
    {
        name: Break,
        type: I,
        asm: ["break"],
        operate: |exc: &mut ConstQueue<ExceptionKind, 5>, _| {
            exc.push(ExceptionKind::Breakpoint).unwrap();
        },
    },
    {
        name: Cop0,
        type: I,
        asm: ["cop0"],
    },
    {
        name: Cop1,
        type: I,
        asm: ["cop1"],
    },
    {
        name: Cop2,
        type: I,
        asm: ["cop2"],
    },
    {
        name: Cop3,
        type: I,
        asm: ["cop3"],
    },
    {
        name: Div,
        type: R,
        asm: ["div"  %(rs), %(rt)],
        write_back: |_, op: &RState, reg: &mut reg::File| {
            // Note: the ALU operations should technically be performed in the ALU stage, but it's
            // OK to do it here as there are no programmer-visible effects (e.g., exceptions) in
            // doing so.

            // Note: MIPS I specifies that division by 0 is undefined, but to be safe, we'll
            // hardcode it to the fairly-reasonable value of 0.
            *reg.lo_mut() = op.rs.checked_div(op.rt).unwrap_or(0);
            *reg.hi_mut() = op.rs % op.rt;
        },
    },
    {
        name: Divu,
        type: R,
        asm: ["divu"  %(rs), %(rt)],
        write_back: |_, op: &RState, reg: &mut reg::File| {
            // See `Div`.
            *reg.lo_mut() = op.rs.checked_div(op.rt).unwrap_or(0);
            *reg.hi_mut() = op.rs % op.rt;
        },
    },
    {
        name: J,
        type: J,
        asm: ["j" *()],
        access_mem: |_, _, will_jump: &mut bool| {
            *will_jump = true;
        },
        write_back: |_, op: &mut JState, reg: &mut reg::File| {
            *reg.pc_mut() = op.target;
        },
    },
    {
        name: Jal,
        type: J,
        asm: ["jal" *()],
    },
    {
        name: Jalr,
        type: R,
        asm: ["jalr" %(rs), %(rd)],
    },
    {
        name: Jr,
        type: R,
        asm: ["jr" %(rs)],
        access_mem: |_, _, will_jump: &mut bool| {
            *will_jump = true;
        },
        write_back: |_, op: &mut RState, reg: &mut reg::File| {
            *reg.pc_mut() = op.rs;
        },
    },
    {
        name: Lb,
        type: I,
        asm: ["lb" %(rt), #(s)],
    },
    {
        name: Lbu,
        type: I,
        asm: ["lbu" %(rt), #(s)],
    },
    {
        name: Lh,
        type: I,
        asm: ["lh" %(rt), #(s)],
    },
    {
        name: Lhu,
        type: I,
        asm: ["lhu" %(rt), #(s)],
    },
    {
        name: Lui,
        type: I,
        asm: ["lui" %(rt), #(s)],
    },
    {
        name: Lw,
        type: I,
        asm: ["lw" %(rt), #(s)],
    },
    {
        name: Lwc0,
        type: I,
        asm: ["lwc0" %(rt), #(s)],
    },
    {
        name: Lwc1,
        type: I,
        asm: ["lwc1" %(rt), #(s)],
    },
    {
        name: Lwc2,
        type: I,
        asm: ["lwc2" %(rt), #(s)],
    },
    {
        name: Lwc3,
        type: I,
        asm: ["lwc3" %(rt), #(s)],
    },
    {
        name: Lwl,
        type: I,
        asm: ["lwl" %(rt), #(s)],
    },
    {
        name: Lwr,
        type: I,
        asm: ["lwr" %(rt), #(s)],
    },
    {
        name: Mfhi,
        type: R,
        asm: ["mfhi" %(rd)],
    },
    {
        name: Mflo,
        type: R,
        asm: ["mflo" %(rd)],
    },
    {
        name: Mthi,
        type: R,
        asm: ["mthi" %(rd)],
    },
    {
        name: Mtlo,
        type: R,
        asm: ["mtlo" %(rd)],
    },
    {
        name: Mult,
        type: R,
        asm: ["mult" %(rs), %(rt)],
    },
    {
        name: Multu,
        type: R,
        asm: ["multu"  %(rs), %(rt)],
    },
    {
        name: Nor,
        type: R,
        asm: ["nor" %(rd), %(rs), %(rt)],
        operate: |_, op: &mut RState| {
            op.rd.update(!(op.rs | op.rt));
        },
    },
    {
        name: Or,
        type: R,
        asm: ["or" %(rd), %(rs), %(rt)],
        operate: |_, op: &mut RState| {
            op.rd.update(op.rs | op.rt);
        },
    },
    {
        name: Ori,
        type: I,
        asm: ["ori" %(rt), %(rs), #(s)],
        operate: |_, op: &mut IState| {
            op.rt.update(op.rs | u32::from(op.imm));
        },
    },
    {
        name: Sb,
        type: I,
        asm: ["sb" %(rt), #(s)],
    },
    {
        name: Sh,
        type: I,
        asm: ["sh" %(rt), #(s)],
    },
    {
        name: Sll,
        type: R,
        asm: ["sll" %(rd), %(rt), ^()],
        operate: |_, op: &mut RState| {
            op.rd.update(op.rt << op.shamt);
        },
    },
    {
        name: Sllv,
        type: R,
        asm: ["sllv" %(rd), %(rt), %(rs)],
        operate: |_, op: &mut RState| {
            op.rd.update(op.rt << (op.rs & 0b11111));
        },
    },
    {
        name: Slt,
        type: R,
        asm: ["slt" %(rd), %(rs), %(rt)],
        operate: |_, op: &mut RState| {
            op.rd.update(((op.rs as i32) < (op.rt as i32)) as u32);
        },
    },
    {
        name: Slti,
        type: I,
        asm: ["slti" %(rt), %(rs), #(s)],
        operate: |_, op: &mut IState| {
            op.rt.update(((op.rs as i32) < (sign_extend(op.imm) as i32)) as u32);
        },
    },
    {
        name: Sltiu,
        type: I,
        asm: ["sltiu" %(rt), %(rs), #(s)],
        operate: |_, op: &mut IState| {
            op.rt.update((op.rs < sign_extend(op.imm)) as u32);
        },
    },
    {
        name: Sltu,
        type: R,
        asm: ["sltu" %(rd), %(rs), %(rt)],
        operate: |_, op: &mut RState| {
            op.rd.update((op.rs < op.rt) as u32);
        },
    },
    {
        name: Sra,
        type: R,
        asm: ["sra" %(rd), %(rt), ^()],
    },
    {
        name: Srav,
        type: R,
        asm: ["srav"  %(rd), %(rt), %(rs)],
    },
    {
        name: Srl,
        type: R,
        asm: ["srl" %(rd), %(rt), ^()],
        operate: |_, op: &mut RState| {
            op.rd.update(op.rt >> op.shamt);
        },
    },
    {
        name: Srlv,
        type: R,
        asm: ["srlv"  %(rd), %(rt), %(rs)],
        operate: |_, op: &mut RState| {
            op.rd.update(op.rt >> (op.shamt & 0b11111));
        },
    },
    {
        name: Sub,
        type: R,
        asm: ["sub" %(rd), %(rs), %(rt)],
    },
    {
        name: Subu,
        type: R,
        asm: ["subu" %(rd), %(rs), %(rt)],
        operate: |_, op: &mut RState| {
            op.rd.update(op.rs.wrapping_sub(op.rt));
        },
    },
    {
        name: Sw,
        type: I,
        asm: ["sw" %(rt), #(s)],
    },
    {
        name: Swc0,
        type: I,
        asm: ["swc0" %(rt), #(s)],
    },
    {
        name: Swc1,
        type: I,
        asm: ["swc1" %(rt), #(s)],
    },
    {
        name: Swc2,
        type: I,
        asm: ["swc2" %(rt), #(s)],
    },
    {
        name: Swc3,
        type: I,
        asm: ["swc3" %(rt), #(s)],
    },
    {
        name: Swl,
        type: I,
        asm: ["swl" %(rt), #(s)],
    },
    {
        name: Swr,
        type: I,
        asm: ["swr" %(rt), #(s)],
    },
    {
        name: Syscall,
        type: I,
        asm: ["syscall"],
        operate: |exc: &mut ConstQueue<ExceptionKind, 5>, _| {
            exc.push(ExceptionKind::Syscall).unwrap();
        },
    },
    {
        name: Xor,
        type: R,
        asm: ["xor" %(rd), %(rs), %(rt)],
        operate: |_, op: &mut RState| {
            op.rd.update(op.rs ^ op.rt);
        },
    },
    {
        name: Xori,
        type: I,
        asm: ["xori" %(rt), %(rs), #(s)],
        operate: |_, op: &mut IState| {
            op.rt.update(op.rs ^ u32::from(op.imm));
        },
    },
);
