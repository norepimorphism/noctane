// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

pub mod asm;
pub mod decode;

pub use asm::Asm;
use crate::cpu::reg;

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
#[derive(Debug)]
pub struct Pipeline {
    pub rd: Option<Instr>,
    pub alu: Option<State>,
    pub mem: Option<State>,
    pub wb: Option<State>,
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
                ],
                $alu_fn:expr,
                $mem_fn:expr,
                $wb_fn:expr $(,)?
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
                        // TODO
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
            pub fn read(instr: Instr, reg: &reg::File) -> State {
                match instr {
                    $(
                        Instr::$variant_name(it) => {
                            State::$variant_name(<concat_idents!($type, State)>::read(it, reg))
                        }
                    )*
                }
            }
        }

        #[derive(Clone, Copy, Debug, Eq, PartialEq)]
        pub enum State {
            $(
                $variant_name(concat_idents!($type, State)),
            )*
        }

        impl State {
            pub fn operate(&mut self) {
                match self {
                    $(
                        Self::$variant_name(it) => $alu_fn(it),
                    )*
                }
            }

            pub fn access_mem(&mut self) {
                match self {
                    $(
                        Self::$variant_name(it) => $mem_fn(it),
                    )*
                }
            }

            pub fn write_back(&mut self, reg: &mut reg::File) {
                match self {
                    $(
                        Self::$variant_name(it) => $wb_fn(it, reg),
                    )*
                }
            }
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

impl JState {
    pub fn read(instr: JType, _: &reg::File) -> Self {
        Self { target: instr.target }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct JState {
    target: u32,
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

impl DestReg {
    fn new(index: u8) -> Self {
        Self { index: index.into(), pending: 0 }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct DestReg {
    index: usize,
    pending: u32,
}

impl DestReg {
    fn update(&mut self, value: u32) {
        self.pending = value;
    }

    fn write(&self, reg: &mut reg::File) {
        reg.set_gpr(self.index, self.pending);
    }
}

def_instr_and_op_kind!(
    {
        name: Add,
        type: R,
        asm: ["add" %(rd), %(rs), %(rt)],
        |state: &mut RState| {
            let (result, overflowed) = state.rs.overflowing_add(state.rt);
            state.rd.update(result);

            if overflowed {
                // TODO: Generate an exception.
            }
        },
        |_| {},
        |state: &mut RState, reg| {
            state.rd.write(reg);
        },
    },
    {
        name: Addi,
        type: I,
        asm: ["addi" %(rt), %(rs), #(s)],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Addiu,
        type: I,
        asm: ["addiu" %(rt), %(rs), #(s)],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Addu,
        type: R,
        asm: ["addu" %(rd), %(rs), %(rt)],
        |state: &mut RState| {
            // This operation is unsigned, so no need to test for overflow.
            state.rd.update(state.rs.wrapping_add(state.rt));
        },
        |_| {},
        |state: &mut RState, reg| {
            state.rd.write(reg);
        },
    },
    {
        name: And,
        type: R,
        asm: ["and" %(rd), %(rs), %(rt)],
        |state: &mut RState| {
            state.rd.update(state.rs & state.rt);
        },
        |_| {},
        |state: &mut RState, reg| {
            state.rd.write(reg);
        },
    },
    {
        name: Andi,
        type: I,
        asm: ["andi" %(rt), %(rs), #(s)],
        |state: &mut IState| {
            state.rt.update(state.rs & u32::from(state.imm));
        },
        |_| {},
        |state: &mut IState, reg| {
            state.rt.write(reg);
        },
    },
    {
        name: BCond,
        type: I,
        asm: ["bcond" %(rs), #(s)],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Beq,
        type: I,
        asm: ["beq" %(rs), %(rt), #(s)],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Bgtz,
        type: I,
        asm: ["bgtz" %(rs), #(s)],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Blez,
        type: I,
        asm: ["blez" %(rs), #(s)],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Bne,
        type: I,
        asm: ["bne" %(rs), %(rt), #(s)],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Break,
        type: I,
        asm: ["break"],
        |_| {
            // TODO: Generate a breakpoint exception.
        },
        |_| {},
        |_, _| {},
    },
    {
        name: Cop0,
        type: I,
        asm: ["cop0"],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Cop1,
        type: I,
        asm: ["cop1"],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Cop2,
        type: I,
        asm: ["cop2"],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Cop3,
        type: I,
        asm: ["cop3"],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Div,
        type: R,
        asm: ["div"  %(rs), %(rt)],
        |_| {},
        |_| {},
        |state: &RState, reg: &mut reg::File| {
            *reg.lo_mut() = state.rs.wrapping_div(state.rt);
            *reg.hi_mut() = state.rs % state.rt;
        },
    },
    {
        name: Divu,
        type: R,
        asm: ["divu"  %(rs), %(rt)],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: J,
        type: J,
        asm: ["j" *()],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Jal,
        type: J,
        asm: ["jal" *()],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Jalr,
        type: R,
        asm: ["jalr" %(rs), %(rd)],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Jr,
        type: R,
        asm: ["jr" %(rs)],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Lb,
        type: I,
        asm: ["lb" %(rt), #(s)],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Lbu,
        type: I,
        asm: ["lbu" %(rt), #(s)],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Lh,
        type: I,
        asm: ["lh" %(rt), #(s)],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Lhu,
        type: I,
        asm: ["lhu" %(rt), #(s)],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Lui,
        type: I,
        asm: ["lui" %(rt), #(s)],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Lw,
        type: I,
        asm: ["lw" %(rt), #(s)],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Lwc0,
        type: I,
        asm: ["lwc0" %(rt), #(s)],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Lwc1,
        type: I,
        asm: ["lwc1" %(rt), #(s)],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Lwc2,
        type: I,
        asm: ["lwc2" %(rt), #(s)],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Lwc3,
        type: I,
        asm: ["lwc3" %(rt), #(s)],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Lwl,
        type: I,
        asm: ["lwl" %(rt), #(s)],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Lwr,
        type: I,
        asm: ["lwr" %(rt), #(s)],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Mfhi,
        type: R,
        asm: ["mfhi" %(rd)],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Mflo,
        type: R,
        asm: ["mflo" %(rd)],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Mthi,
        type: R,
        asm: ["mthi" %(rd)],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Mtlo,
        type: R,
        asm: ["mtlo" %(rd)],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Mult,
        type: R,
        asm: ["mult" %(rs), %(rt)],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Multu,
        type: R,
        asm: ["multu"  %(rs), %(rt)],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Nor,
        type: R,
        asm: ["nor" %(rd), %(rs), %(rt)],
        |state: &mut RState| {
            state.rd.update(!(state.rs | state.rt));
        },
        |_| {},
        |state: &mut RState, reg| {
            state.rd.write(reg);
        },
    },
    {
        name: Or,
        type: R,
        asm: ["or" %(rd), %(rs), %(rt)],
        |state: &mut RState| {
            state.rd.update(state.rs | state.rt);
        },
        |_| {},
        |state: &mut RState, reg| {
            state.rd.write(reg);
        },
    },
    {
        name: Ori,
        type: I,
        asm: ["ori" %(rt), %(rs), #(s)],
        |state: &mut IState| {
            state.rt.update(state.rs | u32::from(state.imm));
        },
        |_| {},
        |state: &mut IState, reg| {
            state.rt.write(reg);
        },
    },
    {
        name: Sb,
        type: I,
        asm: ["sb" %(rt), #(s)],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Sh,
        type: I,
        asm: ["sh" %(rt), #(s)],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Sll,
        type: R,
        asm: ["sll" %(rd), %(rt), ^()],
        |state| {
            // TODO
        },
        |_| {},
        |state, _| {
            // TODO
        },
    },
    {
        name: Sllv,
        type: R,
        asm: ["sllv" %(rd), %(rt), %(rs)],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Slt,
        type: R,
        asm: ["slt" %(rd), %(rs), %(rt)],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Slti,
        type: I,
        asm: ["slti" %(rt), %(rs), #(s)],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Sltiu,
        type: I,
        asm: ["sltiu" %(rt), %(rs), #(s)],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Sltu,
        type: R,
        asm: ["sltu" %(rd), %(rs), %(rt)],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Sra,
        type: R,
        asm: ["sra" %(rd), %(rt), ^()],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Srav,
        type: R,
        asm: ["srav"  %(rd), %(rt), %(rs)],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Srl,
        type: R,
        asm: ["srl" %(rd), %(rt), ^()],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Srlv,
        type: R,
        asm: ["srlv"  %(rd), %(rt), %(rs)],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Srv,
        type: I,
        asm: ["srv"],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Sub,
        type: R,
        asm: ["sub" %(rd), %(rs), %(rt)],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Subu,
        type: R,
        asm: ["subu" %(rd), %(rs), %(rt)],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Sw,
        type: I,
        asm: ["sw" %(rt), #(s)],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Swc0,
        type: I,
        asm: ["swc0" %(rt), #(s)],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Swc1,
        type: I,
        asm: ["swc1" %(rt), #(s)],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Swc2,
        type: I,
        asm: ["swc2" %(rt), #(s)],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Swc3,
        type: I,
        asm: ["swc3" %(rt), #(s)],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Swl,
        type: I,
        asm: ["swl" %(rt), #(s)],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Swr,
        type: I,
        asm: ["swr" %(rt), #(s)],
        |state| todo!(),
        |state| todo!(),
        |state, _| todo!(),
    },
    {
        name: Syscall,
        type: I,
        asm: ["syscall"],
        |_| {
            // TODO: Generate a system call exception.
        },
        |_| {},
        |_, _| {},
    },
    {
        name: Xor,
        type: R,
        asm: ["xor" %(rd), %(rs), %(rt)],
        |state: &mut RState| {
            state.rd.update(state.rs ^ state.rt);
        },
        |_| {},
        |state: &mut RState, reg| {
            state.rd.write(reg);
        },
    },
    {
        name: Xori,
        type: I,
        asm: ["xori" %(rt), %(rs), #(s)],
        |state: &mut IState| {
            state.rt.update(state.rs ^ u32::from(state.imm));
        },
        |_| {},
        |state: &mut IState, reg| {
            state.rt.write(reg);
        },
    },
);
