// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

pub mod asm;
pub mod decode;

pub use asm::Asm;

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
    pub if_: Option<Instr>,
    pub rd: Option<rd::State>,
    pub alu: Option<alu::State>,
    pub mem: Option<mem::State>,
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
                $rd_fn:expr,
                alu: {
                    $($alu_field_name:ident : $alu_field_ty:ty),* $(,)?
                },
                $alu_fn:expr,
                mem: {
                    $($mem_field_name:ident : $mem_field_ty:ty),* $(,)?
                },
                $mem_fn:expr $(,)?
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

        impl crate::Instr {
            pub fn read_reg(self, reg: &crate::cpu::reg::File) -> rd::State {
                match self {
                    $(
                        Self::$variant_name(it) => {
                            rd::State::$variant_name(rd::$variant_name { ops: it.read_reg(reg)})
                        }
                    )*
                }
            }
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

        impl IType {
            pub fn read_reg(self, reg: &crate::cpu::reg::File) -> rd::IState {
                rd::IState {
                    rs_val: reg.gpr(self.rs.into()),
                    rt_val: reg.gpr(self.rt.into()),
                    imm: self.imm,
                }
            }
        }

        impl JType {
            pub fn read_reg(self, _: &crate::cpu::reg::File) -> rd::JState {
                rd::JState { target: self.target }
            }
        }

        impl RType {
            pub fn read_reg(self, reg: &crate::cpu::reg::File) -> rd::RState {
                rd::RState {
                    rs_val: reg.gpr(self.rs.into()),
                    rt_val: reg.gpr(self.rt.into()),
                    rd: self.rd,
                    shamt: self.shamt,
                    funct: self.funct,
                }
            }
        }

        pub mod rd {
            _impl_stage_mod! {
                fn: operate(()) -> alu,
                instrs: [
                    $(
                        {
                            $variant_name: $type,
                            fn: $rd_fn,
                            fields: [],
                        },
                    )*
                ],
            }

            #[derive(Clone, Copy, Debug, Eq, PartialEq)]
            pub struct IState {
                pub rs_val: u32,
                pub rt_val: u32,
                pub imm: u16,
            }

            #[derive(Clone, Copy, Debug, Eq, PartialEq)]
            pub struct JState {
                pub target: u32,
            }

            #[derive(Clone, Copy, Debug, Eq, PartialEq)]
            pub struct RState {
                pub rs_val: u32,
                pub rt_val: u32,
                pub rd: u8,
                pub shamt: u8,
                pub funct: u8,
            }
        }

        pub mod alu {
            use super::rd::{IState, JState, RState};

            _impl_stage_mod! {
                fn: access_mem(()) -> mem,
                instrs: [
                    $(
                        {
                            $variant_name: $type,
                            fn: $alu_fn,
                            fields: [$($alu_field_name: $alu_field_ty,)*],
                        },
                    )*
                ],
            }
        }

        pub mod mem {
            use super::rd::{IState, JState, RState};

            _impl_stage_mod! {
                fn: write_back(&mut crate::cpu::reg::File),
                instrs: [
                    $(
                        {
                            $variant_name: $type,
                            fn: $mem_fn,
                            fields: [$($mem_field_name: $mem_field_ty,)*],
                        },
                    )*
                ],
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

macro_rules! _impl_stage_mod {
    (
        $(
            fn: $fn_name:ident($ctx_ty:ty) $(-> $next_mod:ident)?,
            instrs: [
                $(
                    {
                        $name:ident : $type:tt,
                        fn: $fn:expr,
                        fields: [$($field_name:ident : $field_ty:ty),* $(,)?] $(,)?
                    }
                ),* $(,)?
            ] $(,)?
        )?
    ) => {
        #[derive(Clone, Copy, Debug, Eq, PartialEq)]
        pub enum State {
            $(
                $(
                    $name($name),
                )*
            )?
        }

        $(
            impl State {
                pub fn $fn_name(self, ctx: $ctx_ty) $(-> super::$next_mod::State)? {
                    match self {
                        $(
                            Self::$name(it) => $fn(it, ctx),
                        )*
                    }
                }
            }

            $(
                #[derive(Clone, Copy, Debug, Eq, PartialEq)]
                pub struct $name {
                    pub ops: concat_idents!($type, State),
                    $(
                        pub $field_name: $field_ty,
                    )*
                }
            )*
        )?
    };
}

macro_rules! gen_struct {
    (
        $name:ident($state:expr) -> $next_mod:ident
        $(, $field_name:ident : $field_value:expr)* $(,)?
    ) => {
        super::$next_mod::State::$name(super::$next_mod::$name {
            ops: $state.ops,
            $($field_name: $field_value,)*
        })
    };
}

def_instr_and_op_kind!(
    {
        name: Add,
        type: R,
        asm: ["add" %(rd), %(rs), %(rt)],
        |state: Add, _| {
            gen_struct! {
                Add(state) -> alu,
                // result <- [rs] + [rt]
                result: state.ops.rs_val + state.ops.rt_val,
            }
        },
        alu: { result: u32 },
        |state: Add, _| gen_struct! {
            Add(state) -> mem,
            // No-op.
            result: state.result,
        },
        mem: { result: u32 },
        |state: Add, reg: &mut crate::cpu::reg::File| {
            // [rd] <- result
            // TODO: Generate exception on overflow.
            reg.set_gpr(state.ops.rd.into(), state.result);
        },
    },
    {
        name: Addi,
        type: I,
        asm: ["addi" %(rt), %(rs), #(s)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Addiu,
        type: I,
        asm: ["addiu" %(rt), %(rs), #(s)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Addu,
        type: R,
        asm: ["addu" %(rd), %(rs), %(rt)],
        |state: Addu, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: And,
        type: R,
        asm: ["and" %(rd), %(rs), %(rt)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Andi,
        type: I,
        asm: ["andi" %(rt), %(rs), #(s)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: BCond,
        type: I,
        asm: ["bcond" %(rs), #(s)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Beq,
        type: I,
        asm: ["beq" %(rs), %(rt), #(s)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Bgtz,
        type: I,
        asm: ["bgtz" %(rs), #(s)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Blez,
        type: I,
        asm: ["blez" %(rs), #(s)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Bne,
        type: I,
        asm: ["bne" %(rs), %(rt), #(s)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Break,
        type: I,
        asm: ["break"],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Cop0,
        type: I,
        asm: ["cop0"],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Cop1,
        type: I,
        asm: ["cop1"],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Cop2,
        type: I,
        asm: ["cop2"],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Cop3,
        type: I,
        asm: ["cop3"],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Div,
        type: R,
        asm: ["div"  %(rs), %(rt)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Divu,
        type: R,
        asm: ["divu"  %(rs), %(rt)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: J,
        type: J,
        asm: ["j" *()],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Jal,
        type: J,
        asm: ["jal" *()],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Jalr,
        type: R,
        asm: ["jalr" %(rs), %(rd)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Jr,
        type: R,
        asm: ["jr" %(rs)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Lb,
        type: I,
        asm: ["lb" %(rt), #(s)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Lbu,
        type: I,
        asm: ["lbu" %(rt), #(s)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Lh,
        type: I,
        asm: ["lh" %(rt), #(s)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Lhu,
        type: I,
        asm: ["lhu" %(rt), #(s)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Lui,
        type: I,
        asm: ["lui" %(rt), #(s)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Lw,
        type: I,
        asm: ["lw" %(rt), #(s)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Lwc0,
        type: I,
        asm: ["lwc0" %(rt), #(s)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Lwc1,
        type: I,
        asm: ["lwc1" %(rt), #(s)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Lwc2,
        type: I,
        asm: ["lwc2" %(rt), #(s)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Lwc3,
        type: I,
        asm: ["lwc3" %(rt), #(s)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Lwl,
        type: I,
        asm: ["lwl" %(rt), #(s)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Lwr,
        type: I,
        asm: ["lwr" %(rt), #(s)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Mfhi,
        type: R,
        asm: ["mfhi" %(rd)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Mflo,
        type: R,
        asm: ["mflo" %(rd)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Mthi,
        type: R,
        asm: ["mthi" %(rd)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Mtlo,
        type: R,
        asm: ["mtlo" %(rd)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Mult,
        type: R,
        asm: ["mult" %(rs), %(rt)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Multu,
        type: R,
        asm: ["multu"  %(rs), %(rt)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Nor,
        type: R,
        asm: ["nor" %(rd), %(rs), %(rt)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Or,
        type: R,
        asm: ["or" %(rd), %(rs), %(rt)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Ori,
        type: I,
        asm: ["ori" %(rt), %(rs), #(s)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Sb,
        type: I,
        asm: ["sb" %(rt), #(s)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Sh,
        type: I,
        asm: ["sh" %(rt), #(s)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Sll,
        type: R,
        asm: ["sll" %(rd), %(rt), ^()],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Sllv,
        type: R,
        asm: ["sllv" %(rd), %(rt), %(rs)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Slt,
        type: R,
        asm: ["slt" %(rd), %(rs), %(rt)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Slti,
        type: I,
        asm: ["slti" %(rt), %(rs), #(s)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Sltiu,
        type: I,
        asm: ["sltiu" %(rt), %(rs), #(s)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Sltu,
        type: R,
        asm: ["sltu" %(rd), %(rs), %(rt)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Sra,
        type: R,
        asm: ["sra" %(rd), %(rt), ^()],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Srav,
        type: R,
        asm: ["srav"  %(rd), %(rt), %(rs)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Srl,
        type: R,
        asm: ["srl" %(rd), %(rt), ^()],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Srlv,
        type: R,
        asm: ["srlv"  %(rd), %(rt), %(rs)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Srv,
        type: I,
        asm: ["srv"],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Sub,
        type: R,
        asm: ["sub" %(rd), %(rs), %(rt)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Subu,
        type: R,
        asm: ["subu" %(rd), %(rs), %(rt)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Sw,
        type: I,
        asm: ["sw" %(rt), #(s)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Swc0,
        type: I,
        asm: ["swc0" %(rt), #(s)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Swc1,
        type: I,
        asm: ["swc1" %(rt), #(s)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Swc2,
        type: I,
        asm: ["swc2" %(rt), #(s)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Swc3,
        type: I,
        asm: ["swc3" %(rt), #(s)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Swl,
        type: I,
        asm: ["swl" %(rt), #(s)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Swr,
        type: I,
        asm: ["swr" %(rt), #(s)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Syscall,
        type: I,
        asm: ["syscall"],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Xor,
        type: R,
        asm: ["xor" %(rd), %(rs), %(rt)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
    {
        name: Xori,
        type: I,
        asm: ["xori" %(rt), %(rs), #(s)],
        |state, _| {
            todo!()
        },
        alu: {},
        |state, _| {
            todo!()
        },
        mem: {},
        |state, _| {
            todo!()
        },
    },
);
