// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

pub mod asm;
pub mod decode;

pub use asm::Asm;

macro_rules! def_instr_and_op_kind {
    (
        $(
            (
                $variant_name:tt,
                $type:tt,
                [
                    $display_name:literal
                    $(
                        $kind:tt($($arg:tt)?)
                    ),*
                ]
            ) $(,)?
        ),*
    ) => {
        #[derive(Clone, Debug)]
        pub enum Instr {
            $(
                $variant_name(concat_idents!($type, Type))
            ),*
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
                    ),*
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
                    ),*
                }
            }
        }

        pub enum OpKind {
            $(
                $variant_name
            ),*
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

def_instr_and_op_kind!(
    (Add,       R,  ["add" %(rd), %(rs), %(rt)]),
    (Addi,      I,  ["addi" %(rt), %(rs), #(s)]),
    (Addiu,     I,  ["addiu" %(rt), %(rs), #(s)]),
    (Addu,      R,  ["addu" %(rd), %(rs), %(rt)]),
    (And,       R,  ["and" %(rd), %(rs), %(rt)]),
    (Andi,      I,  ["andi" %(rt), %(rs), #(s)]),
    (BCond,     I,  ["bcond" %(rs), #(s)]),
    (Beq,       I,  ["beq" %(rs), %(rt), #(s)]),
    (Bgtz,      I,  ["bgtz" %(rs), #(s)]),
    (Blez,      I,  ["blez" %(rs), #(s)]),
    (Bne,       I,  ["bne" %(rs), %(rt), #(s)]),
    (Break,     I,  ["break"]),
    (Cop0,      I,  ["cop0"]),
    (Cop1,      I,  ["cop1"]),
    (Cop2,      I,  ["cop2"]),
    (Cop3,      I,  ["cop3"]),
    (Div,       R,  ["div"  %(rs), %(rt)]),
    (Divu,      R,  ["divu"  %(rs), %(rt)]),
    (J,         J,  ["j" *()]),
    (Jal,       J,  ["jal" *()]),
    (Jalr,      R,  ["jalr" %(rs), %(rd)]),
    (Jr,        R,  ["jr" %(rs)]),
    (Lb,        I,  ["lb" %(rt), #(s)]),
    (Lbu,       I,  ["lbu" %(rt), #(s)]),
    (Lh,        I,  ["lh" %(rt), #(s)]),
    (Lhu,       I,  ["lhu" %(rt), #(s)]),
    (Lui,       I,  ["lui" %(rt), #(s)]),
    (Lw,        I,  ["lw" %(rt), #(s)]),
    (Lwc0,      I,  ["lwc0" %(rt), #(s)]),
    (Lwc1,      I,  ["lwc1" %(rt), #(s)]),
    (Lwc2,      I,  ["lwc2" %(rt), #(s)]),
    (Lwc3,      I,  ["lwc3" %(rt), #(s)]),
    (Lwl,       I,  ["lwl" %(rt), #(s)]),
    (Lwr,       I,  ["lwr" %(rt), #(s)]),
    (Mfhi,      R,  ["mfhi" %(rd)]),
    (Mflo,      R,  ["mflo" %(rd)]),
    (Mthi,      R,  ["mthi" %(rd)]),
    (Mtlo,      R,  ["mtlo" %(rd)]),
    (Mult,      R,  ["mult" %(rs), %(rt)]),
    (Multu,     R,  ["multu"  %(rs), %(rt)]),
    (Nor,       R,  ["nor" %(rd), %(rs), %(rt)]),
    (Or,        R,  ["or" %(rd), %(rs), %(rt)]),
    (Ori,       I,  ["ori" %(rt), %(rs), #(s)]),
    (Sb,        I,  ["sb" %(rt), #(s)]),
    (Sh,        I,  ["sh" %(rt), #(s)]),
    (Sll,       R,  ["sll" %(rd), %(rt), ^()]),
    (Sllv,      R,  ["sllv" %(rd), %(rt), %(rs)]),
    (Slt,       R,  ["slt" %(rd), %(rs), %(rt)]),
    (Slti,      I,  ["slti" %(rt), %(rs), #(s)]),
    (Sltiu,     I,  ["sltiu" %(rt), %(rs), #(s)]),
    (Sltu,      R,  ["sltu" %(rd), %(rs), %(rt)]),
    (Sra,       R,  ["sra" %(rd), %(rt), ^()]),
    (Srav,      R,  ["srav"  %(rd), %(rt), %(rs)]),
    (Srl,       R,  ["srl" %(rd), %(rt), ^()]),
    (Srlv,      R,  ["srlv"  %(rd), %(rt), %(rs)]),
    (Srv,       I,  ["srv"]),
    (Sub,       R,  ["sub" %(rd), %(rs), %(rt)]),
    (Subu,      R,  ["subu" %(rd), %(rs), %(rt)]),
    (Sw,        I,  ["sw" %(rt), #(s)]),
    (Swc0,      I,  ["swc0" %(rt), #(s)]),
    (Swc1,      I,  ["swc1" %(rt), #(s)]),
    (Swc2,      I,  ["swc2" %(rt), #(s)]),
    (Swc3,      I,  ["swc3" %(rt), #(s)]),
    (Swl,       I,  ["swl" %(rt), #(s)]),
    (Swr,       I,  ["swr" %(rt), #(s)]),
    (Syscall,   I,  ["syscall"]),
    (Xor,       R,  ["xor" %(rd), %(rs), %(rt)]),
    (Xori,      I,  ["xori" %(rt), %(rs), #(s)]),
);

#[derive(Clone, Copy, Debug)]
pub struct IType {
    pub rs: u8,
    pub rt: u8,
    pub imm: u16,
}

#[derive(Clone, Copy, Debug)]
pub struct JType {
    pub target: u32,
}

#[derive(Clone, Copy, Debug)]
pub struct RType {
    pub rs: u8,
    pub rt: u8,
    pub rd: u8,
    pub shamt: u8,
    pub funct: u8,
}
