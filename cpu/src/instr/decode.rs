// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use bit::BitIndex as _;
use super::{
    Instr,
    IType,
    JType,
    OpKind,
    RType,
};

impl Instr {
    pub const OP_SPECIAL: u8 = 0;

    pub fn try_decode_op_kind(code: u32) -> Option<OpKind> {
        let op = Self::decode_op(code);

        if op == Self::OP_SPECIAL {
            let funct = Self::decode_funct(code);

            OpKind::try_decode_special(funct)
        } else {
            OpKind::try_decode_normal(op)
        }
    }
}

macro_rules! def_decode_instr_part {
    ($fn_name:ident, $range:tt, $ty:ty) => {
        #[inline(always)]
        pub fn $fn_name(code: u32) -> $ty {
            code.bit_range(enc_range::$range) as $ty
        }
    };
}

impl Instr {
    def_decode_instr_part!(decode_op, OP, u8);
    def_decode_instr_part!(decode_target, TARGET, u32);
    def_decode_instr_part!(decode_imm, IMM, u16);
    def_decode_instr_part!(decode_rs, RS, u8);
    def_decode_instr_part!(decode_rt, RT, u8);
    def_decode_instr_part!(decode_rd, RD, u8);
    def_decode_instr_part!(decode_shamt, SHAMT, u8);
    def_decode_instr_part!(decode_funct, FUNCT, u8);
}

mod enc_range {
    use std::ops::Range;

    pub const FUNCT: Range<usize> = 0..6;
    pub const SHAMT: Range<usize> = 6..11;
    pub const RD: Range<usize> = 11..16;
    pub const RT: Range<usize> = 16..21;
    pub const RS: Range<usize> = 21..26;
    pub const IMM: Range<usize> = 0..16;
    pub const TARGET: Range<usize> = 0..26;
    pub const OP: Range<usize> = 26..32;
}

impl OpKind {
    pub fn try_decode_normal(code: u8) -> Option<Self> {
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

    pub fn try_decode_special(code: u8) -> Option<Self> {
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

impl IType {
    pub fn decode(code: u32) -> Self {
        Self {
            rs: Instr::decode_rs(code),
            rt: Instr::decode_rt(code),
            imm: Instr::decode_imm(code),
        }
    }
}

impl JType {
    pub fn decode(code: u32) -> Self {
        Self {
            target: Instr::decode_target(code),
        }
    }
}

impl RType {
    pub fn decode(code: u32) -> Self {
        Self {
            rs: Instr::decode_rs(code),
            rt: Instr::decode_rt(code),
            rd: Instr::decode_rd(code),
            shamt: Instr::decode_shamt(code),
            funct: Instr::decode_funct(code),
        }
    }
}
