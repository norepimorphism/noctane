// SPDX-License-Identifier: MPL-2.0

use bit::BitIndex as _;

macro_rules! def_cmd_and_kind {
    (
        $($variant_name:ident ($mod_name:ident)),* $(,)?
    ) => {
        #[derive(Debug)]
        pub enum Kind {
            $($variant_name,)*
        }

        #[derive(Debug)]
        pub enum Command {
            $($variant_name($mod_name::Command),)*
        }
    };
}

def_cmd_and_kind!(
    Misc(misc),
    Env(env),
    Polygon(poly),
    Line(line),
    Rectangle(rect),
    Transfer(tx),
);

macro_rules! def_decode_opcode_part {
    ($fn_name:ident, $range:tt, $ty:ty) => {
        #[inline(always)]
        pub fn $fn_name(opcode: u8) -> $ty {
            opcode.bit_range(opcode_bits::$range) as $ty
        }
    };
}

def_decode_opcode_part!(decode_kind, KIND, u8);

mod opcode_bits {
    use std::ops::Range;

    pub const KIND: Range<usize> = 5..8;
}

impl Command {
    pub fn decode(opcode: u8, param: u32) -> Option<Self> {
        let mut subopcode = opcode;
        // The 'subopcode' is equivalent to the original opcode but with the `KIND` bits cleared.
        subopcode.set_bit_range(opcode_bits::KIND, 0);

        // Hopefully, this gets optimized into a jump table.
        match decode_kind(opcode) {
            0 => misc::Command::decode(subopcode, param).map(Self::Misc),
            1 => poly::Command::decode(subopcode, param).map(Self::Polygon),
            2 => line::Command::decode(subopcode, param).map(Self::Line),
            3 => rect::Command::decode(subopcode, param).map(Self::Rectangle),
            4..=6 => tx::Command::decode(subopcode, param).map(Self::Transfer),
            7 => env::Command::decode(subopcode, param).map(Self::Env),
            _ => unreachable!(),
        }
    }
}

pub mod misc {
    #[derive(Debug)]
    pub struct Command;

    impl Command {
        pub fn decode(opcode: u8, param: u32) -> Option<Self> {
            todo!()
        }
    }
}

pub mod env {
    #[derive(Debug)]
    pub enum Command {
        SetTexpage,
    }

    impl Command {
        pub fn decode(opcode: u8, param: u32) -> Option<Self> {
            match opcode {
                // TODO
                1 => Some(Self::SetTexpage),
                // TODO
                _ => None,
            }
        }
    }
}

pub mod poly {
    #[derive(Debug)]
    pub struct Command;

    impl Command {
        pub fn decode(opcode: u8, param: u32) -> Option<Self> {
            todo!()
        }
    }
}

pub mod line {
    #[derive(Debug)]
    pub struct Command;

    impl Command {
        pub fn decode(opcode: u8, param: u32) -> Option<Self> {
            todo!()
        }
    }
}

pub mod rect {
    #[derive(Debug)]
    pub struct Command;

    impl Command {
        pub fn decode(opcode: u8, param: u32) -> Option<Self> {
            todo!()
        }
    }
}

pub mod tx {
    #[derive(Debug)]
    pub struct Command;

    impl Command {
        pub fn decode(opcode: u8, param: u32) -> Option<Self> {
            todo!()
        }
    }
}
