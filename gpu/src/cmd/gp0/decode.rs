// SPDX-License-Identifier: MPL-2.0

use bit::BitIndex as _;

use super::Command;

macro_rules! def_decode_cmd_part {
    ($fn_name:ident, $range:tt, $ty:ty) => {
        #[inline(always)]
        pub fn $fn_name(code: u32) -> $ty {
            code.bit_range(enc_range::$range) as $ty
        }
    };
}

impl Command {
    def_decode_cmd_part!(decode_kind, KIND, u8);
}

mod enc_range {
    use std::ops::Range;

    pub const KIND: Range<usize> = 29..32;
}

impl Command {
    pub fn decode(code: u32) -> Self {
        match Self::decode_kind(code) {
            0 => Self::decode_misc(code),
            1 => Self::decode_poly(code),
            2 => Self::decode_line(code),
            3 => Self::decode_rect(code),
            4..=6 => Self::decode_tx(code),
            7 => Self::decode_env(code),
            _ => unreachable!(),
        }
    }

    pub fn decode_misc(code: u32) -> Self {
        todo!()
    }

    pub fn decode_poly(code: u32) -> Self {
        todo!()
    }

    pub fn decode_line(code: u32) -> Self {
        todo!()
    }

    pub fn decode_rect(code: u32) -> Self {
        todo!()
    }

    pub fn decode_tx(code: u32) -> Self {
        todo!()
    }

    pub fn decode_env(code: u32) -> Self {
        todo!()
    }
}
