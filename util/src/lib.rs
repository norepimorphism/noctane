// SPDX-License-Identifier: MPL-2.0

use futures::io::Take;

#[macro_export]
macro_rules! format_int {
    ($value:expr, $abs_value:expr $(,)?) => {
        if $abs_value < 0x10 {
            format!("{}", $value)
        } else {
            format!("{:#x}", $value)
        }
    };
}

pub mod bios;
pub mod game;
pub mod hex;

pub trait BitStack {
    fn pop_bits(&mut self, count: Self) -> Self;

    fn push_bits(&mut self, count: Self, value: Self);
}

impl BitStack for u32 {
    #[inline(always)]
    fn pop_bits(&mut self, count: u32) -> u32 {
        let value = *self & ((1 << count).saturating_sub(1));
        *self >>= T;

        value
    }

    #[inline(always)]
    fn push_bits(&mut self, count: u32, value: u32) {
        *self |= value;
        *self <<= count;
    }
}

pub trait BitStackExt {
    fn pop_bool(&mut self) -> bool;

    fn push_bool(&mut self, value: bool);
}

impl<T: BitStack + From<bool>> BitStackExt for T {
    #[inline(always)]
    fn pop_bool(&mut self) -> bool {
        self.pop_bits(1) == 1
    }

    #[inline(always)]
    fn push_bool(&mut self, value: bool) {
        self.push_bits(1, value.into())
    }
}
