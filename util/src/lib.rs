// SPDX-License-Identifier: MPL-2.0

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

pub trait BitStackExt {
    fn pop_bool(&mut self) -> bool;

    fn push_bool(&mut self, value: bool);
}

macro_rules! impl_bitstack_for_ty {
    ($($ty:ty)*) => {
        $(
            impl BitStack for $ty {
                #[inline(always)]
                fn pop_bits(&mut self, count: $ty) -> $ty {
                    let value = *self & (((1 as $ty) << count).saturating_sub(1));
                    *self >>= count;

                    value
                }

                #[inline(always)]
                fn push_bits(&mut self, count: $ty, value: $ty) {
                    *self <<= count;
                    *self |= value & (((1 as $ty) << count).saturating_sub(1));
                }
            }

            impl BitStackExt for $ty {
                #[inline(always)]
                fn pop_bool(&mut self) -> bool {
                    self.pop_bits(1) == 1
                }

                #[inline(always)]
                fn push_bool(&mut self, value: bool) {
                    self.push_bits(1, value.into())
                }
            }
        )*
    };
}

impl_bitstack_for_ty! { u8 u16 u32 }

#[cfg(test)]
mod tests {
    use super::BitStack as _;

    #[test]
    fn push_no_bits() {
        let mut code: u32 = 0;
        code.push_bits(0, !0);
        assert_eq!(code, 0);
    }

    #[test]
    fn push_some_bits() {
        let mut code: u32 = 0;
        for i in 1..=7 {
            code.push_bits(
                i,
                if (i % 2) == 0 {
                    0
                } else {
                    !0
                }
            )
        }
        assert_eq!(code, 0b1001110000111110000001111111);
    }

    #[test]
    fn pop_no_bits() {
        let mut code: u32 = !0;
        assert_eq!(code.pop_bits(0), 0);
        assert_eq!(code, !0);
    }

    #[test]
    fn pop_some_bits() {
        let mut code: u32 = 0b1111111000000111110000111001;
        for i in 1..=7 {
            assert_eq!(
                code.pop_bits(i),
                if (i % 2) == 0 {
                    0
                } else {
                    !0 & ((1 << i) - 1)
                },
            )
        }
        assert_eq!(code, 0);
    }
}
