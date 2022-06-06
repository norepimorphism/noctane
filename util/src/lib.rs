// SPDX-License-Identifier: MPL-2.0

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
pub mod hex;
