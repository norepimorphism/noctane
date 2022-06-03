// SPDX-License-Identifier: MPL-2.0

use std::fmt;

#[derive(Clone, Debug)]
pub struct Asm {
    pub op_name: String,
    pub operands: Vec<Operand>,
}

impl fmt::Display for Asm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} {}",
            self.op_name,
            self.operands
                .iter()
                .map(|operand| format!("{}", operand))
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

#[derive(Clone, Debug)]
pub enum Operand {
    Reg(u8),
    SInt(i32),
    Text(String),
    UInt(u32),
}

macro_rules! format_int {
    ($value:expr, $abs_value:expr) => {
        if $abs_value < 0x10 {
            format!("{}", $value)
        } else {
            format!("{:#x}", $value)
        }
    };
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Text(it) => {
                write!(f, "{}", it)
            }
            Self::Reg(it) => {
                write!(f, "r{}", it)
            }
            Self::SInt(it) => {
                let sign = match it.signum() {
                    -1 => "-",
                    _ => "",
                };

                write!(f, "{}{}", sign, format_int!(it, it.abs()))
            }
            Self::UInt(it) => {
                write!(f, "{}", format_int!(it, *it))
            }
        }
    }
}
