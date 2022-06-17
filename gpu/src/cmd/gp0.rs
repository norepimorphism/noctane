// SPDX-License-Identifier: MPL-2.0

use bit::BitIndex as _;

use super::MachineCommand;

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
    Transfer(txfer),
);

impl Command {
    pub fn decode(mut mach: MachineCommand) -> Option<Self> {
        let subopcode = mach.opcode.pop_bits(5);

        // Hopefully, this gets optimized into a jump table.
        match mach.opcode {
            0 => misc::Command::decode(subopcode, mach.param).map(Self::Misc),
            1 => poly::Command::decode(subopcode, mach.param).map(Self::Polygon),
            2 => line::Command::decode(subopcode, mach.param).map(Self::Line),
            3 => rect::Command::decode(subopcode, mach.param).map(Self::Rectangle),
            4..=6 => txfer::Command::decode(subopcode, mach.param).map(Self::Transfer),
            7 => env::Command::decode(subopcode, mach.param).map(Self::Env),
            _ => unsafe { unreachable_unchecked!() },
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
