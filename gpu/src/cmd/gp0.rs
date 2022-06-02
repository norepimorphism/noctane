// SPDX-License-Identifier: MPL-2.0

pub mod decode;

macro_rules! def_cmd_and_kind {
    (
        $($variant_name:ident ($mod_name:ident)),* $(,)?
    ) => {
        pub enum Kind {
            $($variant_name,)*
        }

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

pub mod misc {
    pub struct Command;
}

pub mod env {
    pub struct Command;
}

pub mod poly {
    pub struct Command;
}

pub mod line {
    pub struct Command;
}

pub mod rect {
    pub struct Command;
}

pub mod tx {
    pub struct Command;
}
