// SPDX-License-Identifier: MPL-2.0

use std::fmt;

use noctane_cpu::reg;

pub struct Call {
    pub name: &'static str,
    pub args: &'static [Argument],
}

impl fmt::Display for Call {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}({})",
            self.name,
            self.args.iter().map(|arg| arg.to_string()).collect::<Vec<String>>().join(","),
        )
    }
}

pub struct Argument {
    pub name: &'static str,
    pub value: (),
}

impl fmt::Display for Argument {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl Call {
    pub fn new(name: &'static str, args: &'static [Argument]) -> Self {
        Self { name, args }
    }

    pub fn in_a0_table(reg: &reg::File, offset: u32) -> Option<Self> {
        None
    }

    pub fn in_b0_table(reg: &reg::File, offset: u32) -> Option<Self> {
        None
    }

    pub fn in_c0_table(reg: &reg::File, offset: u32) -> Option<Self> {
        None
    }

    pub fn try_from_break(reg: &reg::File, code: u32) -> Option<Self> {
        match code {
            0x101 => Some(Self::pc_init()),
            0x102 => Some(Self::pc_create()),
            0x103 => Some(Self::pc_open()),
            0x104 => Some(Self::pc_close()),
            0x105 => Some(Self::pc_read()),
            0x106 => Some(Self::pc_write()),
            0x107 => Some(Self::pc_l_seek()),
            _ => None,
        }
    }

    pub fn from_syscall(reg: &reg::File, code: u32) -> Self {
        match code {
            0 => Self::no_function(),
            1 => Self::enter_critical_section(),
            2 => Self::exit_critical_section(),
            3 => Self::change_thread_sub_function(),
            _ => Self::deliver_event(),
        }
    }
}

macro_rules! def_fn {
    ($fn_name:ident => $name:ident ($($arg_name:ident : $arg_kind:tt),* $(,)?)) => {
        pub fn $fn_name($($arg_name: ArgumentKind::$arg_kind)*) -> Self {
            Self::new(stringify!($name), &[$(Argument::new($arg_name, ArgumentKind::$arg_kind),)*])
        }
    };
}

impl Call {
    def_fn!(pc_init                         => PCInit());
    def_fn!(pc_create                       => PCCreate());
    def_fn!(pc_open                         => PCOpen());
    def_fn!(pc_close                        => PCClose());
    def_fn!(pc_read                         => PCRead());
    def_fn!(pc_write                        => PCWrite());
    def_fn!(pc_l_seek                       => PClSeek());

    def_fn!(no_function                     => NoFunction());
    def_fn!(enter_critical_section          => EnterCriticalSection());
    def_fn!(exit_critical_section           => ExitCriticalSection());
    def_fn!(change_thread_sub_function      => ChangeThreadSubFunction());
    def_fn!(deliver_event                   => DeliverEvent());
}
