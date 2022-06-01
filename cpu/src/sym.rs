pub fn for_syscall(code: u32) -> &'static str {
    match code {
        0 => "NoFunction",
        1 => "EnterCriticalSection",
        2 => "ExitCriticalSection",
        3 => "ChangeThreadSubFunction",
        _ => "DeliverEvent",
    }
}

pub fn for_break(code: u32) -> Option<&'static str> {
    match code {
        0x101 => Some("PCInit"),
        0x102 => Some("PCCreate"),
        0x103 => Some("PCOpen"),
        0x104 => Some("PCClose"),
        0x105 => Some("PCRead"),
        0x106 => Some("PCWrite"),
        0x107 => Some("PClSeek"),
        _ => None,
    }
}
