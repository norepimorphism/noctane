#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Kind {
    Syscall = 8,
    Breakpoint = 9,
    ReservedInstr = 10,
    UnusableCop = 11,
    IntegerOverflow = 12,
}
