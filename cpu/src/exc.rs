use std::collections::VecDeque;

pub type Queue = VecDeque<Exception>;

pub const VECTOR: u32 = 0x8000_0080;

pub mod code {
    pub const INTERRUPT:        u32 = 0;
    pub const TLB_MODIFICATION: u32 = 1;
    pub const TLB_LOAD:         u32 = 2;
    pub const TLB_STORE:        u32 = 3;
    pub const ADDRESS_LOAD:     u32 = 4;
    pub const ADDRESS_STORE:    u32 = 5;
    pub const BUS_FETCH:        u32 = 6;
    pub const BUS_LOAD:         u32 = 7;
    pub const SYSCALL:          u32 = 8;
    pub const BREAKPOINT:       u32 = 9;
    pub const RESERVED_INSTR:   u32 = 10;
    pub const COP_UNUSABLE:     u32 = 11;
    pub const INTEGER_OVERFLOW: u32 = 12;
}

impl Exception {
    pub fn new(code: u32, epc: u32) -> Self {
        Self { code, epc }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Exception {
    pub code: u32,
    pub epc: u32,
}
