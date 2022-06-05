// SPDX-License-Identifier: MPL-2.0

//! CPU exceptions.

/// The physical address to which execution is (usually) routed after an exception occurs.
pub const VECTOR: u32 = 0x8000_0080;

pub mod code {
    //! Exception codes.
    //!
    //! Each code represents a different kind of exception. In total, there are 13 different codes.

    /// The code for interrupts.
    pub const INTERRUPT: u32 = 0;
    /// The code for exceptions caused by TLB modifications.
    pub const TLB_MODIFICATION: u32 = 1;
    /// The code for exceptions caused by TLB loads.
    pub const TLB_LOAD: u32 = 2;
    /// The code for exceptions caused by TLB stores.
    pub const TLB_STORE: u32 = 3;
    /// The code for address-related exceptions caused by memory loads (e.g. instruction fetches).
    ///
    /// In particular, this code is yielded on attempts to access outside of *kuseg* while in user
    /// mode, or on attempts to read a word of hald-word at a misaligned address.
    pub const ADDRESS_LOAD: u32 = 4;
    /// The code for address-related exceptions caused by memory stores.
    ///
    /// In particular, this code is yielded in attempts to access outside of *kuseg* while in user
    /// mode, or on attempts to read a word of hald-word at a misaligned address.
    pub const ADDRESS_STORE: u32 = 5;
    /// The code for bus-related exceptions caused by instruction fetches.
    pub const BUS_FETCH: u32 = 6;
    /// The code for bus-related exceptions caused by data loads.
    pub const BUS_LOAD: u32 = 7;
    /// The code for `syscall` instruction invocations.
    pub const SYSCALL: u32 = 8;
    /// The code for `break` instruction invocations.
    pub const BREAKPOINT: u32 = 9;
    /// The code yielded in attempts to execute instructions marked by the MIPS I architecture as
    /// 'reserved'.
    ///
    /// In many cases, this code indicates an attempt to execute non-code data.
    pub const RESERVED_INSTR: u32 = 10;
    /// The code yielded in attempts to access an unavailable coprocessor.
    ///
    /// In the PSX, coprocessors 1 and 3 are unusable.
    pub const COP_UNUSABLE: u32 = 11;
    /// The code yielded in arithmetic overflow.
    ///
    /// This code is never yielded due to unsigned arithmetic instrucitons (e.g. *addu*).
    pub const INTEGER_OVERFLOW: u32 = 12;
}

impl Exception {
    /// Creates a new `Exception` with the given code and representing the instruction stored at the
    /// given exception program counter (EPC).
    pub fn new(code: u32, epc: u32) -> Self {
        Self { code, epc }
    }
}

/// A CPU exception.
///
/// Exceptions are not necessarily errors---they are, more accurately, interruptions in normal
/// code execution, which may be caused by errors (e.g., misaligned addresses, signed integer
/// overflow), but which may also be manually triggered by the user with instructions such as
/// `syscall` and `break`.
#[derive(Clone, Copy, Debug)]
pub struct Exception {
    /// A code which identifies the kind of exception that occurred.
    pub code: u32,
    /// The program address of the instruction that caused this exception.
    pub epc: u32,
}
