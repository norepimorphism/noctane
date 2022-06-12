// SPDX-License-Identifier: MPL-2.0

//! Procedural macros for Noctane.

#![feature(let_else, proc_macro_diagnostic)]

use proc_macro::TokenStream;

/// Generates procedural macros for [`noctane:cpu::bus::io`].
///
/// For optimal performance, Noctane uses LUTs to access I/O registers. This system works like so:
/// 1. The address of the desired I/O register, rebased to the start of the I/O region, is
///    continuously compared with the base addresses of the various I/O subregions until a match
///    is found.
///     ```
///     if addr >= subregion_1::BASE_ADDR {
///         subregion_1::access(addr)
///     }
///     if addr >= subregion_2::BASE_ADDR {
///         subregion_2::access(addr)
///     }
///     // And so on...
///     ```
/// 2. The address is rebased to the start of the matched I/O subregion. It represents the index of
///    the byte within this subregion at which the access is based.
///     ```
///     let index = addr - subregion_X::BASE_ADDR;
///     ```
/// 3. A LUT is indexed using the subregion byte index. This LUT contains an entry for each byte
///    within the matched subregion.
///     ```
///     let entry = &LUT[index];
///     ```
/// 4. The selected LUT entry points to an entry within an another LUT; this LUT represents the set
///    of all I/O registers. The first selected entry also contains a byte offset that indicates at
///    which byte within the selected I/O register the access is based.
///     ```
///     let LutEntry { reg_index, byte_offset } = entry;
///     ```
/// 5. The registers LUT is indexed using the register index from the first selected entry. This
///    register entry contains the name of the register as well as read and write functions for
///    8-bit, 16-bit, and 32-bit accesses. An access function is called according to the type of
///    access requested.
///     ```
///     let reg = &REGISTERS[reg_index];
///     // If a 32-bit read was requested...
///     let read = (reg.read_32)(io);
///     // If a 16-bit write was requested...
///     (reg.write_16)(io, value);
///     ```
///
/// The previous examples are a simplification of the real process, but they illustrate the flow
/// well enough. This procedural macro generates both LUTs, defining a module to contain each of the
/// LUTs of register pointers (for lack of a better name).
#[proc_macro]
pub fn gen_cpu_bus_io(input: TokenStream) -> TokenStream {
    cpu::bus::io::gen(input)
}

mod cpu {
    //! Procedural macros for [`noctane_cpu`].

    pub mod bus {
        //! Procedural macros for [`noctane_cpu::bus`].

        pub mod io;
    }
}
