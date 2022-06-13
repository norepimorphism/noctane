// SPDX-License-Identifier: MPL-2.0

pub mod gp0;
pub mod gp1;

pub fn split_mach(mach: u32) -> (u8, u32) {
    let opcode = (mach >> 24) as u8;
    let param = mach & ((1 << 25) - 1);

    (opcode, param)
}
