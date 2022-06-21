// SPDX-License-Identifier: MPL-2.0

use super::MachineCommand;

#[derive(Clone, Debug)]
pub enum Command {
    ResetGpu,
    ClearCommandQueue,
    AckInterrupt,
    SetDisplayEnabled(bool),
    SetDmaSource,
    SetDisplayAreaStart,
    DisableTex,
    GetGpuInfo,
    X27,
}

impl Command {
    pub fn decode(mach: MachineCommand) -> Self {
        // Crossing my fingers that this gets optimized into a LUT...
        match mach.opcode % 0x40 {
            0x00 => Self::ResetGpu,
            0x01 => Self::ClearCommandQueue,
            0x02 => Self::AckInterrupt,
            0x03 => Self::SetDisplayEnabled(mach.param != 0),
            0x04 => Self::SetDmaSource,
            0x05 => Self::SetDisplayAreaStart,
            0x09 => Self::DisableTex,
            0x0a..=0x0f => {
                // TODO: Undocumented.
                todo!()
            }
            0x10..=0x1f => Self::GetGpuInfo,
            0x20 => Self::DisableTex,
            0x21..=0x26 => {
                // TODO: Undocumented.
                todo!()
            }
            0x27 => Self::X27,
            0x28..=0x3f => {
                // TODO: Undocumented.
                todo!()
            }
            // SAFETY: TODO
            0x40.. => unsafe { std::hint::unreachable_unchecked() },
            _ => todo!("Unknown opcode: {:#04x}", mach.opcode),
        }
    }
}
