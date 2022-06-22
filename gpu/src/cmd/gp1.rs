// SPDX-License-Identifier: MPL-2.0

use super::MachineCommand;

impl crate::Gpu {
    pub fn execute_gp1_machine_command(&mut self, mach: u32) {
        let mach = MachineCommand::decode(mach);
        self.execute_gp1_command(Command::decode(mach));
    }

    pub fn execute_gp1_command(&mut self, cmd: Command) {
        // TODO
        // tracing::info!("GP1: {:?}", cmd);
    }
}

#[derive(Clone, Debug)]
pub enum Command {
    ResetGpu,
    ClearCommandQueue,
    AckInterrupt,
    SetDisplayEnabled(bool),
    SetDmaSource,
    SetDisplayAreaStart,
    SetDisplayRangeX,
    SetDisplayRangeY,
    SetDisplayMode,
    DisableTex,
    GetGpuInfo,
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
            0x06 => Self::SetDisplayRangeX,
            0x07 => Self::SetDisplayRangeY,
            0x08 => Self::SetDisplayMode,
            0x09 => Self::DisableTex,
            0x0a..=0x0f => {
                // TODO: Undocumented.
                todo!("{:#04x}", mach.opcode)
            }
            0x10..=0x1f => Self::GetGpuInfo,
            0x20 => Self::DisableTex,
            0x21..=0x3f => {
                // TODO: Undocumented.
                todo!("{:#04x}", mach.opcode)
            }
            // SAFETY: TODO
            0x40.. => unsafe { std::hint::unreachable_unchecked() },
        }
    }
}
