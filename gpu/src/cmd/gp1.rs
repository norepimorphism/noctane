// SPDX-License-Identifier: MPL-2.0

use noctane_util::BitStack as _;

use super::MachineCommand;

impl crate::Gpu {
    pub fn execute_gp1_machine_command(&mut self, mach: u32) {
        let mach = MachineCommand::decode(mach);
        self.execute_gp1_command(Command::decode(mach));
    }

    pub fn execute_gp1_command(&mut self, cmd: Command) {
        // TODO
        tracing::info!("GP1({:?})", cmd);

        match cmd {
            Command::ResetGpu => {
                self.reset();
            }
            Command::ClearCommandQueue => {
                self.clear_gp0_queue();
            }
            Command::AckInterrupt => {
                // TODO
            }
            Command::SetDisplayEnabled(value) => {
                self.display.is_enabled = value;
            }
            Command::SetDmaRequest(mut req) => {
                self.dma_request = match req.pop_bits(2) {
                    0 => None,
                    1 => todo!(),
                    2 => Some(crate::DmaRequest::ToGp0),
                    3 => todo!(),
                    // SAFETY: TODO
                    _ => unsafe { std::hint::unreachable_unchecked() },
                }
            }
            Command::SetDisplayAreaStart => {
                // TODO
            }
            Command::SetDisplayXRange => {
                // TODO
            }
            Command::SetDisplayYRange => {
                // TODO
            }
            Command::SetDisplayMode => {
                // TODO
            }
            Command::DisableTex => {
                // TODO
            }
            Command::GetGpuInfo => {
                // TODO
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum Command {
    ResetGpu,
    ClearCommandQueue,
    AckInterrupt,
    SetDisplayEnabled(bool),
    SetDmaRequest(u32),
    SetDisplayAreaStart,
    SetDisplayXRange,
    SetDisplayYRange,
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
            0x04 => Self::SetDmaRequest(mach.param),
            0x05 => Self::SetDisplayAreaStart,
            0x06 => Self::SetDisplayXRange,
            0x07 => Self::SetDisplayYRange,
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
