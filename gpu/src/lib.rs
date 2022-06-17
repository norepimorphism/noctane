// SPDX-License-Identifier: MPL-2.0

pub mod cmd;

pub use cmd::MachineCommand;

impl Gpu {
    pub fn new() -> Self {
        Self(())
    }
}

/// The PSX Graphics Processing Unit (GPU).
#[derive(Debug, Default)]
pub struct Gpu(());

impl Gpu {
    pub fn queue_gp0_machine_command(&mut self, mach: u32) {
        let mach = MachineCommand::decode(mach);
        if let Some(cmd) = cmd::gp0::Command::decode(mach) {
            self.queue_gp0_command(cmd);
        } else {
            tracing::warn!("Invalid command: {:#010x}", mach);
        }
    }

    pub fn queue_gp0_command(&mut self, cmd: cmd::gp0::Command) {
        // TODO
        tracing::info!("GP0: {:#?}", cmd);
    }

    pub fn queue_gp1_machine_command(&mut self, mach: u32) -> u32 {
        todo!()
    }
}
