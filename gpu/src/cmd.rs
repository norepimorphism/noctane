// SPDX-License-Identifier: MPL-2.0

pub mod gp0;
pub mod gp1;

use noctane_util::BitStack as _;

impl MachineCommand {
    pub fn decode(mut code: u32) -> Self {
        Self {
            opcode: code.pop_bits(8) as u8,
            param: code,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct MachineCommand {
    pub opcode: u8,
    pub param: u32,
}

impl MachineCommand {
    pub fn encode(self) -> u32 {
        let mut value = 0;
        value.push_bits(24, self.param);
        value.push_bits(8, self.opcode.into());

        value
    }
}
