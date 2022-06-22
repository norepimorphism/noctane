// SPDX-License-Identifier: MPL-2.0

pub mod cmd;

use ringbuffer::ConstGenericRingBuffer;

pub use cmd::MachineCommand;

#[derive(Clone, Copy, Debug, Default)]
pub struct Point {
    pub x: u16,
    pub y: u16,
}

#[derive(Clone, Copy, Debug, Default)]
pub struct Extent2d {
    pub width: u16,
    pub height: u16,
}

impl Default for Gpu {
    fn default() -> Self {
        Self {
            gp0_state: None,
            gp0_queue: ConstGenericRingBuffer::new(),
        }
    }
}

/// The PSX Graphics Processing Unit (GPU).
#[derive(Debug)]
pub struct Gpu {
    gp0_state: Option<cmd::gp0::State>,
    gp0_queue: ConstGenericRingBuffer<u32, 16>,
}
