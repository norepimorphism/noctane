// SPDX-License-Identifier: MPL-2.0

pub mod cmd;

use ringbuffer::{ConstGenericRingBuffer, RingBufferExt as _};

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
            display: Display::default(),
            dma_request: None,
            gp0_state: None,
            gp0_queue: ConstGenericRingBuffer::new(),
        }
    }
}

/// The PSX Graphics Processing Unit (GPU).
#[derive(Debug)]
pub struct Gpu {
    pub display: Display,
    pub dma_request: Option<DmaRequest>,
    gp0_state: Option<cmd::gp0::State>,
    gp0_queue: ConstGenericRingBuffer<u32, 16>,
}

impl Gpu {
    pub fn reset(&mut self) {
        self.clear_gp0_queue();
        self.display.is_enabled = false;
        // TODO
    }

    pub fn clear_gp0_queue(&mut self) {
        self.gp0_queue.clear();
    }
}

#[derive(Debug, Default)]
pub struct Display {
    pub is_enabled: bool,
}

#[derive(Clone, Copy, Debug)]
pub enum DmaRequest {
    ToGp0,
    // TODO
}
