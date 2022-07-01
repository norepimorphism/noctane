// SPDX-License-Identifier: MPL-2.0

//! The PSX Graphics Processing Unit (GPU).
//!
//! This module implements GPU-related types and functions, which are considered to encompass the
//! following:
//! - GPU command decoding and execution
//! - Rendering

#![feature(is_some_with)]

pub mod cmd;
pub mod gfx;

use ringbuffer::{ConstGenericRingBuffer, RingBuffer as _, RingBufferExt as _};

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

impl Gpu {
    pub fn new(gfx: gfx::Renderer) -> Self {
        Self {
            display: Display::default(),
            dma_request: None,
            gfx,
            gp0_strat: cmd::gp0::QueueStrategy::PushWord,
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
    pub gfx: gfx::Renderer,
    gp0_strat: cmd::gp0::QueueStrategy,
    gp0_state: Option<cmd::gp0::State>,
    gp0_queue: ConstGenericRingBuffer<u32, 16>,
}

impl Gpu {
    pub fn reset(&mut self) {
        self.clear_gp0_queue();
        self.display.is_enabled = false;
        // TODO
    }

    pub fn gp0_queue_is_empty(&self) -> bool {
        self.gp0_queue.is_empty()
    }

    pub fn gp0_queue_is_full(&self) -> bool {
        self.gp0_queue.is_full()
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
