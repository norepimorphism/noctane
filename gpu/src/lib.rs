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

use ringbuffer::RingBufferExt as _;

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
            display: Default::default(),
            dma_request: None,
            gfx,
            gp0: Default::default(),
            tex_page: Default::default(),
            tex_window: Default::default(),
        }
    }
}

/// The PSX Graphics Processing Unit (GPU).
#[derive(Debug)]
pub struct Gpu {
    display: Display,
    dma_request: Option<DmaRequest>,
    gfx: gfx::Renderer,
    gp0: cmd::gp0::State,
    tex_page: cmd::gp0::TexturePage,
    tex_window: cmd::gp0::TextureWindow,
}

impl Gpu {
    pub fn display(&self) -> &Display {
        &self.display
    }

    pub fn display_mut(&mut self) -> &mut Display {
        &mut self.display
    }

    pub fn gfx(&self) -> &gfx::Renderer {
        &self.gfx
    }

    pub fn gfx_mut(&mut self) -> &mut gfx::Renderer {
        &mut self.gfx
    }

    pub fn gp0(&self) -> &cmd::gp0::State {
        &self.gp0
    }

    pub fn gp0_mut(&mut self) -> &mut cmd::gp0::State {
        &mut self.gp0
    }

    pub fn reset(&mut self) {
        self.gp0.queue.clear();
        self.display.is_enabled = false;
        // TODO
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
