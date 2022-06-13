// SPDX-License-Identifier: MPL-2.0

mod gfx;

use std::ops::{Deref, DerefMut};

impl Window {
    pub fn new(width: usize, height: usize) -> Self {
        let mut window =
            minifb::Window::new("Game", width, height, minifb::WindowOptions::default()).unwrap();
        window.limit_update_rate(None);

        let gfx = gfx::Renderer::new(&window, wgpu::Backends::all()).unwrap();

        Self { inner: window, gfx }
    }
}

pub struct Window {
    inner: minifb::Window,
    gfx: gfx::Renderer,
}

impl Window {
    pub fn gfx(&self) -> &gfx::Renderer {
        &self.gfx
    }

    pub fn gfx_mut(&mut self) -> &mut gfx::Renderer {
        &mut self.gfx
    }
}

impl Deref for Window {
    type Target = minifb::Window;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl DerefMut for Window {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}
