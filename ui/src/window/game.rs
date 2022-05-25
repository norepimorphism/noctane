mod gfx;

use std::ops::{Deref, DerefMut};

impl Window {
    pub fn new(width: usize, height: usize) -> Self {
        let window = minifb::Window::new(
            "Game",
            width,
            height,
            minifb::WindowOptions::default(),
        )
        .unwrap();
        let gfx = gfx::Renderer::new(
            &window,
            wgpu::Backends::all(),
            (640, 480),
        )
        .unwrap();

        Self { inner: window, gfx }
    }
}

pub struct Window {
    inner: minifb::Window,
    gfx: gfx::Renderer,
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
