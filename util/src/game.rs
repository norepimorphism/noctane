// SPDX-License-Identifier: MPL-2.0

pub fn create_window(width: usize, height: usize) -> minifb::Window {
    let mut window =
        minifb::Window::new("Game", width, height, minifb::WindowOptions::default()).unwrap();
    window.limit_update_rate(None);

    window
}
