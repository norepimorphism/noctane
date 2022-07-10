#![feature(slice_as_chunks)]

use wasm_bindgen::{prelude::*, JsCast as _};
use wasm_bindgen_futures::JsFuture;
use winit::platform::web::WindowExtWebSys as _;

fn main() {
    run();
}

#[wasm_bindgen(start)]
pub fn run() {
    let event_loop = winit::event_loop::EventLoop::new();
    let game_window = winit::window::WindowBuilder::new()
        .with_inner_size(winit::dpi::LogicalSize::new(1024, 512))
        .with_resizable(false)
        .build(&event_loop)
        .expect("failed to create window");

    let web_window = web_sys::window().expect("window should exist in this context");
    let noctane_elem = web_window
        .document()
        .expect("document should exist in this context")
        .get_element_by_id("noctane")
        .expect("#noctane should exist")
        .append_child(&web_sys::Element::from(game_window.canvas()))
        .expect("failed to append canvas to document");

    let mut thread = ();

    web_window.set_ondrop(Some({
        Closure::once(|event: web_sys::DragEvent| {
            if let Some(txfer) = event.data_transfer() {
                if let Some(files) = txfer.files() {
                    if let Some(file) = files.get(0) {
                        let stream = file.stream();
                        let reader: web_sys::ReadableStreamDefaultReader = stream
                            .get_reader()
                            .unchecked_into();

                        noctane::Core::run(
                            event_loop,
                            game_window,
                            |core| {
                                let mut bios = core.banks_mut().bios.as_mut_slice();

                                loop {
                                    let read: js_sys::Object = pollster::block_on(JsFuture::from(reader.read()))
                                        .expect("expected promise; found exception")
                                        .into();
                                    let read = js_sys::Object::values(&read);
                                    let is_done = read
                                        .at(1)
                                        .as_bool()
                                        .expect("expected boolean");
                                    if is_done {
                                        break;
                                    }

                                    let chunk: js_sys::Uint8Array = read.at(0).unchecked_into();
                                    let chunk = chunk.to_vec();
                                    let word_count = chunk.len() / 4;

                                    // Fill BIOS with the ROM image.
                                    for (idx, word) in chunk
                                        .as_chunks::<4>()
                                        .0
                                        .into_iter()
                                        .enumerate()
                                    {
                                        // The PSX CPU is little-endian, so we must make sure that
                                        // if the host platform is big-endian, the bytes are swapped
                                        // before being written.
                                        bios[idx] = u32::from_le_bytes(*word);
                                    }
                                    bios = &mut bios[word_count..];
                                }
                            },
                            |mut core| {
                                loop {
                                    core.step();
                                }
                            },
                        );
                    }
                }
            }
        })
        .as_ref()
        .unchecked_ref()
    }));
}
