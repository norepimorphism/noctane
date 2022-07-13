#![feature(slice_as_chunks)]

use wasm_bindgen::{prelude::*, JsCast as _};
use wasm_bindgen_futures::JsFuture;
use wasm_rs_async_executor::single_threaded as async_executor;
use winit::{event_loop::EventLoop, platform::web::WindowExtWebSys as _, window::Window};

fn main() {
    run();
}

#[wasm_bindgen(start)]
pub fn run() {
    // noctane::log::init();

    let drop_callback = Closure::wrap(Box::new(handle_drop) as Box<dyn Fn(_)>);
    drop_zone_element()
        .add_event_listener_with_callback("drop", drop_callback.as_ref().unchecked_ref())
        .expect("failed to add 'drop' event listener");
    drop_callback.forget();
}

fn drop_zone_element() -> web_sys::Element {
    find_element_by_id("drop-zone").expect("'#drop-zone' should exist")
}

fn find_element_by_id(id: &str) -> Option<web_sys::Element> {
    web_sys::window()
        .expect("window should exist")
        .document()
        .expect("document should existt")
        .get_element_by_id(id)
}

fn handle_drop(event: web_sys::DragEvent) {
    if let Some(txfer) = event.data_transfer() {
        if let Some(files) = txfer.files() {
            if let Some(file) = files.get(0) {
                handle_file_drop(file);
            }
        }
    }
}

fn handle_file_drop(file: web_sys::File) {
    run_noctane(|core| {
        let reader: web_sys::ReadableStreamDefaultReader = file
            .stream()
            .get_reader()
            .unchecked_into();
        let mut bios = core.banks_mut().bios.as_mut_slice();
        load_bios(reader, bios);
    });
}

fn run_noctane(setup: impl Fn(&mut noctane::Core)) {
    let event_loop = EventLoop::new();
    let game_window = create_game_window(&event_loop);
    find_element_by_id("canvas-container")
        .expect("'#canvas-container' should exist")
        .replace_child(
            &web_sys::Element::from(game_window.canvas()),
            &drop_zone_element(),
        )
        .expect("failed to append canvas to document");

    noctane::Core::run(
        event_loop,
        game_window,
        setup,
        |mut core| {
            loop {
                core.step();
            }
        },
    );
}

fn create_game_window(event_loop: &EventLoop<()>) -> Window {
    winit::window::WindowBuilder::new()
        .with_inner_size(winit::dpi::LogicalSize::new(1024, 512))
        .with_resizable(false)
        .build(event_loop)
        .expect("failed to create window")
}

fn load_bios(
    reader: web_sys::ReadableStreamDefaultReader,
    mut bios: &mut [u32],
) {
    loop {
        let read: js_sys::Object = async_executor::block_on(JsFuture::from(reader.read()))
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
        let chunk_len = copy_chunk_to_bios(&chunk, bios);
        bios = &mut bios[chunk_len..];
    }
}

fn copy_chunk_to_bios(chunk: &[u8], bios: &mut [u32]) -> usize {
    let chunk_len = chunk.len();

    // SAFETY: TODO
    let (_, chunk, _) = unsafe { chunk.align_to::<u32>() };
    bios[..chunk_len].copy_from_slice(chunk);

    return chunk_len;
}
