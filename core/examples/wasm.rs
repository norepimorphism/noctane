#![feature(slice_as_chunks)]

use serde::Deserialize;
use wasm_bindgen::{prelude::*, JsCast as _};
use wasm_bindgen_futures::JsFuture;
use winit::{event_loop::EventLoop, platform::web::WindowExtWebSys as _, window::Window};

// Prevent `wasm_bindgen` from autostarting main on all spawned threads.
#[wasm_bindgen(start)]
pub fn dummy_main() {}

#[wasm_bindgen]
pub fn run() {
    main();
}

fn main() {
    noctane::log::init();

    let drop_callback = Closure::wrap(Box::new(handle_drop_wrapper) as Box<dyn Fn(_) -> _>);
    drop_zone_element()
        .add_event_listener_with_callback("drop", drop_callback.as_ref().unchecked_ref())
        .expect("failed to add 'drop' event listener");
    tracing::info!("Added 'drop' event listener");
    drop_callback.forget();
}

fn handle_drop_wrapper(event: web_sys::DragEvent) -> JsValue {
    wasm_bindgen_futures::future_to_promise(async {
        handle_drop(event).await;

        Ok(JsValue::UNDEFINED)
    })
    .into()
}

async fn handle_drop(event: web_sys::DragEvent) {
    if let Some(txfer) = event.data_transfer() {
        if let Some(files) = txfer.files() {
            if let Some(file) = files.get(0) {
                handle_file_drop(file).await;
            }
        }
    }
}

async fn handle_file_drop(file: web_sys::File) {
    tracing::info!("Received file drop event");

    let event_loop = EventLoop::new();
    let game_window = create_game_window(&event_loop);
    find_element_by_id("canvas-container")
        .expect("'#canvas-container' should exist")
        .replace_child(
            &web_sys::Element::from(game_window.canvas()),
            &drop_zone_element(),
        )
        .expect("failed to append canvas to document");

    // SAFETY: TODO
    let mut core = unsafe { noctane::Core::new(&game_window) };

    let reader: web_sys::ReadableStreamDefaultReader = file
        .stream()
        .get_reader()
        .unchecked_into();
    load_bios(reader, core.banks_mut().bios.as_mut_slice()).await;

    core.run(
        event_loop,
        game_window,
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

async fn load_bios(
    reader: web_sys::ReadableStreamDefaultReader,
    mut bios: &mut [u32],
) {
    #[derive(Debug, Deserialize)]
    struct ReadResult {
        chunk: Option<serde_bytes::ByteBuf>,
        done: bool,
    }

    tracing::info!("Loading BIOS");

    loop {
        tracing::info!("Reading stream...");
        let read_result: JsValue = JsFuture::from(reader.read())
            .await
            .expect("expected promise; found exception");
        let read_result: ReadResult = serde_wasm_bindgen::from_value(read_result)
            .expect("failed to deserialize read result");
        tracing::info!("{:#?}", read_result);
        if read_result.done {
            tracing::info!("Done");
            break;
        }

        let chunk_len = copy_chunk_to_bios(&read_result.chunk.unwrap(), bios);
        bios = &mut bios[chunk_len..];
    }
}

fn copy_chunk_to_bios(chunk: &[u8], bios: &mut [u32]) -> usize {
    // SAFETY: TODO
    let (_, chunk, _) = unsafe { chunk.align_to::<u32>() };
    let chunk_len = chunk.len();
    bios[..chunk_len].copy_from_slice(chunk);

    return chunk_len;
}
