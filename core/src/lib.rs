// SPDX-License-Identifier: MPL-2.0

#![feature(slice_as_chunks)]

use std::time::Instant;

pub use noctane_cpu::Cpu;
pub use noctane_gpu::Gpu;
use winit::{event_loop::EventLoop, window::Window};

use std::sync::{Arc, Condvar, Mutex};

pub mod bios;

struct Render {
    state: Mutex<RenderState>,
    cvar: Condvar,
}

enum RenderState {
    Requested,
    InProgress,
    Complete,
}

impl Render {
    fn new() -> Self {
        Self {
            state: Mutex::new(RenderState::Complete),
            cvar: Condvar::new(),
        }
    }
}

impl Core {
    pub fn run(
        event_loop: EventLoop<()>,
        game_window: Window,
        setup: impl Fn(&mut Self),
        main: impl Fn(Self) + Sync + Send + 'static,
    ) {
        let render = Arc::new(Render::new());
        // SAFETY: TODO
        let mut this = unsafe { Self::new(&game_window, Arc::clone(&render)) };
        let mut main_thread = Some(std::thread::spawn(move || main(this)));

        event_loop.run(move |event, _, ctrl_flow| {
            use winit::{
                event::{Event, WindowEvent},
                event_loop::ControlFlow,
            };

            *ctrl_flow = ControlFlow::Poll;

            match event {
                Event::WindowEvent { event: WindowEvent::CloseRequested, .. } => {
                    if let Some(thread) = main_thread.take() {
                        thread.join().unwrap();
                    }
                    *ctrl_flow = ControlFlow::Exit;
                }
                Event::MainEventsCleared => {
                    if matches!(*render.state.lock().unwrap(), RenderState::Requested) {
                        game_window.request_redraw();
                    }
                }
                Event::RedrawRequested(_) => {
                    let render = &*render;
                    let mut render_state = render.state.lock().unwrap();
                    if matches!(*render_state, RenderState::Requested) {
                        *render_state = RenderState::InProgress;
                        render.cvar.notify_one();

                        let _ = render.cvar.wait_while(
                            render_state,
                            |state| matches!(state, RenderState::InProgress),
                        )
                        .unwrap();
                    }
                }
                _ => {},
            }
        });
    }

    unsafe fn new(game_window: &Window, render: Arc<Render>) -> Self {
        // SAFETY: TODO
        let gfx = pollster::block_on(noctane_gpu::gfx::Renderer::new(
            &game_window,
            wgpu_types::Backends::all(),
        ))
        .map(|mut it| {
            let size = game_window.inner_size();
            it.resize(size.width, size.height);

            it
        })
        .expect("failed to create renderer");

        Self {
            banks: Default::default(),
            bus_cfg: Default::default(),
            cpu_state: Default::default(),
            dma_cfg: Default::default(),
            instrs_since_last_vblank: 0,
            int: Default::default(),
            gpu: Gpu::new(gfx),
            last_gpu_result: 0,
            last_vblank: Instant::now(),
            post: Default::default(),
            ram_cfg: Default::default(),
            render: render,
            spu_cfg: Default::default(),
            spu_voices: Default::default(),
            timers: Default::default(),
        }
    }
}

pub struct Core {
    banks: Banks,
    bus_cfg: noctane_cpu::bus::io::bus::Config,
    cpu_state: noctane_cpu::State,
    dma_cfg: noctane_cpu::bus::io::dma::Config,
    instrs_since_last_vblank: usize,
    int: noctane_cpu::bus::io::int::Sources,
    gpu: Gpu,
    last_gpu_result: u32,
    last_vblank: Instant,
    post: noctane_cpu::bus::io::post::Status,
    ram_cfg: noctane_cpu::bus::io::ram::Config,
    render: Arc<Render>,
    spu_cfg: noctane_cpu::bus::io::spu::Config,
    spu_voices: [noctane_cpu::bus::io::spu_voice::Config; 24],
    timers: noctane_cpu::bus::io::Timers,
}

#[derive(Default)]
pub struct Banks {
    pub exp_1: noctane_cpu::bus::Exp1,
    pub exp_3: noctane_cpu::bus::Exp3,
    pub bios: noctane_cpu::bus::Bios,
    pub ram: noctane_cpu::bus::Ram,
}

impl Core {
    pub fn banks_mut(&mut self) -> &mut Banks {
        &mut self.banks
    }

    pub fn step(&mut self) -> noctane_cpu::instr::Executed {
        if self.take_vblank().is_some() {
            self.do_vblank();
        }
        self.gpu.execute_next_gp0_command();
        let execed = self.cpu().execute_next_instr();
        self.instrs_since_last_vblank += 1;

        execed
    }

    fn take_vblank(&mut self) -> Option<()> {
        // Greatly reduce the number of syscalls produced.
        if (self.instrs_since_last_vblank < 50_000) || ((self.instrs_since_last_vblank % 5_000) > 0) {
            return None;
        }
        self.instrs_since_last_vblank = 0;

        // 60 Hz.
        if self.last_vblank.elapsed().as_millis() >= 16 {
            self.last_vblank = Instant::now();

            Some(())
        } else {
            None
        }
    }

    fn do_vblank(&mut self) {
        {
            let render = &*self.render;
            // Request a render operation.
            let mut render_state = render.state.lock().unwrap();
            *render_state = RenderState::Requested;
            // The GUI thread locks `render_state` and sees that a render has been requested. At
            // that point, the GUI thread will request a redraw, release the lock, re-acquire
            // the lock (due to *winit* event handling), and, once in the redraw event, will set
            // `render_state` to [`RenderState::InProgress`]. Finally, the lock is released
            // again, and this thread continues.
            let mut render_state = render.cvar.wait_while(
                render_state,
                |state| matches!(state, RenderState::Requested),
            )
            .unwrap();
            // Perform the render operation.
            self.gpu.gfx_mut().render();
            // The GUI thread is waiting for `render_state` to change, so we must now notify it.
            *render_state = RenderState::Complete;
            render.cvar.notify_one();
        }
        // Release the `render_state` lock, allowing the GUI thread to acquire it and
        // observe that the render is complete.

        self.issue_vblank();
    }

    fn issue_vblank(&mut self) {
        // Request a V-blank interrupt.
        self.int.vblank.request = Some(Default::default());
    }

    fn cpu(&mut self) -> Cpu {
        self.cpu_state.connect_bus(noctane_cpu::Bus {
            ram: &mut self.banks.ram,
            exp_1: &mut self.banks.exp_1,
            io: noctane_cpu::bus::Io {
                bus: &mut self.bus_cfg,
                dma: &mut self.dma_cfg,
                int: &mut self.int,
                gpu: &mut self.gpu,
                last_gpu_result: &mut self.last_gpu_result,
                post: &mut self.post,
                ram: &mut self.ram_cfg,
                spu: &mut self.spu_cfg,
                spu_voices: &mut self.spu_voices,
                timers: &mut self.timers,
            },
            exp_3: &mut self.banks.exp_3,
            bios: &mut self.banks.bios,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::Core;

    macro_rules! def_write_to_test {
        (
            $mem:expr,
            $ty:ty,
            $map_addr:expr,
            $read_32_name:ident ($($read_32_arg:expr),* $(,)?),
            $write_8_name:ident (),
            $write_16_name:ident (),
            $write_32_name:ident () $(,)?
        ) => {
            const VALUE: u32 = !0;

            fn assert_success(mem: &mut $ty, addr: u32, bit_width: &str) {
                assert_eq!(
                    mem.$read_32_name($map_addr(addr), $($read_32_arg),*),
                    Ok(VALUE),
                    "failed to write {}-bit value",
                    bit_width,
                );
            }

            for addr in (0..32).step_by(4) {
                let _ = $mem.$write_8_name($map_addr(addr + 0), VALUE as u8);
                let _ = $mem.$write_8_name($map_addr(addr + 1), VALUE as u8);
                let _ = $mem.$write_8_name($map_addr(addr + 2), VALUE as u8);
                let _ = $mem.$write_8_name($map_addr(addr + 3), VALUE as u8);
                assert_success($mem, addr, "8");

                let _ = $mem.$write_16_name($map_addr(addr + 0), VALUE as u16);
                let _ = $mem.$write_16_name($map_addr(addr + 2), VALUE as u16);
                assert_success($mem, addr, "16");

                let _ = $mem.$write_32_name($map_addr(addr), VALUE);
                assert_success($mem, addr, "32");
            }
        };
    }

    #[test]
    fn write_to_cpu_memory() {
        let mut core = Core::default();
        let mut cpu = core.cpu();
        let mem = cpu.mem_mut();

        def_write_to_test!(
            mem,
            noctane_cpu::Memory,
            |addr| addr,
            read_data_32(),
            write_data_8(),
            write_data_16(),
            write_data_32(),
        );
    }

    #[test]
    fn write_to_cpu_bus() {
        let mut core = Core::default();
        let mut cpu = core.cpu();
        let bus = cpu.mem_mut().bus_mut();

        def_write_to_test!(
            bus,
            noctane_cpu::Bus,
            |addr| noctane_cpu::mem::Address::from(addr as usize),
            read_32(),
            write_8(),
            write_16(),
            write_32(),
        );
    }
}
