// SPDX-License-Identifier: MPL-2.0

#![feature(slice_as_chunks)]

pub mod bios;
pub mod log;

use instant::Instant;
pub use noctane_cpu::Cpu;
pub use noctane_gpu::Gpu;
use winit::{event_loop::{EventLoop, EventLoopProxy}, window::Window};

use std::sync::{Arc, Condvar, Mutex, MutexGuard};

#[cfg(target_arch = "wasm32")]
pub use wasm_thread as thread;
#[cfg(not(target_arch = "wasm32"))]
pub use std::thread;

impl Core {
    pub async unsafe fn new(game_window: &Window) -> Self {
        // SAFETY: TODO
        let gfx = Self::create_gfx(game_window).await;

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
            spu_cfg: Default::default(),
            spu_voices: Default::default(),
            timers: Default::default(),
        }
    }

    async unsafe fn create_gfx(game_window: &Window) -> noctane_gpu::gfx::Renderer {
        noctane_gpu::gfx::Renderer::new(
            &game_window,
            wgpu_types::Backends::all(),
        )
        .await
        .map(|mut it| {
            let size = game_window.inner_size();
            it.resize(size.width, size.height);

            it
        })
        .expect("failed to create renderer")
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
    pub fn run(
        self,
        event_loop: EventLoop<()>,
        game_window: Window,
        main: impl Fn(Self) + Sync + Send + 'static,
    ) {
        let mut main_thread = Some(thread::spawn(move || main(self)));

        event_loop.run(move |event, _, ctrl_flow| {
            use winit::{
                event::{Event, WindowEvent},
                event_loop::ControlFlow,
            };

            *ctrl_flow = ControlFlow::Wait;

            match event {
                Event::WindowEvent { event: WindowEvent::CloseRequested, .. } => {
                    if let Some(thread) = main_thread.take() {
                        thread.join().expect("main thread panicked");
                    }
                    *ctrl_flow = ControlFlow::Exit;
                }
                _ => {},
            }
        });
    }

    pub fn banks_mut(&mut self) -> &mut Banks {
        &mut self.banks
    }

    pub fn step(&mut self) -> noctane_cpu::instr::Executed {
        if self.take_vblank().is_some() {
            self.render();
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

    fn render(&mut self) {
        self.gpu.gfx_mut().render();
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
