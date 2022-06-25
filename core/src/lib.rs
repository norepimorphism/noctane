// SPDX-License-Identifier: MPL-2.0

use std::time::Instant;

pub use noctane_cpu::Cpu;
pub use noctane_gpu::Gpu;

impl Core {
    pub fn new(gfx: noctane_gpu::gfx::Renderer) -> Self {
        Self {
            banks: Default::default(),
            bus_cfg: Default::default(),
            cpu_state: Default::default(),
            dma_cfg: Default::default(),
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
}

pub struct Core {
    pub banks: Banks,
    pub bus_cfg: noctane_cpu::bus::io::bus::Config,
    pub cpu_state: noctane_cpu::State,
    pub dma_cfg: noctane_cpu::bus::io::dma::Config,
    pub int: noctane_cpu::bus::io::int::Sources,
    pub gpu: Gpu,
    pub last_gpu_result: u32,
    pub last_vblank: Instant,
    pub post: noctane_cpu::bus::io::post::Status,
    pub ram_cfg: noctane_cpu::bus::io::ram::Config,
    pub spu_cfg: noctane_cpu::bus::io::spu::Config,
    pub spu_voices: [noctane_cpu::bus::io::spu_voice::Config; 24],
    pub timers: noctane_cpu::bus::io::Timers,
}

#[derive(Default)]
pub struct Banks {
    pub exp_1: noctane_cpu::bus::Exp1,
    pub exp_3: noctane_cpu::bus::Exp3,
    pub bios: noctane_cpu::bus::Bios,
    pub ram: noctane_cpu::bus::Ram,
}

impl Core {
    pub fn cpu(&mut self) -> Cpu {
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

    pub fn take_vblank(&mut self) -> Option<()> {
        // 60 Hz.
        if self.last_vblank.elapsed().as_nanos() >= 16666667 {
            self.last_vblank = Instant::now();

            Some(())
        } else {
            None
        }
    }

    pub fn issue_vblank(&mut self) {
        // Request a V-blank interrupt.
        self.int.vblank.request = Some(Default::default());
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
