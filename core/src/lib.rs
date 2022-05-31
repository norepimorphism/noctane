// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

pub use noctane_cpu::Cpu;
pub use noctane_gpu::Gpu;

#[derive(Default)]
pub struct Core {
    cpu_state: noctane_cpu::State,
    gpu: Gpu,
    banks: Banks,
}

impl Core {
    pub fn cpu(&mut self) -> Cpu {
        self.cpu_state.connect_bus(noctane_cpu::Bus {
            main_ram: &mut self.banks.main_ram,
            exp_1: &mut self.banks.exp_1,
            io: noctane_cpu::bus::Io {
                gpu: &mut self.gpu,
            },
            exp_3: &mut self.banks.exp_3,
            bios: &mut self.banks.bios,
        })
    }

    pub fn banks(&self) -> &Banks {
        &self.banks
    }

    pub fn banks_mut(&mut self) -> &mut Banks {
        &mut self.banks
    }
}

#[derive(Default)]
pub struct Banks {
    pub main_ram: noctane_cpu::bus::MainRam,
    pub exp_1: noctane_cpu::bus::Exp1,
    pub exp_3: noctane_cpu::bus::Exp3,
    pub bios: noctane_cpu::bus::Bios,
}

#[cfg(test)]
mod tests {
    use std::ops::DerefMut as _;

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
    fn write_to_mmu() {
        let mut core = Core::default();
        let mut cpu = core.cpu();
        let mmu = cpu.mmu_mut();

        def_write_to_test!(
            mmu,
            noctane_cpu::Mmu,
            |addr| addr,
            read_virt_32(),
            write_virt_8(),
            write_virt_16(),
            write_virt_32(),
        );
    }

    #[test]
    fn write_to_cpu_memory() {
        let mut core = Core::default();
        let mut cpu = core.cpu();
        let mem = cpu.mmu_mut().deref_mut();

        def_write_to_test!(
            mem,
            noctane_cpu::Memory,
            |addr| addr,
            read_32(),
            write_8(),
            write_16(),
            write_32(),
        );
    }

    #[test]
    fn write_to_cpu_bus() {
        let mut core = Core::default();
        let mut cpu = core.cpu();
        let bus = cpu.mmu_mut().deref_mut().bus_mut();

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
