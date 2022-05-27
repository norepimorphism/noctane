// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

#![feature(test)]

extern crate test;

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
            gpu: &mut self.gpu,
            exp_2: &mut self.banks.exp_2,
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
    pub main_ram: Box<noctane_cpu::bus::MainRam>,
    pub exp_1: Box<noctane_cpu::bus::Exp1>,
    pub exp_2: Box<noctane_cpu::bus::Exp2>,
    pub exp_3: Box<noctane_cpu::bus::Exp3>,
    pub bios: Box<noctane_cpu::bus::Bios>,
}
