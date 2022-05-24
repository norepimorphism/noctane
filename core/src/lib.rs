// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

#![feature(test)]

extern crate test;

use std::{cell::{Ref, RefCell, RefMut}, rc::Rc};

pub use noctane_cpu::Cpu;

impl Core {
    pub fn new() -> Self {
        let cpu = Cpu::new();

        Self { cpu }
    }
}

pub struct Core {
    cpu: Cpu,
}

impl Core {
    pub fn cpu(&self) -> &Cpu {
        &self.cpu
    }

    pub fn cpu_mut(&mut self) -> &mut Cpu {
        &mut self.cpu
    }
}

#[cfg(test)]
mod tests {
    use test::Bencher;

    use super::*;

    #[bench]
    fn bench_cpu_execute_next_instr(b: &mut Bencher) {
        let mut core = Core::new();
        let cpu = core.cpu_mut();

        b.iter(|| {
            cpu.execute_next_instr();
        })
    }
}
