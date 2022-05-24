// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

#![feature(test)]

extern crate test;

use std::{cell::{Ref, RefCell, RefMut}, rc::Rc};

pub use noctane_cpu::Cpu;
pub use noctane_mem::Memory;

impl Core {
    pub fn new() -> Self {
        let mem = Rc::new(RefCell::new(Memory::new()));
        let cpu = Cpu::new(mem.clone());

        Self { cpu, mem }
    }
}

pub struct Core {
    cpu: Cpu,
    mem: Rc<RefCell<Memory>>,
}

impl Core {
    pub fn cpu(&self) -> &Cpu {
        &self.cpu
    }

    pub fn cpu_mut(&mut self) -> &mut Cpu {
        &mut self.cpu
    }

    pub fn mem(&self) -> Ref<Memory> {
        self.mem.borrow()
    }

    pub fn mem_mut(&mut self) -> RefMut<Memory> {
        self.mem.borrow_mut()
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
