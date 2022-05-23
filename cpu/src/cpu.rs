use std::{cell::RefCell, rc::Rc};

mod pipeline;
mod reg;

use noctane_mem::Memory;

pub use pipeline::Pipeline;

impl Cpu {
    pub fn new(
        mem: Rc<RefCell<Memory>>,
    ) -> Self {
        Self {
            mem,
            pipe: Pipeline::default(),
            reg: reg::File::default(),
        }
    }
}

pub struct Cpu {
    mem: Rc<RefCell<Memory>>,
    pipe: Pipeline,
    reg: reg::File,
}

impl Cpu {
    pub fn read_mem(&mut self, addr: u32) -> u32 {
        self.mem.borrow_mut().read(addr)
    }

    pub fn write_mem(&mut self, addr: u32, value: u32) {
        self.mem.borrow_mut().write(addr, value);
    }

    pub fn reg(&self) -> &reg::File {
        &self.reg
    }

    pub fn reg_mut(&mut self) -> &mut reg::File {
        &mut self.reg
    }

    pub fn pipeline(&self) -> &Pipeline {
        &self.pipe
    }

    pub fn pipeline_mut(&mut self) -> &mut Pipeline {
        &mut self.pipe
    }
}
