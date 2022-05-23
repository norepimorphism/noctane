#[derive(Default)]
pub struct File {
    /// The program counter.
    pc: u32,
    hi: u32,
    lo: u32,
    /// General-purpose registers. Zero-indexed.
    gprs: [u32; 31]
}

impl File {
    pub fn pc(&self) -> u32 {
        self.pc
    }

    pub fn pc_mut(&mut self) -> &mut u32 {
        &mut self.pc
    }

    pub fn hi(&self) -> u32 {
        self.hi
    }

    pub fn hi_mut(&mut self) -> &mut u32 {
        &mut self.hi
    }

    pub fn lo(&self) -> u32 {
        self.lo
    }

    pub fn lo_mut(&mut self) -> &mut u32 {
        &mut self.lo
    }

    pub fn gpr(&self, index: usize) -> u32 {
        if index == 0 {
            0
        } else {
            self.gprs[index - 1]
        }
    }

    pub fn set_gpr(&mut self, index: usize, value: u32) {
        if index > 0 {
            self.gprs[index - 1] = value;
        }
    }
}
