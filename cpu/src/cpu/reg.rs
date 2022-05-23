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

    pub fn set_pc(&mut self, value: u32) {
        self.pc = value;
    }

    pub fn hi(&self) -> u32 {
        self.hi
    }

    pub fn set_hi(&mut self, value: u32) {
        self.hi = value;
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
