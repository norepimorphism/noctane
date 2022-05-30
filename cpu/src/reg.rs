use std::{fmt, intrinsics::unlikely};

#[derive(Debug, Default)]
pub struct File {
    /// The program counter.
    pc: u32,
    hi: u32,
    lo: u32,
    /// General-purpose registers. Zero-indexed.
    gprs: [u32; 32],
    cprs: [u32; 32],
}

impl fmt::Display for File {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "pc: {:08x} r16:{:08x}", self.pc, self.gprs[15])?;
        writeln!(f, "r1: {:08x} r17:{:08x}", self.gprs[1], self.gprs[17])?;
        writeln!(f, "r2: {:08x} r18:{:08x}", self.gprs[2], self.gprs[18])?;
        writeln!(f, "r3: {:08x} r19:{:08x}", self.gprs[3], self.gprs[19])?;
        writeln!(f, "r4: {:08x} r20:{:08x}", self.gprs[4], self.gprs[20])?;
        writeln!(f, "r5: {:08x} r21:{:08x}", self.gprs[5], self.gprs[21])?;
        writeln!(f, "r6: {:08x} r22:{:08x}", self.gprs[6], self.gprs[22])?;
        writeln!(f, "r7: {:08x} r23:{:08x}", self.gprs[7], self.gprs[23])?;
        writeln!(f, "r8: {:08x} r24:{:08x}", self.gprs[8], self.gprs[24])?;
        writeln!(f, "r9: {:08x} r25:{:08x}", self.gprs[9], self.gprs[25])?;
        writeln!(f, "r10:{:08x} r26:{:08x}", self.gprs[10], self.gprs[26])?;
        writeln!(f, "r11:{:08x} r27:{:08x}", self.gprs[11], self.gprs[27])?;
        writeln!(f, "r12:{:08x} r28:{:08x}", self.gprs[12], self.gprs[28])?;
        writeln!(f, "r13:{:08x} r29:{:08x}", self.gprs[13], self.gprs[29])?;
        writeln!(f, "r14:{:08x} r30:{:08x}", self.gprs[14], self.gprs[30])?;
        writeln!(f, "r15:{:08x} r31:{:08x}", self.gprs[15], self.gprs[31])?;
        writeln!(f, "hi: {:08x} lo: {:08x}", self.hi, self.lo)?;
        // TODO: Write control registers.

        Ok(())
    }
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
        if unlikely(index == 0) {
            0
        } else {
            self.gprs[index]
        }
    }

    pub fn set_gpr(&mut self, index: usize, value: u32) {
        self.gprs[index] = value;
    }

    pub fn cpr(&self, index: usize) -> u32 {
        self.cprs[index]
    }

    pub fn set_cpr(&mut self, index: usize, value: u32) {
        self.cprs[index] = value;
    }
}
