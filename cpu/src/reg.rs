use std::fmt;

impl Default for File {
    fn default() -> Self {
       let mut this = Self {
           // This is the reset vector.
           // TODO: We should do something more proper in the future, like having a method called
           // `reset` that sets the PC to this.
           pc: 0xbfc0_0000,
           hi: 0,
           lo: 0,
           gprs: [0; 32],
           cprs: [0; 32],
           sr_is_dirty: false,
           pending_cause: None,
       };

       // PRId has a special default.
       this.cprs[cpr::PRID_IDX] = cpr::Prid::default().0;

       this
    }
}

#[derive(Debug)]
pub struct File {
    /// The program counter.
    pc: u32,
    hi: u32,
    lo: u32,
    /// General-purpose registers.
    gprs: [u32; 32],
    /// System coprocessor (COP0) registers.
    cprs: [u32; 32],
    sr_is_dirty: bool,
    pending_cause: Option<cpr::Cause>,
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
        if index == 0 {
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
        if index == cpr::STATUS_IDX {
            self.sr_is_dirty = true;
        }

        self.cprs[index] = value;
    }

    pub fn apply_sr(&mut self) -> Option<u32> {
        if self.sr_is_dirty {
            self.sr_is_dirty = false;

            Some(self.cprs[cpr::STATUS_IDX])
        } else {
            None
        }
    }
}

pub mod cpr {
    use bitfield::bitfield;

    use crate::exc::Exception;

    pub const BAD_VADDR_IDX:    usize = 8;
    pub const STATUS_IDX:       usize = 12;
    pub const CAUSE_IDX:        usize = 13;
    pub const EPC_IDX:          usize = 14;
    pub const PRID_IDX:         usize = 15;

    bitfield! {
        pub struct Status(u32);
        pub ie_c, set_ie_c: 0;
        pub ku_c, set_ku_c: 1;
        pub ie_p, set_ie_p: 2;
        pub ku_p, set_ku_p: 3;
        pub ie_o, set_ie_o: 4;
        pub ku_o, set_ku_o: 5;
        pub im, set_im: 15, 8;
        pub bool, is_c, set_is_c: 16;
        pub bool, sw_c, set_sw_c: 17;
        pub pz, set_pz: 18;
        pub cm, set_cm: 19;
        pub pe, set_pe: 20;
        pub ts, set_ts: 21;
        pub bev, set_bev: 22;
        pub re, set_re: 25;
        pub cu0, set_cu0: 28;
        pub cu1, set_cu1: 29;
        pub cu2, set_cu2: 30;
        pub cu3, set_cu3: 31;
    }

    impl From<Exception> for Cause {
        fn from(exc: Exception) -> Self {
            let mut this = Self(0);
            this.set_exc_code(exc.code);
            // TODO

            this
        }
    }

    bitfield! {
        pub struct Cause(u32);
        impl Debug;
        pub exc_code, set_exc_code: 6, 2;
        pub ip, set_ip: 15, 8;
        pub ce, set_ce: 29, 28;
        pub bd, set_bd: 31;
    }

    impl Default for Prid {
        fn default() -> Self {
            let mut this = Self(0);
            this.set_imp(3);
            // I have no idea what Sony set this to.
            this.set_rev(0xaaaa);

            this
        }
    }

    bitfield! {
        pub struct Prid(u32);
        pub rev, set_rev: 7, 0;
        pub imp, set_imp: 15, 8;
    }
}
