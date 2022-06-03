// SPDX-License-Identifier: MPL-2.0

//! CPU and COP0 registers.

use std::fmt;

impl Default for File {
    fn default() -> Self {
        Self {
            // This is the reset vector.
            // TODO: We should do something more proper in the future, like having a method called
            // `reset` that sets the PC to this.
            pc: 0xbfc0_0000,
            hi: 0,
            lo: 0,
            gprs: [0; 32],
            cprs: [0; 32],
            sr_is_dirty: false,
        }
    }
}

/// The register file.
///
/// Why is it called a 'file', anyway?
#[derive(Debug)]
pub struct File {
    pc: u32,
    hi: u32,
    lo: u32,
    gprs: [u32; 32],
    cprs: [u32; 32],
    sr_is_dirty: bool,
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
        writeln!(f, "BadVaddr: {:08x}", self.cpr(cpr::BAD_VADDR_IDX))?;
        writeln!(f, "SR:       {:08x}", self.cpr(cpr::STATUS_IDX))?;
        writeln!(f, "Cause:    {:08x}", self.cpr(cpr::CAUSE_IDX))?;
        writeln!(f, "EPC:      {:08x}", self.cpr(cpr::EPC_IDX))?;
        writeln!(f, "PRId:     {:08x}", self.cpr(cpr::PRID_IDX))?;

        Ok(())
    }
}

impl File {
    /// The program counter (PC).
    pub fn pc(&self) -> u32 {
        self.pc
    }

    /// A mutable reference to the program counter (PC).
    pub fn pc_mut(&mut self) -> &mut u32 {
        &mut self.pc
    }

    /// The HI register.
    pub fn hi(&self) -> u32 {
        self.hi
    }

    /// A mutable reference to the HI register.
    pub fn hi_mut(&mut self) -> &mut u32 {
        &mut self.hi
    }

    /// The LO register.
    pub fn lo(&self) -> u32 {
        self.lo
    }

    /// A mutable reference to the LO register.
    pub fn lo_mut(&mut self) -> &mut u32 {
        &mut self.lo
    }

    /// Returns the value of the general-purpose register (GPR) identified by the given zero-based
    /// index.
    pub fn gpr(&self, index: usize) -> u32 {
        if index == 0 { 0 } else { self.gprs[index] }
    }

    /// Sets the value of the general purpose register (GPR) identified by the given zero-based
    /// index.
    pub fn set_gpr(&mut self, index: usize, value: u32) {
        self.gprs[index] = value;
    }

    /// Returns the value of the control register identified by the given zero-based index.
    pub fn cpr(&self, index: usize) -> u32 {
        if index == cpr::PRID_IDX {
            // Unlike the other control registers, the PRId is read-only (RM[3-4]). As such, its
            // value is hardcoded here.

            let mut prid = cpr::Prid(0);

            // R3000As supposedly have the 'Imp' value set to 3 (RM[3-4]).
            prid.set_imp(3);
            // I have no idea what Sony set this to. If you know, please file an issue to the
            // Noctane GitHub!
            prid.set_rev(0xaa);

            prid.0
        } else {
            self.cprs[index]
        }
    }

    /// Sets the value of the control register identified by the given zero-based index.
    pub fn set_cpr(&mut self, index: usize, value: u32) {
        if (index == cpr::STATUS_IDX) && (value != self.cprs[cpr::STATUS_IDX]) {
            // When the status register (SR) is modified, we set the following flag to indicate to
            // the CPU that it should update itself accordingly. When the CPU calls the
            // [`Self::altered_sr`] method, this flag is cleared.
            self.sr_is_dirty = true;
        }

        // It is possible to overwrite the PRId register using this, but that's OK as its value is
        // hardcoded, as shown above in [`Self::cpr`].
        self.cprs[index] = value;
    }

    /// Returns the new value of the status register (SR) if it was modified since this method was
    /// last called.
    ///
    /// This method is useful for updating the CPU in response to SR modifications.
    pub fn altered_sr(&mut self) -> Option<u32> {
        if self.sr_is_dirty {
            // Clear this flag so that this method returns `None` if it is called again without
            // modifying the SR.
            self.sr_is_dirty = false;

            Some(self.cprs[cpr::STATUS_IDX])
        } else {
            None
        }
    }
}

pub mod cpr {
    //! Control registers.

    use bitfield::bitfield;

    use crate::exc::Exception;

    /// The index of the BadVaddr register.
    pub const BAD_VADDR_IDX: usize = 8;
    /// The index of the status register (SR).
    pub const STATUS_IDX: usize = 12;
    /// The index of the Cause register.
    pub const CAUSE_IDX: usize = 13;
    /// The index of the EPC register.
    pub const EPC_IDX: usize = 14;
    /// The index of the PRId register.
    pub const PRID_IDX: usize = 15;

    bitfield! {
        /// The status register (SR).
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
        /// The Cause register.
        pub struct Cause(u32);
        impl Debug;
        pub exc_code, set_exc_code: 6, 2;
        pub ip, set_ip: 15, 8;
        pub ce, set_ce: 29, 28;
        pub bd, set_bd: 31;
    }

    bitfield! {
        /// The PRId register.
        pub struct Prid(u32);
        pub rev, set_rev: 7, 0;
        pub imp, set_imp: 15, 8;
    }
}
