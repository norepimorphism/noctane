// SPDX-License-Identifier: MPL-2.0

//! CPU cache.

use noctane_util::{BitStack as _, BitStackExt as _};

pub mod i;

pub const CTRL_ADDR: u32 = 0xfffe0130;

impl Default for Cache {
    fn default() -> Self {
        Self {
            i: i::Cache::default(),
            i_is_enabled: false,
            // As with other forms of memory, this shall be filled with 'screaming `0xaa`s' by
            // default, This makes it more obvious when uninitialized reads occur.
            d: [0xaa; 0xff],
            d_is_enabled: DualBoolean::default(),
        }
    }
}

/// CPU cache.
#[derive(Clone, Debug)]
pub struct Cache {
    /// The instruction cache (I-cache).
    pub i: i::Cache,
    /// Whether or not the I-cache should be consulted.
    pub i_is_enabled: bool,
    /// The data cache (D-cache).
    ///
    /// In Sony lingo, this is known as the 'scratchpad', and it doesn't function as a traditional
    /// D-cache. Rather, it is mapped to a special memory region and accessed like normal RAM with
    /// the exception that all accesses are intercepted by the CPU.
    pub d: [u32; 0xff],
    /// Whether or not the D-cache should be consulted.
    ///
    /// This is a 'dual boolean', which means it requires two switches to be enabled. See
    /// [`DualBoolean`] for details.
    pub d_is_enabled: DualBoolean,
}

impl Cache {
    pub fn decode_ctrl(&mut self, mut code: u32) {
        // TODO: Unknown (R/W).
        let _ = code.pop_bits(3);
        self.d_is_enabled.primary = code.pop_bool();
        // TODO: Unknown (R/W).
        let _ = code.pop_bits(2);
        // Always zero.
        code.pop_bits(1);
        self.d_is_enabled.secondary = code.pop_bool();
        // TODO: Unknown (R/W).
        let _ = code.pop_bits(1);
        // TODO: Crash (?).
        let _ = code.pop_bits(1);
        // Always zero.
        code.pop_bits(1);
        self.i_is_enabled = code.pop_bool();
        // TODO: Unknown (R/W).
        let _ = code.pop_bits(20);
    }

    pub fn encode_ctrl(&self) -> u32 {
        todo!()
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub struct DualBoolean {
    pub primary: bool,
    pub secondary: bool,
}

impl DualBoolean {
    pub fn get(&self) -> bool {
        self.primary && self.secondary
    }
}
