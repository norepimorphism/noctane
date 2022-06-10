// SPDX-License-Identifier: MPL-2.0

//! CPU cache.

pub mod i;

impl Default for Cache {
    fn default() -> Self {
        Self {
            i: i::Cache::default(),
            // As with other forms of memory, this shall be filled with 'screaming `0xaa`s' by
            // default, This makes it more obvious when uninitialized reads occur.
            d: [0xaa; 0xff],
        }
    }
}

/// CPU cache.
pub struct Cache {
    /// The instruction cache (I-cache).
    pub i: i::Cache,
    /// The data cache (D-cache).
    ///
    /// In Sony lingo, this is known as the 'scratchpad', and it doesn't function as a traditional
    /// D-cache. Rather, it is mapped to a special memory region and accessed like normal RAM with
    /// the exception that all accesses are intercepted by the CPU.
    pub d: [u32; 0xff],
}
