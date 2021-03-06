// SPDX-License-Identifier: MPL-2.0

//! The instruction cache (I-cache).

mod entry;

use std::fmt;

use entry::Entry;

use crate::mem;

impl From<mem::Address> for Address {
    /// Decomposes a physical address into an instruction cache address.
    fn from(it: mem::Address) -> Self {
        // The I-cache operates on physical addresses, so we must use the `working` field, which is
        // assumed, at this point, to contain a physical address.
        let mut addr = it.working;

        // We are only addressing words, so we can silently ignore the byte index.
        addr >>= 2;

        // One I-cache line contains four words, so we must store the position of the word to read.
        let word_idx = addr & 0b11;
        addr >>= 2;

        // The I-cache is 4096 bytes. Given a line size of 4 words, or 4 * `sizeof(u32)` = 16 bytes,
        // there are 4096 / 16 = 256 D-cache lines, so line indices are perfectly represented by a
        // single byte.
        let entry_idx = addr & 0xff;
        addr >>= 8;

        // The tag contains the remaining bits.
        let tag = addr;

        Self {
            // As mentioned in [`Address::init`], the purpose of this field is to address the CPU
            // bus for line fetches. As such, we need the working address, which is a physical
            // address.
            init: it.working,
            tag,
            entry_idx,
            word_idx,
        }
    }
}

/// Indexes a [`Cache`].
#[derive(Clone, Copy)]
struct Address {
    /// The original physical address.
    ///
    /// This field has only one purpose, which is to address the CPU bus in the event that a cache
    /// miss occurs and a line must be fetched.
    init: usize,
    /// The topmost bits of [`init`].
    ///
    /// [`init`]: Self::init
    tag: usize,
    /// The index of the entry within a [`Cache`] to be accessed.
    ///
    /// This value may between 0 and 255, inclusive.
    entry_idx: usize,
    /// The index of the word within the cache line of the entry represented by [`entry_idx`] to be
    /// accessed.
    ///
    /// This value may be 0, 1, 2, or 3.
    ///
    /// [`entry_idx`]: Self::entry_idx
    word_idx: usize,
}

impl fmt::Display for Address {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{:05x}:{:02x}:{:1}",
            self.tag, self.entry_idx, self.word_idx
        )
    }
}

impl Default for Cache {
    fn default() -> Self {
        Self {
            entries: [Entry::default(); 256],
            is_isolated: false,
        }
    }
}

/// An instruction cache (I-cache).
///
/// This cache contains 256 entries of four 32-bit words each. [Addresses] passed to this cache
/// are at the physical level; that is, the [`working`] field represents a physical address.
///
/// # Isolation
///
/// When the I-cache is isolated, a special mechanism is enabled in which all writes&mdash;partial
/// writes as well as full 32-bit stores&mdash;do not cause data to be written, but rather
/// invalidate the cache line of the entry that they access. This feature is useful, for example,
/// when copying instructions into an uncached address region. Because the writes are not observed
/// by the I-cache, if the cache contains any entries that point to the memory locations of the
/// instructions that were just written, those entries have become invalid. The cache, of course,
/// doesn't know this yet, so it is the programmer's responsibility to manually invalidate those
/// cache entries.
///
/// [Addresses]: mem::Address
/// [`working`]: mem::Address::working
#[derive(Clone, Debug)]
pub struct Cache {
    is_isolated: bool,
    entries: [Entry; 256],
}

impl fmt::Display for Cache {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, entry) in self.entries.iter().enumerate() {
            writeln!(f, "{:03x}   {}", i * 16, entry)?;
        }

        Ok(())
    }
}

impl Cache {
    /// Determines if this cache is isolated.
    ///
    /// See the [`Cache`] documentation for the effects of isolation.
    pub fn is_isolated(&self) -> bool {
        self.is_isolated
    }

    /// Sets whether or not the cache is isolated.
    ///
    /// See the [`Cache`] documentation for the effects of isolation.
    pub fn set_isolated(&mut self, value: bool) {
        if self.is_isolated != value {
            if value {
                tracing::info!("Isolated I-cache");
            } else {
                tracing::info!("Un-isolated I-cache");
            }
        }

        self.is_isolated = value;
    }

    /// Reads a 32-bit value from the given address within this cache.
    ///
    /// If a cache miss occurs, a cache line is read from the `fetch_line` argument, which is
    /// expected to return either an array of four native-endian words, or an error, in which case
    /// this method returns that error.
    pub fn read(
        &mut self,
        addr: mem::Address,
        fetch_line: impl FnOnce(mem::Address) -> Result<[u32; 4], ()>,
    ) -> Result<u32, ()> {
        let addr = Address::from(addr);
        let entry = &mut self.entries[addr.entry_idx];

        entry
            .read(addr, fetch_line)
            .map(|word| {
                // As the data contained within `word` is little-endian, we should use
                // [`u32::to_le`] to convert to native-endian first.
                word.to_le()
            })
    }

    /// Writes a 32-bit value to the given address within this cache.
    pub fn write(&mut self, addr: mem::Address, value: u32) {
        let addr = Address::from(addr);
        let entry = &mut self.entries[addr.entry_idx];
        entry.write(addr, value);

        if self.is_isolated {
            // As a special mechanism, isolated writes to the I-cache invalidate the corresponding
            // entries.
            entry.invalidate();
        }
    }

    pub fn entry_for(&self, addr: mem::Address) -> &Entry {
        &self.entries[Address::from(addr).entry_idx]
    }

    pub fn entry_mut_for(&mut self, addr: mem::Address) -> &mut Entry {
        &mut self.entries[Address::from(addr).entry_idx]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Generates a sequence of test addresses.
    fn gen_test_addrs() -> impl Iterator<Item = mem::Address> {
        (0x0000_00000..=0xffff_ffff)
            .step_by(0x800)
            .map(mem::Address::from)
    }

    #[test]
    fn read_far() {
        let mut cache = Cache::default();
        for addr in gen_test_addrs() {
            // First, we generate a random byte. This is an attempt to see that, over many
            // cache accesses, many different values can be successfully written and read
            // back.
            let written: u8 = rand::random();

            // Then, we construct a 32-bit value from mirrors of that random byte. The
            // purpose of this is to generate a suitable value for any of the write
            // functions that this macro is called with.
            let written: u32 = u32::from_be_bytes([written; 4]);

            // Then, we perform the actual write. We may have to chop off a few bits.
            cache.write(addr, written);

            // Because [`Cache`] is, by default, not isolated, the previous write was
            // accepted. However, the data within the cache was not actually 'updated';
            // rather, as this was the first time writing to that cache address, and only
            // one word was written, the cache doesn't yet know what the other words should
            // be, so it simply marked the entry as invalid. We should expect that a cache
            // read will request to fetch the full line from memory.
            let read = cache.read(addr, |_| Ok([written; 4]));

            assert_eq!(
                Ok(written),
                read,
                "failed to read from {:#010x} ({})",
                addr.init,
                Address::from(addr),
            );
        }
    }

    #[test]
    fn read_near() {
        let mut cache = Cache::default();
        for addr in gen_test_addrs() {
            // We will attempt something very similar to that in [`read_far`],
            // but instead of such far-spaced addresses, we will use ones closer together---
            // within the same cache line, even.

            let written: u8 = rand::random();
            let written: u32 = u32::from_be_bytes([written; 4]);

            // First, we perform a normal write. The cache entry is marked as invalid.
            cache.write(addr, written);
            // Then, we read. The cache line is fetched from memory, and the cache entry is
            // marked as valid.
            cache.read(addr, |_| Ok([written; 4])).unwrap();
            // Then, we write again, but to the next word in the cache line. We will use a
            // different value for `written` so that we don't get a false positive as the
            // entire cache line was filled with `written`. In this case, inverting
            // `written` works.
            let next_addr = mem::Address::from(addr.init.wrapping_add(4));
            cache.write(next_addr, !written);

            // Because the cache entry was marked as valid, the new write should've updated
            // its data. Let's see.
            let read = cache.read(next_addr, |_| {
                // If the read requests to fetch the line again, then something has gone
                // wrong.
                panic!("fetched cache line twice");
            });

            assert_eq!(
                // Remember: the second word is an inverted `written`.
                Ok(!written),
                read,
                "failed to read from {:#010x} ({})",
                addr.init,
                Address::from(addr),
            );
        }
    }
}
