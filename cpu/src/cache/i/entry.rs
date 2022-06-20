// SPDX-License-Identifier: MPL-2.0

//! An I-cache entry.

use std::fmt;

use super::Address;
use crate::mem;

impl Default for Entry {
    fn default() -> Self {
        Self {
            // Because cache entries, by default, are not necessarily up-to-date with memory, they
            // are, by default, invalid.
            is_valid: false,
            tag: 0,
            line: [0xaa; 4],
        }
    }
}

/// An I-cache entry.
#[derive(Clone, Copy, Debug)]
pub struct Entry {
    /// Whether or not the line contained within this entry is a valid copy of the data pointed to
    /// by the tag and this entry's location in cache.
    is_valid: bool,
    /// The topmost bits of the address that this entry represents.
    tag: usize,
    /// The data contained within this entry.
    line: [u32; 4],
}

impl fmt::Display for Entry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.is_valid {
            write!(f, "! ")?;
        } else {
            write!(f, "  ")?;
        }

        write!(f, "({:05x})", self.tag)?;
        for word in self.line {
            // The words contained within this line have already been byte-swapped on big-endian
            // hosts, so we need to swap the bytes again such that they are native-endian.
            write!(f, " {:08x}", word.to_le())?;
        }

        Ok(())
    }
}

impl Entry {
    /// Reads a word from this entry.
    ///
    /// As this entry contains four words, the precise word returned is selected by the `addr`
    /// argument. If a miss occurs, the `fetch_line` function is invoked, and this entry is updated
    /// accordingly.
    pub(super) fn read(
        &mut self,
        addr: Address,
        fetch_line: impl FnOnce(mem::Address) -> Result<[u32; 4], ()>,
    ) -> Result<u32, ()> {
        if self.is_valid && self.test_hit(addr) {
            // We are a valid copy of the data at `addr`.
            tracing::trace!("Cache hit! (addr={})", addr);
        } else {
            // Sadge.
            tracing::trace!("Cache miss. (addr={})", addr);

            // We're not up-to-date with the CPU bus. Accordingly, we will refresh ourselves by
            // calling `fetch_line`.
            // Note: For this to be correct, `fetch_line` must return native-endian words; that is,
            // on big-endian hosts, they have not yet been byte-swapped.
            let mut line = fetch_line(mem::Address::from(addr.init & !0b1111))?;
            // Now, we byte-swap `line`.
            for word in line.iter_mut() {
                *word = word.to_le();
            }
            self.line.copy_from_slice(&line);

            // We now contain a valid copy of the data pointed to by `addr`.
            self.is_valid = true;
            self.tag = addr.tag;
        }

        Ok(self.line[addr.word_idx])
    }

    /// Writes to a word in this entry.
    ///
    /// As this entry contains four words, the precise word returned is selected by the `addr`
    /// argument.
    pub(super) fn write(&mut self, addr: Address, value: u32) {
        self.line[addr.word_idx] = value.to_le();

        if !self.test_hit(addr) {
            // We just attempted to write to a cache entry containing data that doesn't belong to
            // the given address. The data contained within this entry is still valid for its
            // current address, so there should be no need to invalidate this entry. However,
            // the IDT R3000 reference manual indicates that "the cache is always updated (possibly
            // discarding data from a previously cached location)" (p. 5-2). Therefore, we will go
            // ahead and invalidate this entry anyway.
            self.is_valid = false;
            // Now, to indicate that the data here is specifically an outdated version of the given
            // address, we update the tag.
            self.tag = addr.tag;
        }
    }

    /// Manually invalidates this entry.
    pub fn invalidate(&mut self) {
        self.is_valid = false;
    }

    /// Determines if this entry is a copy (valid or not) of the data contained at the given
    /// address.
    fn test_hit(&self, addr: Address) -> bool {
        self.tag == addr.tag
    }
}
