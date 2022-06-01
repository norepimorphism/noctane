use std::fmt;

use crate::mem;
use super::Address;

impl Default for Entry {
    fn default() -> Self {
        Self {
            // Because cache entries, by default, are not necessarily up-to-date with memory, they
            // are, by default, invalid.
            is_valid: false,
            tag: 0,
            line: [0; 4],
        }
    }
}

#[derive(Clone, Copy)]
pub struct Entry {
    is_valid: bool,
    tag: usize,
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
            write!(f, " {:08x}", word)?;
        }

        Ok(())
    }
}

impl Entry {
    pub(super) fn read(
        &mut self,
        addr: Address,
        fetch_line: impl FnOnce(mem::Address) -> [u32; 4],
    ) -> u32 {
        if self.is_valid && self.test_hit(addr) {
            // We are a valid copy of the data at `addr`.
            tracing::trace!("Cache hit! (addr={})", addr);
        } else {
            // Sadge.
            tracing::trace!("Cache miss. (addr={})", addr);

            // Uh oh! We're not up-to-date with the CPU bus. Accordingly, we will refresh ourselves
            // by calling `fetch_line`.
            let line = fetch_line(mem::Address::from(addr.working & !0b1111));
            self.line.copy_from_slice(&line);

            // We now contain a valid copy of the data pointed to by `addr`.
            self.is_valid = true;
            self.tag = addr.tag;
        }

        self.line[addr.word_idx]
    }

    pub(super) fn write(&mut self, addr: Address, value: u32) {
        self.line[addr.word_idx] = value;

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

    pub(super) fn write_partial(
        &mut self,
        addr: Address,
        modify_word: impl FnOnce(&mut u32),
    ) {
        if self.is_valid && self.test_hit(addr) {
            let word = &mut self.line[addr.word_idx];
            modify_word(word);
            // As with [`Self::write`], we can assume that the CPU bus was writen-through to, so
            // this entry is still valid.
        } else {
            // Unlike normal 32-bit writes, partial-word writes with addresses that don't match the
            // tag of this entry are simply ignored.
        }
    }

    /// Manually invalidates this entry.
    pub fn invalidate(&mut self) {
        self.is_valid = false;
    }

    /// Determines if this entry is a copy of the data contained at the given address.
    fn test_hit(&self, addr: Address) -> bool {
        self.tag == addr.tag
    }
}
