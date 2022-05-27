use std::fmt;

impl Address {
    /// Decomposes a physical address into an instruction cache address.
    fn from_phys(mut addr: u32) -> Self {
        // We are only addressing words, so we can silently ignore the byte index.
        addr >>= 2;

        // One I-cache line contains four words, so we must store the position of the word to read.
        let word_idx = (addr & 0b11) as usize;
        addr >>= 2;

        // The I-cache is 4096 bytes. Given a line size of 4 words, or 4 * `sizeof(u32)` = 16 bytes,
        // there are 4096 / 16 = 256 D-cache lines, so line indices are perfectly represented by a
        // single byte.
        let entry_idx = (addr & 0xff) as usize;
        addr >>= 8;

        // The tag contains the remaining bits.
        let tag = addr;

        Self { tag, entry_idx, word_idx }
    }
}

#[derive(Clone)]
struct Address {
    tag: u32,
    entry_idx: usize,
    word_idx: usize,
}

impl fmt::Debug for Address {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:x}:{:x}:{}", self.tag, self.entry_idx, self.word_idx)
    }
}

impl Default for InstrCache {
    fn default() -> Self {
        Self {
            entries: [Entry::default(); 256],
            is_isolated: false,
        }
    }
}

pub struct InstrCache {
    entries: [Entry; 256],
    is_isolated: bool,
}

impl InstrCache {
    pub fn read_32<E>(
        &mut self,
        addr: u32,
        read_on_miss: impl FnOnce() -> Result<u32, E>,
    ) -> Result<u32, E> {
        self._read_32(&Address::from_phys(addr), read_on_miss)
    }

    fn _read_32<E>(
        &mut self,
        addr: &Address,
        read_on_miss: impl FnOnce() -> Result<u32, E>,
    ) -> Result<u32, E> {
        match self.access(addr) {
            AccessResult::Hit(value) => Ok(value),
            AccessResult::Miss(old) => {
                let value = read_on_miss()?;
                // Update the cache.
                *old = value;

                Ok(value)
            }
        }
    }

    fn access<'a>(&'a mut self, addr: &Address) -> AccessResult<'a> {
        let entry = &mut self.entries[addr.entry_idx];

        // When the cache is isolated, all cache accesses are hits.
        if self.is_isolated || entry.test_hit(&addr) {
            AccessResult::Hit(entry.line[addr.word_idx])
        } else {
            AccessResult::Miss(&mut entry.line[addr.word_idx])
        }
    }

    pub fn write_32<E>(
        &mut self,
        addr: u32,
        value: u32,
        write_on_miss: impl FnOnce() -> Result<(), E>,
    ) -> Result<(), E> {
        self._write_32(&Address::from_phys(addr), value, write_on_miss)
    }

    fn _write_32<E>(
        &mut self,
        addr: &Address,
        value: u32,
        write_on_miss: impl FnOnce()-> Result<(), E>,
    ) -> Result<(), E> {
        // tracing::debug!("Updating cache ({:?})", addr);
        self.entries[addr.entry_idx].line[addr.word_idx] = value;

        write_on_miss()
    }
}

enum AccessResult<'a> {
    Hit(u32),
    Miss(&'a mut u32),
}

impl Default for Entry {
    fn default() -> Self {
        Self {
            tag: !0,
            line: [0; 4],
        }
    }
}

#[derive(Clone, Copy)]
struct Entry {
    tag: u32,
    line: [u32; 4],
}

impl Entry {
    fn test_hit(&self, addr: &Address) -> bool {
        self.tag == addr.tag
    }
}
