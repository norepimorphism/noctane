use std::fmt;

impl Address {
    /// Decomposes a physical address into an instruction cache address.
    pub fn from_phys(mut addr: u32) -> Self {
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
pub struct Address {
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
    pub fn read(
        &mut self,
        addr: &Address,
        read_on_miss: impl FnOnce() -> u32,
    ) -> u32 {
        tracing::debug!("Accessing cache ({:?})", addr);

        match self.access(addr) {
            AccessResult::Hit(value) => value,
            AccessResult::Miss(old) => {
                let value = read_on_miss();
                // Update the cache.
                *old = value;

                value
            }
        }
    }

    fn access<'a>(&'a mut self, addr: &Address) -> AccessResult<'a> {
        let entry = &mut self.entries[addr.entry_idx];

        // When the cache is isolated, all cache accesses are hits.
        if self.is_isolated || entry.test_hit(&addr) {
            tracing::debug!("Cache hit");

            AccessResult::Hit(entry.line[addr.word_idx])
        } else {
            tracing::debug!("Cache miss");

            AccessResult::Miss(&mut entry.line[addr.word_idx])
        }
    }

    pub fn write(
        &mut self,
        addr: &Address,
        value: u32,
        write_on_miss: impl FnOnce(),
    ) {
        tracing::debug!("Updating cache ({:?})", addr);
        self.entries[addr.entry_idx].line[addr.word_idx] = value;
        write_on_miss();
    }
}

enum AccessResult<'a> {
    Hit(u32),
    Miss(&'a mut u32),
}

#[derive(Clone, Copy, Default)]
struct Entry {
    tag: u32,
    line: [u32; 4],
}

impl Entry {
    fn test_hit(&self, addr: &Address) -> bool {
        self.tag == addr.tag
    }
}
