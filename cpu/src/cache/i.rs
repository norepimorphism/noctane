use std::fmt;

use crate::mem;

impl From<mem::Address> for Address {
    /// Decomposes a physical address into an instruction cache address.
    fn from(it: mem::Address) -> Self {
        let mut addr = it.init;

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

        Self { working: it.working, tag, entry_idx, word_idx }
    }
}

#[derive(Clone)]
struct Address {
    working: usize,
    tag: usize,
    entry_idx: usize,
    word_idx: usize,
}

impl fmt::Debug for Address {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:x}:{:x}:{}", self.tag, self.entry_idx, self.word_idx)
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

pub struct Cache {
    entries: [Entry; 256],
    is_isolated: bool,
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
    pub fn is_isolated(&self) -> bool {
        self.is_isolated
    }

    pub fn read_8<E>(
        &mut self,
        addr: mem::Address,
        fetch_line: impl FnOnce(mem::Address) -> Result<[u32; 4], E>,
    ) -> Result<u8, E> {
        self.read(addr, fetch_line, |word| addr.index_byte_in_word(word))
    }

    pub fn read_16<E>(
        &mut self,
        addr: mem::Address,
        fetch_line: impl FnOnce(mem::Address) -> Result<[u32; 4], E>,
    ) -> Result<u16, E> {
        self.read(addr, fetch_line, |word| addr.index_halfword_in_word(word))
    }

    pub fn read_32<E>(
        &mut self,
        addr: mem::Address,
        fetch_line: impl FnOnce(mem::Address) -> Result<[u32; 4], E>,
    ) -> Result<u32, E> {
        self.read(addr, fetch_line, |word| word)
    }

    fn read<T, E>(
        &mut self,
        addr: mem::Address,
        fetch_line: impl FnOnce(mem::Address) -> Result<[u32; 4], E>,
        extract: impl FnOnce(u32) -> T,
    ) -> Result<T, E> {
        let addr = Address::from(addr);

        let entry = &mut self.entries[addr.entry_idx];

        // When the cache is isolated, all cache accesses are hits.
        if self.is_isolated || entry.test_hit(&addr) {
            tracing::trace!("Cache hit! (addr={:?})", addr);
        } else {
            tracing::trace!("Cache miss... (addr={:?})", addr);
            entry.tag = addr.tag;

            let line = fetch_line(mem::Address::from(addr.working & !0b1111))?;
            for i in 0..4 {
                entry.line[i] = line[i].to_be_bytes();
            }
        };

        Ok(extract(u32::from_be_bytes(entry.line[addr.word_idx])))
    }

    pub fn write_8(
        &mut self,
        mem_addr: mem::Address,
        value: u8,
    ) {
        let addr = Address::from(mem_addr);

        let entry = &mut self.entries[addr.entry_idx];
        if entry.test_hit(&addr) {
            tracing::trace!("Updating cache (addr={:?})", addr);
            entry.line[addr.word_idx][mem_addr.byte_idx] = value;
        }
    }

    pub fn write_16(
        &mut self,
        mem_addr: mem::Address,
        value: u16,
    ) {
        let addr = Address::from(mem_addr);

        let entry = &mut self.entries[addr.entry_idx];
        if entry.test_hit(&addr) {
            tracing::trace!("Updating cache (addr={:?})", addr);
            entry
                .line[addr.word_idx]
                .as_chunks_mut::<2>()
                .0
                [mem_addr.halfword_idx] = value.to_be_bytes();
        }
    }

    pub fn write_32(
        &mut self,
        mem_addr: mem::Address,
        value: u32,
    ) {
        let addr = Address::from(mem_addr);
        tracing::trace!("Updating cache (addr={:?})", addr);

        let entry = &mut self.entries[addr.entry_idx];
        entry.tag = addr.tag;
        entry.line[addr.word_idx] = value.to_be_bytes();
    }
}

impl Default for Entry {
    fn default() -> Self {
        Self {
            tag: 0,
            line: [[0; 4]; 4],
        }
    }
}

#[derive(Clone, Copy)]
struct Entry {
    tag: usize,
    line: [[u8; 4]; 4],
}

impl fmt::Display for Entry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({:05x})", self.tag)?;
        for word in self.line {
            write!(f, " {:08x}", u32::from_be_bytes(word))?;
        }

        Ok(())
    }
}

impl Entry {
    fn test_hit(&self, addr: &Address) -> bool {
        self.tag == addr.tag
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn gen_test_addrs() -> impl Iterator<Item = mem::Address> {
        (0x0000_00000..=0xffff_ffff).step_by(2048).map(mem::Address::from)
    }

    macro_rules! def_read_cache_fn {
        ($fn_name:ident, $ty:ty, $read_name:ident, $write_name:ident $(,)?) => {
            #[test]
            fn $fn_name() {
                let mut cache = Cache::default();
                for addr in gen_test_addrs() {
                    let written: u8 = rand::random();
                    let written: u32 = u32::from_be_bytes([written; 4]);
                    cache.$write_name(addr, written as $ty);

                    let read = cache.$read_name(
                        addr,
                        |_| -> Result<_, ()> { Ok([written; 4]) },
                    )
                    .unwrap();

                    assert_eq!(
                        written as $ty,
                        read,
                        "failed to read from {:#010x} ({:?})",
                        addr.init,
                        Address::from(addr),
                    );
                }
            }
        };
    }

    def_read_cache_fn!(read_cache_8, u8, read_8, write_8);
    def_read_cache_fn!(read_cache_16, u16, read_16, write_16);
    def_read_cache_fn!(read_cache_32, u32, read_32, write_32);
}
