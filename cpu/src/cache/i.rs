mod entry;

use std::fmt;

use crate::mem;

use entry::Entry;

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

#[derive(Clone, Copy)]
struct Address {
    working: usize,
    tag: usize,
    entry_idx: usize,
    word_idx: usize,
}

impl fmt::Display for Address {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:05x}:{:02x}:{:1}", self.tag, self.entry_idx, self.word_idx)
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
}

macro_rules! def_read_fn {
    ($fn_name:ident() -> $ty:ty { $extract:expr }) => {
        pub fn $fn_name(
            &mut self,
            addr: mem::Address,
            fetch_line: impl FnOnce(mem::Address) -> [u32; 4],
        ) -> $ty {
            self.read(addr.into(), fetch_line, |word| $extract(word, addr))
        }
    };
}

impl Cache {
    def_read_fn!(read_8() -> u8 { |word, addr: mem::Address| addr.index_byte_in_word(word) });
    def_read_fn!(read_16() -> u16 { |word, addr: mem::Address| addr.index_halfword_in_word(word) });
    def_read_fn!(read_32() -> u32 { |word, _| word });

    fn read<T>(
        &mut self,
        addr: Address,
        fetch_line: impl FnOnce(mem::Address) -> [u32; 4],
        extract: impl FnOnce(u32) -> T,
    ) -> T {
        let entry = &mut self.entries[addr.entry_idx];

        extract(entry.read(addr, fetch_line))
    }

    pub fn write_8(
        &mut self,
        addr: mem::Address,
        value: u8,
    ) {
        self.write_partial(
            addr.into(),
            |mut word| {
                word[addr.byte_idx] = value;

                word
            },
        )
    }

    pub fn write_16(
        &mut self,
        addr: mem::Address,
        value: u16,
    ) {
        self.write_partial(
            addr.into(),
            |mut word| {
                word
                    .as_chunks_mut::<2>()
                    .0
                    [addr.halfword_idx] = value.to_be_bytes();

                word
            },
        )
    }

    fn write_partial(
        &mut self,
        addr: Address,
        map_word: impl FnOnce([u8; 4]) -> [u8; 4],
    ) {
        let entry = &mut self.entries[addr.entry_idx];
        if self.is_isolated {
            tracing::trace!(
                "Invalidating cache line due to isolated partial write... (addr={})",
                addr,
            );

            // The IDT R3000 reference manual states (p. 5-3):
            //     As a special mechanism, with the D-cache isolated, a partial-word write will
            //     invalidate the appropriate cache line.
            //
            // If we assume the I-cache essentially fulfills the role of the D-cache as well in the
            // PSX, then an isolated partial-write automatically invalidates this cache entry.
            entry.invalidate();
        } else {
            entry.write_partial(
                addr,
                |word| {
                    *word = u32::from_be_bytes(map_word(word.to_be_bytes()));
                },
            );
        }
    }

    pub fn write_32(
        &mut self,
        addr: mem::Address,
        value: u32,
    ) {
        let addr = Address::from(addr);
        let entry = &mut self.entries[addr.entry_idx];
        entry.write(addr, value);

        if self.is_isolated {
            entry.invalidate();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn gen_test_addrs() -> impl Iterator<Item = mem::Address> {
        (0x0000_00000..=0xffff_ffff).step_by(0x800).map(mem::Address::from)
    }

    macro_rules! def_read_cache_far_fn {
        ($fn_name:ident, $ty:ty, $read_name:ident, $write_name:ident $(,)?) => {
            #[test]
            fn $fn_name() {
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
                    cache.$write_name(addr, written as $ty);

                    // Because [`Cache`] is, by default, not isolated, the previous write was
                    // accepted. However, the data within the cache was not actually 'updated';
                    // rather, as this was the first time writing to that cache address, and only
                    // one word was written, the cache doesn't yet know what the other words should
                    // be, so it simply marked the entry as invalid. We should expect that a cache
                    // read will request to fetch the full line from memory.
                    let read = cache.$read_name(
                        addr,
                        |_| [written; 4],
                    );

                    assert_eq!(
                        written as $ty,
                        read,
                        "failed to read from {:#010x} ({})",
                        addr.init,
                        Address::from(addr),
                    );
                }
            }
        };
    }

    macro_rules! def_read_cache_near_fn {
        ($fn_name:ident, $ty:ty, $read_name:ident, $write_name:ident $(,)?) => {
            #[test]
            fn $fn_name() {
                let mut cache = Cache::default();
                for addr in gen_test_addrs() {
                    // We will attempt something very similar to that in [`def_read_cache_far_fn`],
                    // but instead of such far-spaced addresses, we will use ones closer together---
                    // within the same cache line, even.

                    let written: u8 = rand::random();
                    let written: u32 = u32::from_be_bytes([written; 4]);

                    // First, we perform a normal write. The cache entry is marked as invalid.
                    cache.$write_name(addr, written as $ty);
                    // Then, we read. The cache line is fetched from memory, and the cache entry is
                    // marked as valid.
                    cache.$read_name(addr, |_| [written; 4]);
                    // Then, we write again, but to the next word in the cache line. We will use a
                    // different value for `written` so that we don't get a false positive as the
                    // entire cache line was filled with `written`. In this case, inverting
                    // `written` works.
                    let next_addr = mem::Address::from(addr.init.wrapping_add(4));
                    cache.$write_name(next_addr, !written as $ty);

                    // Because the cache entry was marked as valid, the new write should've updated
                    // its data. Let's see.
                    let read = cache.$read_name(
                        next_addr,
                        |_| {
                            // If the read requests to fetch the line again, then something has gone
                            // wrong.
                            panic!("fetched cache line twice");
                        },
                    );

                    assert_eq!(
                        // Remember: the second word is an inverted `written`.
                        !written as $ty,
                        read,
                        "failed to read from {:#010x} ({})",
                        addr.init,
                        Address::from(addr),
                    );
                }
            }
        };
    }

    def_read_cache_far_fn!(read_cache_8_far, u8, read_8, write_8);
    def_read_cache_far_fn!(read_cache_16_far, u16, read_16, write_16);
    def_read_cache_far_fn!(read_cache_32_far, u32, read_32, write_32);
    def_read_cache_near_fn!(read_cache_8_near, u8, read_8, write_8);
    def_read_cache_near_fn!(read_cache_16_near, u16, read_16, write_16);
    def_read_cache_near_fn!(read_cache_32_near, u32, read_32, write_32);
}
