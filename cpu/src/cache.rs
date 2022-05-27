mod i_cache;

use i_cache::InstrCache;

impl Default for Cache {
    fn default() -> Self {
        Self {
            i: InstrCache::default(),
            d: [0xff; 0xff],
        }
    }
}

pub struct Cache {
    pub i: InstrCache,
    pub d: [u32; 0xff],
}
