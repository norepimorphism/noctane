mod i;

impl Default for Cache {
    fn default() -> Self {
        Self {
            i: i::Cache::default(),
            d: [0xff; 0xff],
        }
    }
}

pub struct Cache {
    pub i: i::Cache,
    pub d: [u32; 0xff],
}
