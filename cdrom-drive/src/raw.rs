impl Volume {
    pub fn new(logical_sector_size: usize, data: Vec<u8>) -> Self {
        Self { logical_sector_size, data }
    }
}

pub struct Volume {
    logical_sector_size: usize,
    data: Vec<u8>,
}

impl Volume {
    pub fn sector(&self, index: usize) -> Result<&[u8], crate::Error> {
        let start = index * self.logical_sector_size;
        let end = start + self.logical_sector_size;

        self.data.get(start..end).ok_or_else(|| crate::Error::MissingSector { index })
    }
}

/*
enum LogicalSector {
    Data,
    VolumeDescriptor,
    PathTable,
    DirectoryRecord,
}

#[derive(Deserialize)]
struct RawVolume {
    sys: [LogicalSector; 16],
    data: Vec<LogicalSector>,
}

#[derive(Deserialize)]
struct LogicalSector(Vec<u8>);

/// See Section 8.1.
#[derive(Deserialize)]
struct Descriptor {
    kind: DescriptorKind,
    id: [u8; 5],
    version: u8,
}

/// See Section 8.1.1.
#[derive(Deserialize)]
enum DescriptorKind {
    BootRecord,
    Primary,
    Supplementary,
    Partition,
    SetTerminator,
}

/// See Section 8.2.
#[derive(Deserialize)]
struct BootRecord {
    boot_sys_id: String,
    boot_id: String,
}

/// See Section 8.4.
#[derive(Deserialize)]
struct PrimaryDescriptor {
    #[serde(skip)]
    _unused_0: u8,
    sys_id: String,
    vol_id: String,
    _unused_3: u8,
    vol_space_size: u64,
    _unused_5: [u8; 32],
    vol_set_size: u32,
    vol_seq_num: u32,
    logical_block_size: u32,
    path_table_size: u64,
}

/// See Section 8.5.
#[derive(Deserialize)]
struct SupplementaryDescriptor {
    vol_flags: u8,
    sys_id: String,
    vol_id: String,
    _unused_3: [u8; 8],
    vol_space_size: u64,
    esc_seqs: [u8; 32],
    vol_set_size: u32,
    vol_seq_num: u32,
    logical_block_size: i32,
    path_table_size: u64,
}

#[derive(Deserialize)]
struct PartitionDescriptor {
    _unused_0: u8,
    sys_id: String,
    vol_part_id: String,
    vol_part_addr: u64,
    vol_part_size: u64,
}

/// See Section 9.1.
#[derive(Deserialize)]
struct DirectoryRecord {
    extent_addr: u64,
}
*/
