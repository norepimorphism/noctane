// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

// Unless otherwise specified, "Section x.x.x" refers to that from ISO 9660 or ECMA-119. To read
// along with the code, a free copy of ECMA-119 is available [here].
//
// [here]: https://www.ecma-international.org/publications-and-standards/standards/ecma-119/

mod volume;

use std::io::{self, Read};

use serde::de::Deserialize as _;

pub use entry::Entry;
use volume::Volume;

#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    DecodeUtf8(std::str::Utf8Error),
    Deserialize(volume::DeserializeError),
    IntegerOverflow,
    ExpectedLogicalSector,
    ExpectedVolumeDescriptor,
    ExpectedLogicalBlock,
    ExpectedPathTable,
    ExpectedEntryId,
    InvalidStandardId,
    IncompatibleVersion,
}

/// An ISO 9660 or ECMA-119 fileystem for CD-ROM images.
pub struct FileSystem {
    pub boot_records: Vec<Region>,
    pub partitions: Vec<Region>,
    pub root_dirs: Vec<Directory>,
}

/// Miscellaneous volume space.
pub struct Region {
    /// The identifier of this region.
    pub id: String,
    /// The identifier of the system that produced this region.
    pub system_id: String,
    /// The contents of this region.
    pub data: Vec<u8>,
}

/// A container of entries.
pub struct Directory {
    /// Metadata for this directory.
    pub meta: entry::Metadata,
    /// The contents of this directory.
    pub entries: Vec<Entry>,
}


pub struct File {
    /// Metadata for this file.
    pub meta: entry::Metadata,
    /// The contents of this file.
    pub data: Vec<u8>,
}

pub mod entry {
    use crate::{Directory, File};

    /// A directory or file.
    pub enum Entry {
        /// A directory.
        Directory(Directory),
        /// A file.
        File(File),
    }

    pub struct Metadata {
        /// Whether or not this entry should be listed to users.
        pub is_listed: bool,
        pub attr: Option<Attributes>,
    }

    pub struct Attributes {
        pub owner_id: u16,
        pub group_id: u16,
        pub is_protected: bool,
        pub perms: Permissions,
    }

    pub struct Permissions {
        pub system: Permission,
        pub owner: Permission,
        pub group: Permission,
        pub all: Permission,
    }

    pub struct Permission {
        pub read: bool,
        pub execute: bool,
    }
}

impl FileSystem {
    /// Attempts to read a file system with logical sectors of the standard size 2048.
    pub fn from_reader(reader: impl Read) -> Result<Self, crate::Error> {
        // 2^11 = 2048.
        Self::from_reader_with_sector_size(reader, 11)
    }

    /// Attempts to read a file system with logical sectors of the size 2^`n`.
    pub fn from_reader_with_sector_size(
        mut reader: impl Read,
        n: u32,
    ) -> Result<Self, crate::Error> {
        let logical_sector_size = 2usize.pow(n);
        tracing::debug!("logical_sector_size: {}", logical_sector_size);

        // There must be a system area, which is 16 logical sectors, and there must be at least
        // one logical sector (set terminator) in the data area. Given a logical sector size of
        // 2^n bytes, the minimum size of the file system is (17 * 2^n).
        let mut data = Vec::with_capacity(17 * logical_sector_size);

        let data_len = reader.read_to_end(&mut data).map_err(crate::Error::Io)?;
        tracing::debug!("data_len: {}", data_len);

        // TODO: Is there a valid reason why these would be inequal?
        assert_eq!(data_len, data.len());

        let vol = Volume::new(logical_sector_size, data);
        let mut boot_records = Vec::new();
        let mut partitions = Vec::new();
        let mut root_dirs = Vec::new();

        for desc in vol.descriptors() {
            match desc? {
                volume::Descriptor::BootRecord(desc) => {
                    boot_records.push(Region {
                        id: desc.boot_id.to_str()?.into(),
                        system_id: desc.boot_sys_id.to_str()?.into(),
                        data: desc.data.into(),
                    });
                }
                volume::Descriptor::Partition(desc) => {
                    let start: usize = u32::from(desc.vol_part_addr)
                        .try_into()
                        .map_err(|_| Error::IntegerOverflow)?;
                    let size: usize = u32::from(desc.vol_part_size)
                        .try_into()
                        .map_err(|_| Error::IntegerOverflow)?;

                    let data = vol
                        .bytes()
                        .get(start..(start + size))
                        .ok_or(Error::ExpectedLogicalSector)
                        .map(|it| it.to_vec())?;

                    partitions.push(Region {
                        id: desc.vol_part_id.to_str()?.into(),
                        system_id: desc.sys_id.to_str()?.into(),
                        data,
                    });
                }
                volume::Descriptor::Primary(desc) => {
                    tracing::debug!("{:#?}", desc);

                    let mut de = volume::Deserializer::from_bytes(&desc.root_dir);
                    let root_dir = volume::EntryRecord::deserialize(&mut de)
                        .map_err(Error::Deserialize)?;
                    let root_dir_addr: usize = u32::from(root_dir.extent_addr)
                        .try_into()
                        .map_err(|_| Error::IntegerOverflow)?;

                    let block = vol
                        .blocks(u16::from(desc.logical_block_size).into())
                        .nth(root_dir_addr)
                        .ok_or(Error::ExpectedPathTable)?;

                    let mut de = volume::Deserializer::from_bytes(block);
                    let root_dir = volume::EntryRecord::deserialize(&mut de)
                        .map_err(Error::Deserialize)?;
                    tracing::debug!("{:#?}", root_dir);

                    while let Ok(entry) = volume::EntryRecord::deserialize(&mut de) {
                        if entry.len == 0 {
                            break;
                        }

                        tracing::debug!(
                            " - /{}\n  {} bytes\n  {}",
                            entry.file_id.to_str().unwrap(),
                            u32::from(entry.data_len),
                            entry.timestamp,
                        );
                    }
                }
            }
        }

        Ok(Self { boot_records, partitions, root_dirs })
    }
}
