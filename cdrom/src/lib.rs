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

#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    DecodeUtf8(std::str::Utf8Error),
    Deserialize(volume::Error),
    ExpectedLogicalSector,
    ExpectedVolumeDescriptor,
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
    pub fn from_reader(reader: impl Read) -> Result<(), crate::Error> {
        // 2^11 = 2048.
        Self::from_reader_with_sector_size(reader, 11)
    }

    /// Attempts to read a file system with logical sectors of the size 2^`n`.
    pub fn from_reader_with_sector_size(
        mut reader: impl Read,
        n: u32,
    ) -> Result<(), crate::Error> {
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

        // Split the data into sectors and skip the system area.
        let mut sectors = data.chunks_exact(logical_sector_size).skip(16);

        // We are now in the data area. We will make no assumptions regarding the location of
        // logical sectors, treating this CD-ROM image as just that: a generic CD-ROM rather than a
        // specialized PSX game disc.

        for desc in iter_volume_descriptors(&mut sectors) {
            
        }

        Ok(())
    }
}

struct VolumeDescriptor<'a> {
    kind: volume::DescriptorHeader,
    de: volume::Deserializer<'a>,
}

fn iter_volume_descriptors<'a>(
    sectors: &mut impl Iterator<Item = &'a [u8]>,
) -> impl 'a + Iterator<Item = Result<VolumeDescriptor<'a>, Error>> {
    std::iter::from_fn(|| {
        let desc = sectors
            .next()
            .ok_or(Error::ExpectedLogicalSector)?;
        let mut de = volume::Deserializer::from_bytes(desc);

        let header = volume::DescriptorHeader::deserialize(&mut de)
            .map_err(Error::Deserialize)?;
        // TODO: Only log the descriptor type.
        tracing::info!("{:?}", header);

        if header.std_id != *b"CD001" {
            return Some(Err(Error::InvalidStandardId));
        }
        
        if matches!(header.kind, volume::DescriptorKind::SetTerminator {
            return None;
        }
        
        Some(Ok(Self { kind: header.kind, de })
    })
}

fn process_boot_record(de: &mut volume::Deserializer) -> Result<(), Error> {
    let desc = volume::BootRecord::deserialize(de)
        .map_err(Error::Deserialize)?;
    // TODO: Remove this.
    tracing::debug!("{:#?}", desc);

    Ok(())
}

fn process_primary_volume_descriptor(de: &mut volume::Deserializer) -> Result<(), Error> {
    let desc = volume::PrimaryDescriptor::deserialize(de)
        .map_err(Error::Deserialize)?;
    // TODO: Remove this.
    tracing::debug!("{:#?}", desc);

    let mut root_dir_de = volume::Deserializer::from_bytes(&desc.root_dir);
    let root_dir = volume::EntryRecord::deserialize(&mut root_dir_de)
        .map_err(Error::Deserialize)?;
    // TODO: Remove this.
    tracing::debug!("{:#?}", root_dir);

    Self::validate_root_directory(&root_dir)?;

    Ok(())
}

fn validate_root_directory(_: &volume::EntryRecord) -> Result<(), Error> {
    Ok(())
}

fn process_volume_partition_descriptor(de: &mut volume::Deserializer) -> Result<(), Error> {
    let desc = volume::PartitionDescriptor::deserialize(de)
        .map_err(Error::Deserialize)?;
    // TODO: Remove this.
    tracing::debug!("{:#?}", desc);

    Ok(())
}
