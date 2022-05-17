// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

// Unless otherwise specified, "Section x.x.x" refers to that from ISO 9660 or ECMA-119. To read
// along with the code, a free copy of ECMA-119 is available [here].
//
// [here]: https://www.ecma-international.org/publications-and-standards/standards/ecma-119/

#![feature(cstr_from_bytes_until_nul)]

mod volume;

use std::io::{self, Read};

use serde::de::Deserialize as _;

#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    Nul(std::ffi::NulError),
    IntoString(std::ffi::IntoStringError),
    FromUtf8(std::str::Utf8Error),
    Deserialize(volume::Error),
    ExpectedLogicalSector,
    ExpectedVolumeDescriptor,
    InvalidStandardId,
    IncompatibleVersion,
}

pub struct FileSystem(());

impl FileSystem {
    /// Attempts to read a file system with logical sectors of the standard size 2048.
    pub fn from_reader(reader: impl Read) -> Result<Self, crate::Error> {
        // 2^11 = 2048.
        Self::from_reader_with_sector_size_exp(reader, 11)
    }

    /// Attempts to read a file system with logical sectors of the size 2^`n`.
    pub fn from_reader_with_sector_size_exp(
        mut reader: impl Read,
        n: u32,
    ) -> Result<Self, crate::Error> {
        let logical_sector_size = 2usize.pow(n);
        tracing::debug!("logical_sector_size: {}", logical_sector_size);

        // There must be a system area, which is 16 logical sectors, and there is likely at least
        // one logical sector in the data section. Given a logical sector size of 2^n bytes, the
        // minimum size is (17 * 2^n).
        let mut data = Vec::with_capacity(17 * logical_sector_size);

        let data_len = reader.read_to_end(&mut data).map_err(crate::Error::Io)?;
        tracing::debug!("data_len: {}", data_len);

        assert_eq!(data_len, data.len());

        let mut sectors = data.chunks_exact(logical_sector_size).skip(16);

        // We are now in the data section. We will make no assumptions regarding the location of
        // logical sectors, treating this CD-ROM image as just that: a generic CD rather than a
        // specialized PSX game disc.

        loop {
            // First up are the volume descriptors. We will process these until we hit a descriptor
            // set terminator.

            let desc = sectors
                .next()
                .ok_or(Error::ExpectedLogicalSector)?;
            let mut de = volume::Deserializer::from_bytes(desc);

            let header = volume::DescriptorHeader::deserialize(&mut de)
                .map_err(Error::Deserialize)?;
            tracing::info!("found volume descriptor: {:?}", header);

            if header.std_id != *b"CD001" {
                return Err(Error::InvalidStandardId);
            }

            match header.kind {
                volume::DescriptorKind::BootRecord => {
                    let desc = volume::BootRecord::deserialize(&mut de)
                        .map_err(Error::Deserialize)?;
                    tracing::debug!("boot record: {:#?}", desc);
                }
                volume::DescriptorKind::Primary => {
                    let desc = volume::PrimaryDescriptor::deserialize(&mut de)
                        .map_err(Error::Deserialize)?;
                    tracing::debug!("primary descriptor: {:#?}", desc);
                }
                volume::DescriptorKind::Partition => {
                    let desc = volume::PartitionDescriptor::deserialize(&mut de)
                        .map_err(Error::Deserialize)?;
                    tracing::debug!("partition descriptor: {:#?}", desc);
                }
                volume::DescriptorKind::SetTerminator => {
                    // A set terminator indicates that the volume descriptor set ends here.
                    break;
                }
                _ => todo!(),
            }
        }

        Ok(Self(()))
    }
}
