// SPDX-License-Identifier: MPL-2.0

mod raw;

use std::fmt;

pub use raw::DeserializeError;
use serde::Deserialize as _;

use crate::Error;

impl Volume {
    pub fn new(logical_sector_size: usize, data: Vec<u8>) -> Self {
        Self {
            logical_sector_size,
            data,
        }
    }
}

#[derive(Debug)]
pub struct Volume {
    logical_sector_size: usize,
    data: Vec<u8>,
}

impl Volume {
    pub fn system_area(&self) -> Result<&[u8], Error> {
        self.data
            .get(0..self.system_area_size())
            .ok_or(Error::InvalidSystemAreaSize)
    }

    pub fn system_area_size(&self) -> usize {
        self.logical_sector_size * 16
    }

    pub fn data_area(&self) -> Result<&[u8], Error> {
        self.data
            .get(self.system_area_size()..)
            .ok_or(Error::ExpectedDataArea)
    }

    pub fn descriptors(&self) -> impl '_ + Iterator<Item = Result<Descriptor, Error>> {
        // Skip the system area.
        let mut sectors = self.data_sectors();

        std::iter::from_fn(move || {
            let desc = match sectors.next() {
                Some(it) => it,
                None => {
                    return Some(Err(Error::ExpectedVolumeDescriptor));
                }
            };

            let mut de = raw::Deserializer::from_bytes(desc);

            let header = match raw::DescriptorHeader::deserialize(&mut de) {
                Ok(it) => it,
                Err(e) => {
                    return Some(Err(Error::Deserialize(e)));
                }
            };
            // TODO: Only log the descriptor type.
            tracing::info!("Found {:?}", header.kind);

            if header.std_id != *b"CD001" {
                return Some(Err(Error::InvalidStandardId));
            }

            match header.kind {
                raw::DescriptorKind::BootRecord => todo!(),
                // These are basically the same... right?
                raw::DescriptorKind::Primary | raw::DescriptorKind::Secondary => {
                    let raw_desc = match raw::PrimaryDescriptor::deserialize(&mut de) {
                        Ok(it) => it,
                        Err(e) => {
                            return Some(Err(Error::Deserialize(e)));
                        }
                    };

                    Some(PrimaryDescriptor::read(self, raw_desc).map(Descriptor::Primary))
                }
                raw::DescriptorKind::Partition => todo!(),
                raw::DescriptorKind::SetTerminator => {
                    // A set terminator indicates that the volume descriptor set ends here.
                    None
                }
            }
        })
    }

    pub fn sectors(&self) -> impl Iterator<Item = &[u8]> {
        self.data.chunks_exact(self.logical_sector_size)
    }

    pub fn data_sectors(&self) -> impl Iterator<Item = &[u8]> {
        // Skip the system area.
        self.sectors().skip(16)
    }

    pub fn blocks(&self, logical_block_size: usize) -> impl Iterator<Item = &[u8]> {
        self.data.chunks_exact(logical_block_size)
    }

    pub fn bytes(&self) -> &[u8] {
        self.data.as_slice()
    }
}

#[derive(Debug)]
pub enum Descriptor {
    Primary(PrimaryDescriptor),
}

impl PrimaryDescriptor {
    fn read(vol: &Volume, raw: raw::PrimaryDescriptor) -> Result<Self, Error> {
        let directories = Self::read_path_table(
            vol,
            u16::from(raw.logical_block_size).into(),
            Self::select_path_table_lba(&raw).into(),
            u32::from(raw.path_table_size)
                .try_into()
                .map_err(|_| Error::IntegerOverflow)?,
        )?;

        Ok(Self { directories })
    }

    fn select_path_table_lba(raw: &raw::PrimaryDescriptor) -> u16 {
        if cfg!(target_endian = "little") {
            raw.l_path_table_lba.into()
        } else {
            raw.m_path_table_lba.into()
        }
    }

    fn read_path_table(
        vol: &Volume,
        logical_block_size: usize,
        lba: usize,
        size: usize,
    ) -> Result<Vec<Directory>, Error> {
        let start_addr = lba * logical_block_size;
        let end_addr = start_addr + size;

        let bytes = vol
            .data
            .get(start_addr..end_addr)
            .ok_or(Error::ExpectedPathTable)?;
        let mut de = raw::Deserializer::from_bytes(bytes);

        let records = Self::iter_directories(vol, logical_block_size, &mut de)
            .collect::<Result<Vec<Directory>, Error>>()?;

        Ok(records)
    }

    fn iter_directories<'de>(
        vol: &'de Volume,
        logical_block_size: usize,
        table_de: &'de mut raw::Deserializer<'de>,
    ) -> impl 'de + Iterator<Item = Result<Directory, Error>> {
        std::iter::from_fn(move || {
            let raw_record = raw::PathTableRecord::deserialize(&mut *table_de).ok()?;
            let parent_num = raw_record.parent_num.into();
            let id = match table_de
                .take(raw_record.dir_id_len.into())
                .ok_or(Error::ExpectedEntryId)
                .and_then(|it| std::str::from_utf8(it).map_err(Error::DecodeUtf8))
            {
                Ok(it) => it.into(),
                Err(e) => {
                    return Some(Err(e));
                }
            };

            let extent_lba = match u32::from(raw_record.extent_lba)
                .try_into()
                .map_err(|_| Error::IntegerOverflow)
            {
                Ok(it) => it,
                Err(e) => {
                    return Some(Err(e));
                }
            };
            let files = match Self::read_directory(vol, logical_block_size, extent_lba) {
                Ok(it) => it,
                Err(e) => {
                    return Some(Err(e));
                }
            };

            // See Section 9.4.6.
            if (raw_record.dir_id_len % 2) != 0 {
                let _ = table_de.take(1);
            }

            Some(Ok(Directory {
                parent_num,
                id,
                files,
            }))
        })
    }

    fn read_directory(
        vol: &Volume,
        logical_block_size: usize,
        lba: usize,
    ) -> Result<Vec<File>, Error> {
        let start_addr = lba * logical_block_size;
        let bytes = vol
            .data
            .get(start_addr..)
            .ok_or(Error::ExpectedExtent)?;
        let mut de = raw::Deserializer::from_bytes(bytes);

        let files = Self::iter_files(&mut de)
            // Skip the root directory entries.
            .skip(2)
            .collect::<Result<Vec<File>, Error>>()?;

        Ok(files)
    }

    fn iter_files<'de>(
        table_de: &'de mut raw::Deserializer<'de>,
    ) -> impl 'de + Iterator<Item = Result<File, Error>> {
        std::iter::from_fn(|| {
            let raw_record_len = table_de.take(1)?[0];
            if raw_record_len == 0 {
                return None;
            }
            if raw_record_len < 34 {
                tracing::warn!("Raw file record is shorter than minimum expected length");
            }

            let bytes = match table_de.take(usize::from(raw_record_len - 1)) {
                Some(it) => it,
                None => {
                    return Some(Err(Error::ExpectedFileRecord));
                }
            };
            let mut de = raw::Deserializer::from_bytes(bytes);
            let raw_record = match raw::EntryRecord::deserialize(&mut de) {
                Ok(it) => it,
                Err(e) => {
                    return Some(Err(Error::Deserialize(e)));
                }
            };

            let id = match raw_record.file_id.to_str() {
                Ok(it) => it.into(),
                Err(e) => {
                    return Some(Err(e));
                }
            };
            let timestamp = raw_record.timestamp.into();

            Some(Ok(File { id, timestamp }))
        })
    }
}

#[derive(Debug)]
pub struct PrimaryDescriptor {
    pub directories: Vec<Directory>,
}

#[derive(Clone, Debug)]
pub struct Directory {
    pub parent_num: u32,
    pub id: String,
    pub files: Vec<File>,
}

#[derive(Clone, Debug)]
pub struct File {
    pub id: String,
    pub timestamp: NumTimestamp,
}

impl From<raw::NumTimestamp> for NumTimestamp {
    fn from(raw: raw::NumTimestamp) -> Self {
        Self {
            year: 1900 + u32::from(raw.year),
            month: raw.month.into(),
            day: raw.day.into(),
            hour: raw.hour.into(),
            minute: raw.minute.into(),
            second: raw.second.into(),
            gmt_offset: raw.gmt_offset.into(),
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct NumTimestamp {
    pub year: u32,
    pub month: u32,
    pub day: u32,
    pub hour: u32,
    pub minute: u32,
    pub second: u32,
    pub gmt_offset: u32,
}

impl fmt::Display for NumTimestamp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}/{}/{} {:02}:{:02}:{:02}",
            self.month, self.day, self.year, self.hour, self.minute, self.second,
        )?;

        let gmt_offset_as_min = self.gmt_offset * 15;
        if gmt_offset_as_min > 0 {
            // TODO: Implement minus.
            write!(
                f,
                " +{:02}:{:02}",
                gmt_offset_as_min / 60,
                gmt_offset_as_min % 60,
            )?;
        }

        Ok(())
    }
}
