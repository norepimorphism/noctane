// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

// Unless otherwise specified, "Section x.x.x" refers to that from ISO 9660 or ECMA-119. To read
// along with the code, a free copy of ECMA-119 is available [here].
//
// [here]: https://www.ecma-international.org/publications-and-standards/standards/ecma-119/

mod volume;

use std::{collections::VecDeque, io::{self, Read}};

use derivative::Derivative;

pub use entry::Entry;
pub use volume::{NumTimestamp, Volume};

#[derive(Debug)]
pub enum Error {
    DecodeUtf8(std::str::Utf8Error),
    Deserialize(volume::DeserializeError),
    ExpectedDataArea,
    ExpectedEntryId,
    ExpectedExtent,
    ExpectedFileRecord,
    ExpectedLogicalBlock,
    ExpectedLogicalSector,
    ExpectedPathTable,
    ExpectedPathTableRecord,
    ExpectedVolumeDescriptor,
    IncompatibleVersion,
    IntegerOverflow,
    InvalidParentDirectory,
    InvalidStandardId,
    InvalidSystemAreaSize,
    Io(io::Error),
}

/// An ISO 9660 or ECMA-119 fileystem for CD-ROM images.
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct FileSystem {
    #[derivative(Debug = "ignore")]
    pub system_area: Vec<u8>,
    pub boot_records: Vec<Region>,
    pub partitions: Vec<Region>,
    pub root_dirs: Vec<Directory>,
}

/// Miscellaneous volume space.
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct Region {
    /// The identifier of this region.
    pub id: String,
    /// The identifier of the system that produced this region.
    pub system_id: String,
    /// The contents of this region.
    #[derivative(Debug = "ignore")]
    pub data: Vec<u8>,
}

/// A container of entries.
#[derive(Clone, Debug)]
pub struct Directory {
    /// Metadata for this directory.
    pub meta: entry::Metadata,
    /// The contents of this directory.
    pub entries: Vec<Entry>,
}

#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct File {
    /// Metadata for this file.
    pub meta: entry::Metadata,
    /// The contents of this file.
    #[derivative(Debug = "ignore")]
    pub data: Vec<u8>,
}

pub mod entry {
    use crate::{Directory, File, NumTimestamp};

    /// A directory or file.
    #[derive(Clone, Debug)]
    pub enum Entry {
        /// A directory.
        Directory(Directory),
        /// A file.
        File(File),
    }

    #[derive(Clone, Debug)]
    pub struct Metadata {
        /// Whether or not this entry should be listed to users.
        pub is_listed: bool,
        pub name: String,
        pub timestamp: NumTimestamp,
        pub attr: Option<Attributes>,
    }

    impl Default for Metadata {
        fn default() -> Self {
            Self {
                is_listed: true,
                name: String::new(),
                timestamp: NumTimestamp::default(),
                attr: None,
            }
        }
    }

    #[derive(Clone, Debug)]
    pub struct Attributes {
        pub owner_id: u16,
        pub group_id: u16,
        pub is_protected: bool,
        pub perms: Permissions,
    }

    impl Default for Attributes {
        fn default() -> Self {
            Self {
                owner_id: 0,
                group_id: 0,
                is_protected: false,
                perms: Default::default(),
            }
        }
    }

    #[derive(Clone, Debug, Default)]
    pub struct Permissions {
        pub system: Permission,
        pub owner: Permission,
        pub group: Permission,
        pub all: Permission,
    }

    #[derive(Clone, Debug)]
    pub struct Permission {
        pub read: bool,
        pub execute: bool,
    }

    impl Default for Permission {
        fn default() -> Self {
            Self { read: true, execute: true }
        }
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

        let mut data = Vec::new();

        tracing::info!("Reading the entire CD-ROM image into memory...");
        let data_len = reader.read_to_end(&mut data).map_err(crate::Error::Io)?;
        tracing::debug!("data_len: {}", data_len);

        // TODO: Is there a valid reason why these would be inequal?
        assert_eq!(data_len, data.len());

        let vol = Volume::new(logical_sector_size, data);

        let system_area = vol.system_area()?.into();
        let mut boot_records = Vec::new();
        let mut partitions = Vec::new();
        let mut root_dirs = Vec::new();

        for desc in vol.descriptors() {
            match desc? {
                /*
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
                */
                volume::Descriptor::Primary(desc) => {
                    root_dirs.push(Directory::read_tree(desc.directories.into_iter())?);
                }
            }
        }

        Ok(Self { system_area, boot_records, partitions, root_dirs })
    }
}

impl Directory {
    fn read_tree<I>(mut dirs: I) -> Result<Self, Error>
    where
        I: Iterator<Item = volume::Directory> + DoubleEndedIterator + ExactSizeIterator,
    {
        // The following is an algorithm I devised to produce a tree structure from a flat sequence
        // of `volume::Directory`s in the original order in which they were serialized. This
        // algorithm relies on a key assumption: that parent directory indices are serialized in
        // ascending order. And, as far as I can tell from ECMA-119 Section 6.9.1, that assertion
        // should be correct in standard-compliant CD-ROM filesystems.
        //
        // The algorithm works like so:
        //
        // 1. Reverse the iterator of `volume::Directory`s such that the directory with the highest
        //    parent index is first.
        //
        // 2. Take the now-first (previously last) directory from the iterator. Note its parent
        //    directory index; if that index is 1, then the root directory is the only parent
        //    directory in the entire filesystem, and so we can simply append the remaining
        //    directories in the iterator to the root directory.
        //
        // 3. Otherwise, if the parent index is not 1, then create a sequence of pools with a length
        //    of the parent index. Because there are as many different parent indices as there are
        //    parents, and the indices begin at 1, this length is also the total number of parent
        //    directories; that is, we have created a pool for each parent directory.
        //
        // 4. Store the first directory from the iterator in the last pool.
        //
        // 5. For the remaining directories in the iterator, perform the following steps:
        //
        //    a. Take the next directory from the iterator. Note its index *in the iterator*. If
        //       that index is a valid index into the pool sequence, then this directory is the
        //       parent of those in the pool identified by its iterator index. Pop the children
        //       from this pool and append them to the parent directory.
        //
        //    b. Store this directory in the pool identified by its parent directory index.
        //
        // I'm sure there are more efficient algorithms to accomplish what I have set out to do.
        // But, my abomination works in the present. Hopefully this explanation has demystified the
        // following plate of code spaghetti.

        let mut root_dir: Directory = dirs
            .next()
            .ok_or(Error::ExpectedPathTableRecord)
            .map(|it| it.into())?;

        let mut dirs = dirs
            .enumerate()
            // Now, both the iterator indices and directories are in reverse, so the first item is
            // the directory with the highest parent directory index.
            .rev();

        if let Some((_, last_dir)) = dirs.next() {
            let last_parent_idx = last_dir.parent_num
                .try_into()
                .map_err(|_| Error::IntegerOverflow)?;
            let last_dir = last_dir.into();

            if last_parent_idx == 1 {
                // The parent of the last directory is the root directory. Therefore, the root
                // directory is the only parent directory in the filesystem.

                root_dir.entries.push(Entry::Directory(last_dir));
                while let Some((_, dir)) = dirs.next() {
                    root_dir.entries.push(Entry::Directory(dir.into()));
                }
            } else {
                let mut pools = VecDeque::new();
                pools.resize(last_parent_idx, Vec::new());

                // Slice indices are zero-indexed while parent directory indices are not, so we must
                // subtract one.
                pools[last_parent_idx - 1] = vec![last_dir];

                while let Some((idx, dir)) = dirs.next() {
                    let parent_idx: usize = dir.parent_num
                        .try_into()
                        .map_err(|_| Error::IntegerOverflow)?;
                    let mut dir = Directory::from(dir);

                    if (idx + 2) == pools.len() {
                        let mut entries = pools
                            .pop_back()
                            .unwrap()
                            .into_iter()
                            .map(|dir| Entry::Directory(dir))
                            .collect();
                        dir.entries.append(&mut entries);
                    }

                    pools
                        .get_mut(parent_idx - 1)
                        .ok_or(Error::InvalidParentDirectory)?
                        .push(dir);
                }

                let mut root_dir_entries = pools
                    .pop_front()
                    .unwrap()
                    .into_iter()
                    .map(|dir| Entry::Directory(dir))
                    .collect();
                root_dir.entries.append(&mut root_dir_entries);
            }
        }

        Ok(root_dir)
    }
}

impl From<volume::Directory> for Directory {
    fn from(it: volume::Directory) -> Self {
        let meta = entry::Metadata {
            // TODO
            is_listed: true,
            name: it.id,
            // TODO
            timestamp: NumTimestamp::default(),
            // TODO
            attr: None,
        };
        let entries = it.files
            .into_iter()
            .map(|it| Entry::File(it.into()))
            .collect();

        Directory { meta, entries }
    }
}

impl From<volume::File> for File {
    fn from(it: volume::File) -> Self {
        Self {
            meta: entry::Metadata {
                // TODO
                is_listed: true,
                name: it.id,
                timestamp: it.timestamp,
                // TODO
                attr: None,
            },
            // TODO
            data: Vec::new(),
        }
    }
}
