// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

// Unless otherwise specified, "Section x.x.x" refers to that from ISO 9660 or ECMA-119. To read
// along with the code, a free copy of ECMA-119 is available [here].
//
// [here]: https://www.ecma-international.org/publications-and-standards/standards/ecma-119/

mod raw;

use std::io::{self, Read};

#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    /// An expected logical sector is missing from the CD-ROM image.
    MissingSector { index: usize },
    FromUtf8(std::str::Utf8Error),
}

pub struct Volume {
    // descs: Vec<LogicalSector>,
}

impl Volume {
    /// Attempts to read a volume with logical sectors of the standard size 2048.
    pub fn read(vol: impl Read) -> Result<Self, crate::Error> {
        // 2^11 = 2048.
        Self::read_with_sector_size_exp(vol, 11)
    }

    /// Attempts to read a volume with logical sectors of the size 2^`n`.
    pub fn read_with_sector_size_exp(mut vol: impl Read, n: u32) -> Result<Self, crate::Error> {
        let logical_sector_size = 2usize.pow(n);
        tracing::debug!("logical_sector_size: {}", logical_sector_size);

        // There must be a system area, which is 16 logical sectors, and there is likely at least
        // one logical sector in the data section. Given a logical sector size of 2^n bytes, the
        // minimum size is (17 * 2^n).
        let mut data = Vec::with_capacity(17 * logical_sector_size);

        let data_len = vol.read_to_end(&mut data).map_err(crate::Error::Io)?;
        tracing::debug!("data_len: {}", data_len);

        assert_eq!(data_len, data.len());

        // This is semantically a copy but shouldn't actually compile into one.
        let raw = raw::Volume::new(logical_sector_size, data);

        // AFAIK, we're pretty much on our own for deserializing a CD-ROM image---we can't use
        // zero-copy (de)serialization libraries (e.g., *rkyv*) as fields in a CD-ROM aren't
        // necessarily aligned to word boundaries, and using *serde* would be a mess. Instead, we
        // go the old-fashioned way: read the entire image into a buffer, and manually copy the bits
        // and pieces we need into a custom structure.
        //
        // As an optimization (because we're working with large files here), we will not utilize any
        // internal representations (IRs). That is, the CD-ROM image is deserialized and parsed
        // simultaneously. Fortunately, there are some tools that can aid us in parsing. We will be
        // drawing from *nom*'s repetoire of parser combinators.

        // System sectors 0..3 [are zeroed], so we can skip them. The first meaningful data starts
        // at sector 4: the license string. It looks ASCII to me, so we will parse it as UTF-8.
        //
        // [are zeroed]: https://psx-spx.consoledev.net/cdromdrive/#system-area-prior-to-volume-descriptors

        // `String::from_utf8` requires a `Vec` of the entire input, but it's likely that only part
        // of the sector contains valid ASCII. We use `str::from_utf8` in the hopes that a full copy
        // isn't necessary.
        let license = std::str::from_utf8(raw.sector(4)?)
            .map_err(Error::FromUtf8)
            .map(String::from)?;
        tracing::debug!("license: {}", license);

        todo!()
    }
}
