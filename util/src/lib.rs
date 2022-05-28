// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use std::{fmt, io};

pub fn dump_hex(w: &mut impl io::Write, pc: u32, bytes: [u8; 4]) {
    write!(
        w,
        "{:08x}   {}   {}",
        pc,
        bytes
            .iter()
            .map(|byte| format!("{:02x}", byte))
            .collect::<Vec<String>>()
            .join(" "),
        bytes
            .iter()
            .map(|byte| {
                if byte.is_ascii_graphic() {
                    unsafe { char::from_u32_unchecked(u32::from(*byte)) }
                } else {
                    '.'
                }
            })
            .map(|byte| format!("{}", byte))
            .collect::<String>(),
    )
    .unwrap();
}
