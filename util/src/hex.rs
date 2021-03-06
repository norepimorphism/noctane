// SPDX-License-Identifier: MPL-2.0

use std::io;

pub fn dump(w: &mut impl io::Write, pc: u32, bytes: [u8; 4]) {
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
