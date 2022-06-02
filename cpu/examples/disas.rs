// SPDX-License-Identifier: MPL-2.0

#![feature(slice_as_chunks)]

use noctane_cpu::instr::Instr;

fn main() {
    let rom_filepath = std::env::args()
        .nth(1)
        .expect("expected ROM filepath");
    let rom = std::fs::read(rom_filepath).expect("failed to read ROM");

    rom.as_chunks()
        .0
        .into_iter()
        .enumerate()
        .for_each(|(i, bytes)| {
            let enc_instr = u32::from_le_bytes(*bytes);
            noctane_util::dump_hex(&mut std::io::stdout(), i as u32, *bytes);
            if let Some(instr) = Instr::decode(enc_instr) {
                println!("   {}", instr.asm())
            } else {
                println!();
            }
        });
}
