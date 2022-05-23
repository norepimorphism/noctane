#![feature(slice_as_chunks)]

use noctane_cpu::instr::Instr;

fn main() {
    let rom_filepath = std::env::args().nth(1).expect("expected ROM filepath");
    let rom = std::fs::read(rom_filepath).expect("failed to read ROM");

    rom
        .as_chunks()
        .0
        .into_iter()
        .enumerate()
        .for_each(|(i, bytes)| {
            let enc_instr = u32::from_le_bytes(*bytes);

            print!(
                "{:08x}   {}   {}",
                i * 4,
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
            );

            if let Some(instr) = Instr::decode(enc_instr) {
                println!("   {}", instr.asm())
            } else {
                println!();
            }
        });
}
