#![feature(slice_as_chunks)]

fn main() {
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::DEBUG)
        .with_ansi(true)
        .with_level(true)
        .without_time()
        .init();

    let rom_filepath = std::env::args().nth(1).expect("expected ROM filepath");
    let rom = std::fs::read(rom_filepath).expect("failed to read ROM");

    let mut core = noctane::Core::new();

    // Fill instruction cache with first 256 instructions of ROM.
    for addr in 0..=255u8 {
        let instr = u32::from_le_bytes(rom.as_chunks::<4>().0[usize::from(addr)]);
        let addr = 0xffff_0000 | (u32::from(addr) * 4);
        core.cpu_mut().write_phys_i_mem(addr, instr);
    }

    for _ in 0..256 {
        let pc = core.cpu_mut().reg().pc();
        let enc_instr = core.cpu_mut().read_phys_i_mem(pc);
        let enc_instr_bytes = enc_instr.to_be_bytes();

        print!(
            "{:08x}   {}   {}",
            pc,
            enc_instr_bytes
                .iter()
                .map(|byte| format!("{:02x}", byte))
                .collect::<Vec<String>>()
                .join(" "),
                enc_instr_bytes
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
        if let Some(instr) = noctane_cpu::Instr::decode(enc_instr) {
            println!("   {}", instr.asm());
        } else {
            println!();
        }

        core.cpu_mut().execute_next_instr();
    }

    println!("{}", core.cpu().reg());
}
