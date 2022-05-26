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

    // Fill memory with ROM.
    for (addr, instr) in rom.as_chunks::<4>().0.into_iter().enumerate() {
        let instr = u32::from_le_bytes(*instr);
        let addr = (addr * 4) as u32;
        core.cpu_mut().mmu_mut().write_32(addr, instr).unwrap();
    }

    loop {
        let pc = core.cpu_mut().reg().pc();
        let enc_instr = core.cpu_mut().mmu_mut().read_virt_32(pc).unwrap();
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
}
