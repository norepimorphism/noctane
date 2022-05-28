#![feature(slice_as_chunks)]

fn main() {
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::INFO)
        .with_ansi(true)
        .with_level(true)
        .without_time()
        .init();

    let bios_filepath = std::env::args().nth(1).expect("expected ROM filepath");
    let bios = std::fs::read(bios_filepath).expect("failed to read ROM");

    let mut core = noctane::Core::default();
    let mut cpu = core.cpu();

    // Fill memory with the BIOS.
    for (addr, instr) in bios.as_chunks::<4>().0.into_iter().enumerate() {
        let instr = u32::from_le_bytes(*instr);
        let addr = (addr * 4) as u32;
        cpu.mmu_mut().write_32(0xbfc0_0000 + addr, instr).unwrap();
    }

    *cpu.reg_mut().pc_mut() = 0xbfc0_0000;

    loop {
        let pc = cpu.reg().pc();
        let enc_instr = cpu.mmu_mut().read_virt_32(pc).unwrap();
        let enc_instr_bytes = enc_instr.to_le_bytes();

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

        cpu.execute_next_instr();
    }
}
