#![feature(slice_as_chunks)]

fn main() {
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::TRACE)
        .with_ansi(true)
        .with_level(true)
        .with_target(false)
        .without_time()
        .init();

    let bios_filepath = std::env::args().nth(1).expect("expected ROM filepath");
    let bios = std::fs::read(bios_filepath).expect("failed to read ROM");

    let mut core = noctane::Core::default();

    // Fill memory with the BIOS.
    for (idx, instr) in bios.as_chunks::<4>().0.into_iter().enumerate() {
        core
            .banks_mut()
            .bios
            .as_chunks_mut::<4>()
            .0
            [idx] = u32::from_le_bytes(*instr).to_be_bytes();
    }

    let mut cpu = core.cpu();
    *cpu.reg_mut().pc_mut() = 0xbfc0_0000;

    loop {
        let pc = cpu.reg().pc();
        let last_exec = cpu.execute_next_instr();
        if let Some(exec) = last_exec {
            println!("{:08x}   {}", exec.pc, exec.instr.asm());
        }
    }
}
