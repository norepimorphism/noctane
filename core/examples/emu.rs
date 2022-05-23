fn main() {
    let mut core = noctane::Core::new();
    loop {
        let pc = core.cpu_mut().reg().pc();
        let fetched = core.mem_mut().read_32(pc);
        if let Some(next_instr) = noctane_cpu::Instr::decode(fetched) {
            println!("{}", next_instr.asm());
        }

        core.cpu_mut().execute_next_instr();
    }
}
