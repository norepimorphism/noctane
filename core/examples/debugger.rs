#![feature(slice_as_chunks)]

use std::{collections::HashSet, io::Write as _};

fn main() {
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::DEBUG)
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
        core.banks_mut().bios[idx] = u32::from_le_bytes(*instr);
    }

    Debugger::new(&mut core).run();
}

impl<'a> Debugger<'a> {
    fn new(core: &'a mut noctane::Core) -> Self {
        Self { cpu: core.cpu(), breakpoints: HashSet::new() }
    }
}

struct Debugger<'a> {
    cpu: noctane_cpu::Cpu<'a, 'a>,
    breakpoints: HashSet<u32>,
}

impl Debugger<'_> {
    fn run(mut self) {
        loop {
            print!("> ");
            let _ = std::io::stdout().lock().flush();

            let mut input = String::new();
            std::io::stdin().read_line(&mut input).expect("failed to read input");

            let mut args = input.split_ascii_whitespace();
            if let Some(cmd) = args.next() {
                self.process_command(cmd, args);
            } else {
                println!("expected command");
            }
        }
    }

    fn process_command<'a>(
        &mut self,
        cmd: &str,
        args: impl Iterator<Item = &'a str>,
    ) {
        let result = match cmd {
            "+b" => {
                self.do_add_breakpoint(args)
            }
            "c" => {
                self.do_continue();

                Ok(())
            }
            "pr" => {
                self.do_print_reg();

                Ok(())
            }
            "px" => {
                self.do_print_i_cache();

                Ok(())
            }
            "s" => {
                self.do_step();

                Ok(())
            }
            _ => {
                Err("invalid command")
            }
        };

        if let Err(e) = result {
            println!("error: {}", e);
        }
    }

    fn do_add_breakpoint<'a>(
        &mut self,
        mut args: impl Iterator<Item = &'a str>,
    ) -> Result<(), &str> {
        let addr = args.next().ok_or("expected breakpoint address")?;
        let addr = u32::from_str_radix(addr, 16)
            .map_err(|_| "failed to parse breakpoint address")?;
        self.breakpoints.insert(addr);

        Ok(())
    }

    fn do_continue(&mut self) {
        loop {
            self.do_step();

            let pc = self.cpu.reg().pc();
            if self.breakpoints.get(&pc).is_some() {
                println!("BREAK @ {:#010x}", pc);
                break;
            }
        }
    }

    fn do_print_reg(&self) {
        println!("{}", self.cpu.reg());
    }

    fn do_print_i_cache(&self) {
        println!("{}", self.cpu.mmu().cache().i);
    }

    fn do_step(&mut self) {
        if let Some(exec) = self.cpu.execute_next_instr() {
            println!("{:08x}   {}", exec.pc, exec.instr.asm());
        }
    }
}
