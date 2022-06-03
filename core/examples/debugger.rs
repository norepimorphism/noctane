// SPDX-License-Identifier: MPL-2.0

#![feature(slice_as_chunks)]

use std::{collections::HashSet, io::Write as _};

fn main() {
    tracing_subscriber::fmt()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .with_ansi(true)
        .with_level(true)
        .with_target(false)
        .without_time()
        .init();

    let bios_filepath = std::env::args()
        .nth(1)
        .expect("expected ROM filepath");
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
        Self {
            cpu: core.cpu(),
            breakpoints: HashSet::new(),
        }
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
            std::io::stdin()
                .read_line(&mut input)
                .expect("failed to read input");

            let mut args = input.split_ascii_whitespace();
            if let Some(cmd) = args.next() {
                self.process_command(cmd, args);
            } else {
                println!("expected command");
            }
        }
    }

    fn process_command<'a>(&mut self, cmd: &str, args: impl Iterator<Item = &'a str>) {
        let result = match cmd {
            "+b" => self.do_add_breakpoint(args),
            "c" | "cl" => {
                self.do_continue(false);

                Ok(())
            }
            "cq" => {
                self.do_continue(true);

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
            "s" | "sl" => {
                self.do_step_loudly(None);

                Ok(())
            }
            "sq" => {
                self.do_step_quietly();

                Ok(())
            }
            _ => Err("invalid command"),
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
        let addr =
            u32::from_str_radix(addr, 16).map_err(|_| "failed to parse breakpoint address")?;
        self.breakpoints.insert(addr);

        Ok(())
    }

    fn do_continue(&mut self, be_quiet: bool) {
        loop {
            let pc = self.cpu.reg().pc();
            let mut hit_breakpoint = self.breakpoints.get(&pc).is_some();

            if be_quiet {
                self.do_step_quietly();
            } else {
                self.do_step_loudly(Some(&mut hit_breakpoint));
            }

            if hit_breakpoint {
                println!("BREAK @ {:#010x}", pc);
                break;
            }
        }
    }

    fn do_print_reg(&self) {
        println!("{}", self.cpu.reg());
    }

    fn do_print_i_cache(&self) {
        println!("{}", self.cpu.mem().cache().i);
    }

    fn do_step_loudly(&mut self, hit_breakpoint: Option<&mut bool>) {
        let fetched = self.cpu.execute_next_instr();
        print!("{:08x}   {}", fetched.addr, fetched.instr.asm());

        let comment = match fetched.instr {
            noctane_cpu::Instr::Break(_) => {
                if let Some(it) = hit_breakpoint {
                    *it = true;
                }

                // See RM[A-21].
                let code = (fetched.op >> 6) & ((1 << 21) - 1);
                let comment = noctane_util::sym::for_break(code)
                    .map(|sym| format!("{}()", sym))
                    .unwrap_or_else(|| format!("{:#05x}", code));

                Some(comment)
            }
            noctane_cpu::Instr::Syscall(_) => {
                // Sony stores a code identifying the function of the syscall in the *r4* register.
                let code = self.cpu.reg().gpr(4);

                Some(noctane_util::sym::for_syscall(code).to_string())
            }
            _ => None,
        };
        if let Some(comment) = comment {
            print!("\t\t; {}", comment);
        }

        println!();
    }

    fn do_step_quietly(&mut self) {
        self.cpu.execute_next_instr();
    }
}
