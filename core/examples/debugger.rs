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
            "c" => {
                self.do_continue(|this| this.step());

                Ok(())
            }
            "cl" => {
                self.do_continue(|this| this.step_lightly());

                Ok(())
            }
            "cs" => {
                self.do_continue(|this| this.step_silently());

                Ok(())
            }
            "pr" => {
                self.print_reg();

                Ok(())
            }
            "px" => {
                self.print_i_cache();

                Ok(())
            }
            "s" => {
                self.step();

                Ok(())
            }
            "sl" => {
                self.step_lightly();

                Ok(())
            }
            "ss" => {
                self.step_silently();

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

    fn do_continue(
        &mut self,
        mut step: impl FnMut(&mut Self,
    ) -> noctane_cpu::instr::Fetched) {
        loop {
            let fetched = step(self);
            let hit_breakpoint =
                matches!(fetched.instr, noctane_cpu::Instr::Break(_)) ||
                self.breakpoints.get(&fetched.addr).is_some();
            if hit_breakpoint {
                println!("BREAK @ {:#010x}", fetched.addr);
                break;
            }
        }
    }

    fn print_reg(&self) {
        println!("{}", self.cpu.reg());
    }

    fn print_i_cache(&self) {
        println!("{}", self.cpu.mem().cache().i);
    }

    fn step(&mut self) -> noctane_cpu::instr::Fetched {
        let fetched = self.cpu.execute_next_instr();
        self.print_instr(fetched);
        if let Some(comment) = self.gen_comment(fetched) {
            self.print_comment(comment);
        }
        println!();

        fetched
    }

    fn step_lightly(&mut self) -> noctane_cpu::instr::Fetched {
        let fetched = self.cpu.execute_next_instr();
        if let Some(comment) = self.gen_comment(fetched) {
            self.print_instr(fetched);
            self.print_comment(comment);
            println!();
        }

        fetched
    }

    fn step_silently(&mut self) -> noctane_cpu::instr::Fetched {
        self.cpu.execute_next_instr()
    }

    fn print_instr(&self, fetched: noctane_cpu::instr::Fetched) {
        print!("{:08x}   {}", fetched.addr, fetched.instr.asm());
    }

    fn print_comment(&self, comment: String) {
        print!("\t\t; {}", comment);
    }

    fn gen_comment(&self, fetched: noctane_cpu::instr::Fetched) -> Option<String> {
        match fetched.instr {
            noctane_cpu::Instr::Break(_) => {
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
                let comment = format!("{}()", noctane_util::sym::for_syscall(code));

                Some(comment)
            }
            _ => {
                let r9 = self.cpu.reg().gpr(9);
                let sym = match fetched.addr {
                    0xa0 => noctane_util::sym::for_a_func(r9),
                    0xb0 => noctane_util::sym::for_b_func(r9),
                    0xc0 => noctane_util::sym::for_c_func(r9),
                    _ => None,
                };

                sym.map(|it| format!("{}()", it))
            }
        }
    }
}
