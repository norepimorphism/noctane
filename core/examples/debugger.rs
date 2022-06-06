// SPDX-License-Identifier: MPL-2.0

#![feature(let_else, slice_as_chunks)]

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
            stdout: String::new(),
        }
    }
}

struct Debugger<'a> {
    cpu: noctane_cpu::Cpu<'a, 'a>,
    breakpoints: HashSet<u32>,
    stdout: String,
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
            "pi" => {
                self.print_i_cache();

                Ok(())
            }
            "pr" => {
                self.print_reg();

                Ok(())
            }
            "p!" => {
                self.print_stdout();

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
        mut step: impl FnMut(&mut Self) -> noctane_cpu::instr::Executed) {
        loop {
            let execed = step(self);
            if self.breakpoints.get(&execed.fetched.addr).is_some() {
                println!("BREAK @ {:#010x}", execed.fetched.addr);
                break;
            }
        }
    }

    fn print_i_cache(&self) {
        println!("{}", self.cpu.mem().cache().i);
    }

    fn print_reg(&self) {
        println!("{}", self.cpu.reg());
    }

    fn print_stdout(&self) {
        println!("{}", self.stdout);
    }

    fn step(&mut self) -> noctane_cpu::instr::Executed {
        let execed = self.cpu.execute_next_instr();
        self.print_fetched(&execed.fetched);
        self.print_executed(&execed);

        execed
    }

    fn step_lightly(&mut self) -> noctane_cpu::instr::Executed {
        let execed = self.cpu.execute_next_instr();
        self.print_executed(&execed);

        execed
    }

    fn step_silently(&mut self) -> noctane_cpu::instr::Executed {
        self.cpu.execute_next_instr()
    }

    fn print_fetched(&self, fetched: &noctane_cpu::instr::Fetched) {
        println!("{:08x}   {}", fetched.addr, fetched.instr.asm());
    }

    fn print_executed(&mut self, execed: &noctane_cpu::instr::Executed) {
        if let noctane_cpu::instr::PcBehavior::Jumps {
            kind,
            target_addr,
        } = execed.pc_behavior {
            if matches!(kind, noctane_cpu::instr::JumpKind::Exception) {
                let exc = self.cpu.last_exception();
                match exc.code {
                    noctane_cpu::exc::code::BREAKPOINT => {
                        // See RM[A-21].
                        let code = (execed.fetched.op >> 6) & ((1 << 21) - 1);
                        if let Some(call) = noctane_util::bios::func::Call::try_from_break(
                            &mut self.cpu,
                            code,
                        ) {
                            println!("{}", call);
                        }
                    }
                    noctane_cpu::exc::code::SYSCALL => {
                        // Sony stores a code identifying the function of the syscall in the *r4*
                        // register.
                        let code = self.cpu.reg().gpr(4);
                        println!(
                            "{}",
                            noctane_util::bios::func::Call::from_syscall(&self.cpu, code),
                        );
                    }
                    _ => {
                        println!("!!! EXCEPTION !!!");
                    }
                }
            } else {
                macro_rules! print_table_call {
                    ($table_name:ident $fn_name:ident) => {
                        {
                            self.step_silently();
                            let offset = self.cpu.reg().gpr(9) as u8;
                            if let Some(call) = noctane_util::bios::func::Call::$fn_name(&mut self.cpu, offset) {
                                println!("{}", call);
                            } else {
                                println!(concat!(stringify!($table_name), "_off_{:02x}()"), offset);
                            }
                        }
                    };
                }

                match target_addr {
                    0xa0 => print_table_call!(a0 in_a0_table),
                    0xb0 => print_table_call!(b0 in_b0_table),
                    0xc0 => print_table_call!(c0 in_c0_table),
                    _ => {
                        // This is consistent with IDA symbol naming.
                        // println!("sub_{:08X}()", target_addr);
                    }
                }
            }
        }
    }
}
