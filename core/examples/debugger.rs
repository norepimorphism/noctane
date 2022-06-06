// SPDX-License-Identifier: MPL-2.0

#![feature(let_else, slice_as_chunks)]

use std::{collections::HashSet, io::Write as _};

fn main() {
    setup_tracing();

    let bios_filepath = std::env::args()
        // Skip the executable path argument and retrieve the first 'real' argument.
        .nth(1)
        .expect("expected ROM filepath");
    let bios = std::fs::read(bios_filepath).expect("failed to read ROM");

    let mut core = noctane::Core::default();

    // Fill BIOS with the ROM image.
    for (idx, instr) in bios.as_chunks::<4>().0.into_iter().enumerate() {
        // The PSX CPU is little-endian, so we must make sure that if the host platform is
        // big-endian, the bytes are swapped before being written.
        core.banks_mut().bios[idx] = u32::from_le_bytes(*instr);
    }

    Debugger::new(&mut core).run();
}

fn setup_tracing() {
    tracing_subscriber::fmt()
        // Set the environment variable `RUST_LOG` to one of `TRACE`, `DEBUG`, `INFO`, `WARN`, or
        // `ERROR`. Reading from the environment saves us from writing additional code to parse
        // verbosity flags.
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .with_ansi(true)
        .with_level(true)
        // The target is mostly just noise, I think.
        .with_target(false)
        // Timestamps are mostly noise as well.
        .without_time()
        .init();
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
            // For the above symbol to display before user input is expected, we must flush standard
            // output.
            let _ = std::io::stdout().lock().flush();

            let mut input = String::new();
            std::io::stdin()
                .read_line(&mut input)
                .expect("failed to read input");

            // As if this is a real shell, we will split the input by whitespace and consider the
            // first token as the command name and the remaining tokens as arguments.
            let mut args = input.split_ascii_whitespace();
            if let Some(cmd) = args.next() {
                self.process_command(cmd, args);
            } else {
                println!("expected command");
            }
        }
    }

    fn process_command<'a>(&mut self, cmd: &str, args: impl Iterator<Item = &'a str>) {
        // The naming convention for commands is follows:
        //
        // - If `_` is being printed, the command is named `p_`.
        // - If `_` is being added, the format is `+_` ; conversely, if `_` is being removed, the
        //   format is `-_`.
        // - Continuation commands begin with `c` and are optionally followed by `l` or `s`, which
        //   stand for 'lightly' and 'silently', respectively.
        // - Step commands begin with `s` and are optionally followed by `l` or `s`, which are
        //   equivalent in meaning to those defined for continuation commands.

        // Functions starting with the prefix `do_` represent commands that may or may not consume
        // arguments, but which are nonetheless irrelevant to more general usage outside command
        // invocation. As fallible commands, these functions always return `Result<(), &str>`. They
        // may or may not accept an `args` argument.

        let result = match cmd {
            "+b"    => self.do_add_breakpoint(args),
            "-b"    => self.do_remove_breakpoint(args),
            "c"     => self.do_continue(|this| this.step()),
            "cl"    => self.do_continue(|this| this.step_lightly()),
            "cs"    => self.do_continue(|this| this.step_silently()),
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
        args: impl Iterator<Item = &'a str>,
    ) -> Result<(), &'static str> {
        let addr = Self::get_breakpoint_addr(args)?;
        self.breakpoints.insert(addr);

        Ok(())
    }

    fn do_remove_breakpoint<'a>(
        &mut self,
        args: impl Iterator<Item = &'a str>,
    ) -> Result<(), &'static str> {
        let addr = Self::get_breakpoint_addr(args)?;
        self.breakpoints.remove(&addr);

        Ok(())
    }

    fn get_breakpoint_addr<'a>(
        mut args: impl Iterator<Item = &'a str>,
    ) -> Result<u32, &'static str> {
        let addr = args.next().ok_or("expected breakpoint address")?;

        Self::parse_hex_arg(addr).ok_or("failed to parse breakpoint address")
    }

    fn parse_hex_arg(arg: &str) -> Option<u32> {
        u32::from_str_radix(arg, 16).ok()
    }

    fn do_continue(
        &mut self,
        mut step: impl FnMut(&mut Self) -> noctane_cpu::instr::Executed,
    ) -> Result<(), &str> {
        loop {
            let execed = step(self);
            if self.breakpoints.get(&execed.fetched.addr).is_some() {
                println!("BREAK @ {:#010x}", execed.fetched.addr);
                return Ok(());
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
                self.print_exception(execed);
            } else {
                self.print_jump(target_addr);
            }
        }
    }

    fn print_exception(&mut self, execed: &noctane_cpu::instr::Executed) {
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
                } else {
                    println!("break_{:05x}()", code);
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
    }

    fn print_jump(&mut self, target_addr: u32) {
        macro_rules! print_table_call {
            ($table_name:ident $fn_name:ident) => {
                {
                    use noctane_util::bios::func;

                    // We just queued a jump. Many (all?) table jumps set *r9* in the delay slot,
                    // so we must step before we can see the final value of *r9* going into the
                    // target location.
                    self.step_silently();
                    // Sony stores the offset of the table function pointer in the *r9* register.
                    let offset = self.cpu.reg().gpr(9) as u8;

                    if let Some(call) = func::Call::$fn_name(&mut self.cpu, offset) {
                        if matches!(call.name, "std_out_putchar") {
                            let func::ArgumentValue::Char(c) = call.args[0].value else {
                                panic!("std_out_putchar() is borked");
                            };

                            self.stdout.push(c);
                        }

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
