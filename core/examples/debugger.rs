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
        core.banks.bios[idx] = u32::from_le_bytes(*instr);
    }

    Debugger::new(core).run();
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

impl Debugger {
    fn new(core: noctane::Core) -> Self {
        Self {
            core,
            addr_breakpoints: HashSet::new(),
            game_window: noctane_util::game::Window::new(640, 480),
            sym_breakpoints: HashSet::new(),
            stdout: String::new(),
        }
    }
}

struct Debugger {
    core: noctane::Core,
    addr_breakpoints: HashSet<u32>,
    game_window: noctane_util::game::Window,
    sym_breakpoints: HashSet<String>,
    stdout: String,
}

impl Debugger {
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
            "c"     => self.do_continue(|this, step| {
                this.print_fetched(&step.execed.fetched);
                this.print_step(step);
            }),
            "cl"    => self.do_continue(|this, step| {
                this.print_step(step);
            }),
            "cs"    => self.do_continue(|_, _| {
                // Do nothing. We're silent, after all.
            }),
            "pic"   => self.do_print_i_cache(),
            "pio"   => self.do_print_io(),
            "pr"    => self.do_print_reg(),
            "p!"    => self.do_print_stdout(),
            "s"     => self.do_step(),
            "sl"    => self.do_step_lightly(),
            "ss"    => self.do_step_silently(),
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
        self.edit_breakpoints(
            args,
            |this, arg| {
                this.do_add_addr_breakpoint(arg)
            },
            |this, arg| {
                this.do_add_sym_breakpoint(arg)
            },
        )
    }

    fn do_remove_breakpoint<'a>(
        &mut self,
        args: impl Iterator<Item = &'a str>,
    ) -> Result<(), &'static str> {
        self.edit_breakpoints(
            args,
            |this, arg| {
                this.do_remove_addr_breakpoint(arg)
            },
            |this, arg| {
                this.do_remove_sym_breakpoint(arg)
            },
        )
    }

    fn edit_breakpoints<'a>(
        &mut self,
        mut args: impl Iterator<Item = &'a str>,
        mut do_addr: impl FnMut(&mut Self, &'a str) -> Result<(), &'static str>,
        mut do_sym: impl FnMut(&mut Self, &'a str) -> Result<(), &'static str>,
    ) -> Result<(), &'static str> {
        let arg = args.next().ok_or("expected breakpoint")?;
        let c = arg.chars().nth(0).expect("argument is empty");

        if c.is_ascii_hexdigit() {
            do_addr(self, arg)
        } else {
            do_sym(self, arg)
        }
    }

    fn do_add_addr_breakpoint(&mut self, arg: &str) -> Result<(), &'static str> {
        let addr = Self::get_breakpoint_addr(arg)?;
        self.addr_breakpoints.insert(addr);

        Ok(())
    }

    fn do_remove_addr_breakpoint(&mut self, arg: &str) -> Result<(), &'static str> {
        let addr = Self::get_breakpoint_addr(arg)?;
        self.addr_breakpoints.remove(&addr);

        Ok(())
    }

    fn get_breakpoint_addr(arg: &str) -> Result<u32, &'static str> {
        Self::parse_hex_arg(arg).ok_or("failed to parse breakpoint address")
    }

    fn parse_hex_arg(arg: &str) -> Option<u32> {
        u32::from_str_radix(arg, 16).ok()
    }

    fn do_add_sym_breakpoint(&mut self, arg: &str) -> Result<(), &'static str> {
        self.sym_breakpoints.insert(arg.to_string());

        Ok(())
    }

    fn do_remove_sym_breakpoint<'a>(&mut self, arg: &str) -> Result<(), &'static str> {
        self.sym_breakpoints.remove(arg);

        Ok(())
    }

    fn do_continue(
        &mut self,
        mut handle_step: impl FnMut(&mut Self, &Step),
    ) -> Result<(), &str> {
        loop {
            let step = self.step();
            handle_step(self, &step);

            let should_break_on_addr = self.addr_breakpoints
                .get(&step.execed.fetched.addr)
                .is_some();
            if  step.should_break || should_break_on_addr {
                Self::print_break(&step.execed);
                return Ok(());
            }
        }
    }

    fn print_break(execed: &noctane_cpu::instr::Executed) {
        println!("BREAK @ {:#010x}", execed.fetched.addr);
    }

    fn do_print_i_cache(&self) -> Result<(), &'static str> {
        println!("{}", self.core.cpu_state.cache.i);

        Ok(())
    }

    fn do_print_io(&mut self) -> Result<(), &'static str> {
        println!("{:#?}", self.core.cpu().mem().bus().io);

        Ok(())
    }

    fn do_print_reg(&self) -> Result<(), &'static str> {
        println!("{}", self.core.cpu_state.reg);

        Ok(())
    }

    fn do_print_stdout(&self) -> Result<(), &'static str> {
        println!("{}", self.stdout);

        Ok(())
    }

    fn do_step(&mut self) -> Result<(), &'static str> {
        let step = self.step();
        self.print_fetched(&step.execed.fetched);
        self.print_step(&step);

        Ok(())
    }

    fn do_step_lightly(&mut self) -> Result<(), &'static str> {
        let step = self.step();
        self.print_step(&step);

        Ok(())
    }

    fn do_step_silently(&mut self) -> Result<(), &'static str> {
        let _ = self.step();

        Ok(())
    }
}

struct Step {
    execed: noctane_cpu::instr::Executed,
    jump: Option<Jump>,
    should_break: bool,
}

enum Jump {
    Exception(noctane_cpu::Exception),
    Normal {
        fn_name: Option<String>,
    }
}

impl Debugger {
    fn step(&mut self) -> Step {
        let execed = self.core.cpu().execute_next_instr();
        self.core.update_io();
        //self.game_window.gfx_mut().render();
        // self.game_window.update();

        if let noctane_cpu::instr::PcBehavior::Jumps {
            kind,
            target_addr,
        } = execed.pc_behavior {
            if matches!(kind, noctane_cpu::instr::JumpKind::Exception) {
                Step {
                    execed,
                    jump: Some(Jump::Exception(self.core.cpu_state.reg.last_exception())),
                    should_break: true,
                }
            } else {
                let jump = self.handle_normal_jump(kind, target_addr);

                Step {
                    execed,
                    jump: Some(Jump::Normal { fn_name: jump.fn_name }),
                    should_break: jump.should_break,
                }
            }
        } else {
            Step { execed, jump: None, should_break: false }
        }
    }
}

struct NormalJump {
    fn_name: Option<String>,
    should_break: bool,
}

impl Debugger {
    fn handle_normal_jump(
        &mut self,
        kind: noctane_cpu::instr::JumpKind,
        target_addr: u32,
    ) -> NormalJump {
        let mut fn_name = None;
        let mut should_break = false;

        macro_rules! handle_table_call {
            ($table_name:ident $fn_name:ident) => {
                {
                    use noctane_util::bios::func;

                    // We just queued a jump. Many (all?) table jumps set *r9* in the delay slot,
                    // so we must step before we can see the final value of *r9* going into the
                    // target location.
                    // TODO: What if the instruction in the delay slot causes an exception or
                    // triggers a breakpoint?
                    // TODO: This will always be silent.
                    self.step();
                    // Sony stores the offset of the table function pointer in the *r9* register.
                    let offset = self.core.cpu_state.reg.gpr(9) as u8;

                    if let Some(call) = func::Call::$fn_name(self, offset) {
                        if self.sym_breakpoints.get(call.name).is_some() {
                            should_break = true;
                        }

                        match call.name {
                            "std_out_putchar" => {
                                let func::ArgumentValue::Char(c) = call.args[0].value else {
                                    panic!("std_out_putchar() is borked");
                                };
                                self.stdout.push(c);
                            }
                            // `std_out_puts` doesn't call `std_out_putchar`, so we need to handle
                            // it separately.
                            "std_out_puts" => {
                                let func::ArgumentValue::String(ref s) = call.args[0].value else {
                                    panic!("std_out_puts() is borked");
                                };
                                self.stdout.push_str(s);
                            }
                            _ => {}
                        }

                        fn_name = Some(format!("{}", call));
                    } else {
                        fn_name = Some(format!(
                            concat!(stringify!($table_name),
                            "_off_{:02x}()"),
                            offset,
                        ));
                    }
                }
            };
        }

        match target_addr {
            0xa0 => handle_table_call!(a0 in_a0_table),
            0xb0 => handle_table_call!(b0 in_b0_table),
            0xc0 => handle_table_call!(c0 in_c0_table),
            _ => {
                if matches!(kind, noctane_cpu::instr::JumpKind::Call) {
                    // This is consistent with IDA symbol naming.
                    fn_name = Some(format!("sub_{:08X}()", target_addr));
                }
            }
        }

        NormalJump { fn_name, should_break }
    }

    fn print_fetched(&self, fetched: &noctane_cpu::instr::Fetched) {
        println!("{:08x}   {}", fetched.addr, fetched.instr.asm());
    }

    fn print_step(&mut self, step: &Step) {
        if let Some(ref jump) = step.jump {
            match jump {
                Jump::Exception(exc) => {
                    self.print_exception(&step.execed, exc);
                }
                Jump::Normal { fn_name } => {
                    if let Some(fn_name) = fn_name {
                        println!("{}", fn_name);
                    }
                }
            }
        }
    }

    fn print_exception(
        &mut self,
        execed: &noctane_cpu::instr::Executed,
        exc: &noctane_cpu::Exception,
    ) {
        match exc.code {
            noctane_cpu::exc::code::INTERRUPT => {
                println!("!!! INTERRUPT !!!")
            }
            noctane_cpu::exc::code::BREAKPOINT => {
                // See RM[A-21].
                let code = (execed.fetched.op >> 6) & ((1 << 21) - 1);
                if let Some(call) = noctane_util::bios::func::Call::try_from_break(
                    self,
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
                let code = self.core.cpu_state.reg.gpr(4);
                println!(
                    "{}",
                    noctane_util::bios::func::Call::from_syscall(self, code),
                );
            }
            _ => {
                println!("!!! EXCEPTION !!!");
                println!("Code: {:#}", exc.code)
            }
        }
    }
}

impl noctane_util::bios::func::CallSource for Debugger {
    fn read_raw_arg(&mut self, index: usize) -> u32 {
        let reg = &self.core.cpu_state.reg;

        if index < 6 {
            reg.gpr(4 + index)
        } else {
            let sp = reg.gpr(29);

            // TODO: We should probably handle the error somehow.
            self.core.cpu().mem_mut().read_32(sp + ((index * 4) as u32)).unwrap_or(0)
        }
    }

    fn read_8(&mut self, addr: u32) -> u8 {
        self.core.cpu().mem_mut().read_8(addr).unwrap_or(0)
    }

    fn read_16(&mut self, addr: u32) -> u16 {
        self.core.cpu().mem_mut().read_16(addr).unwrap_or(0)
    }

    fn read_32(&mut self, addr: u32) -> u32 {
        self.core.cpu().mem_mut().read_32(addr).unwrap_or(0)
    }
}
