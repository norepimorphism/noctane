// SPDX-License-Identifier: MPL-2.0

pub mod cmd;

use ringbuffer::{
    ConstGenericRingBuffer,
    RingBuffer as _,
    RingBufferRead as _,
    RingBufferWrite as _,
};

pub use cmd::MachineCommand;

#[derive(Clone, Copy, Debug, Default)]
pub struct Point {
    pub x: u16,
    pub y: u16,
}

#[derive(Clone, Copy, Debug, Default)]
pub struct Extent2d {
    pub width: u16,
    pub height: u16,
}

impl Default for Gpu {
    fn default() -> Self {
        Self {
            gp0_state: None,
            gp0_queue: ConstGenericRingBuffer::new(),
        }
    }
}

/// The PSX Graphics Processing Unit (GPU).
#[derive(Debug)]
pub struct Gpu {
    gp0_state: Option<cmd::gp0::State>,
    gp0_queue: ConstGenericRingBuffer<u32, 16>,
}

impl Gpu {
    pub fn queue_gp0_word(&mut self, word: u32) {
        // TODO: Some commands don't use queue space.
        self.gp0_queue.push(word);
    }

    pub fn execute_next_gp0_command(&mut self) {
        use cmd::gp0::Command;

        if let Some(mach) = self.gp0_queue.dequeue() {
            if let Some(ref state) = self.gp0_state {
                if self.gp0_queue.len() >= state.arg_count {
                    tracing::info!("GP0");
                    self.gp0_state = None;
                }
            }

            let mach = MachineCommand::decode(mach);
            let cmd = Command::decode(mach);

            macro_rules! process_gp0_command {
                (
                    $(
                        {
                            pat: $pat:pat,
                            arg_count: $arg_count:expr,
                            fn: $fn:expr $(,)?
                        }
                    ),* $(,)?
                ) => {
                    match cmd {
                        $(
                            $pat => {
                                macro_rules! process_arg_count {
                                    (0) => {
                                        tracing::info!("GP0: {:?}", cmd);

                                        $fn()
                                    };
                                    ($_:expr) => {
                                        self.gp0_state = Some(cmd::gp0::State {
                                            arg_count: $arg_count,
                                        })
                                    };
                                }

                                process_arg_count!($arg_count)
                            }
                        )*
                        _ => {
                            todo!()
                        }
                    }
                };
            }

            process_gp0_command!(
                {
                    pat: Command::Nop,
                    arg_count: 0,
                    fn: || {
                        // Do nothing.
                    },
                },
                {
                    pat: Command::RenderPoly { .. },
                    arg_count: 1,
                    fn: || {

                    },
                },
            );
        }
    }

    pub fn execute_gp1_machine_command(&mut self, mach: u32) {
        let mach = MachineCommand::decode(mach);
        self.execute_gp1_command(cmd::gp1::Command::decode(mach));
    }

    pub fn execute_gp1_command(&mut self, cmd: cmd::gp1::Command) {
        // TODO
        tracing::info!("GP1: {:?}", cmd);
    }
}
