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

        if let Some(ref state) = self.gp0_state {
            if self.gp0_queue.len() >= state.arg_count {
                self.gp0_state = None;
            }
            return;
        }

        if let Some(mach) = self.gp0_queue.dequeue() {
            let mach = MachineCommand::decode(mach);
            let cmd = Command::decode(mach);

            macro_rules! process_gp0_command {
                (
                    $(
                        {
                            pat: $pat:pat,
                            arg_count: $arg_count:tt,
                            fn: $fn:expr $(,)?
                        }
                    ),* $(,)?
                ) => {
                    match cmd {
                        $(
                            $pat => {
                                macro_rules! process_arg_count {
                                    (0) => {
                                        $fn()
                                    };
                                    ($_:tt) => {
                                        self.gp0_state = Some(cmd::gp0::State {
                                            arg_count: $arg_count,
                                        })
                                    };
                                }

                                process_arg_count!($arg_count)
                            }
                        )*
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
                    pat: Command::ClearTexCache,
                    arg_count: 0,
                    fn: || {
                        tracing::info!("Clear texture cache");
                    },
                },
                {
                    pat: Command::QuickfillRect,
                    arg_count: 2,
                    fn: || {
                        tracing::info!("Quick-fill rectangle");
                    },
                },
                {
                    pat: Command::X03,
                    arg_count: 0,
                    fn: || {
                        // TODO: Who knows what this does?
                    },
                },
                {
                    pat: Command::RequestInt,
                    arg_count: 0,
                    fn: || {
                        tracing::info!("Request IRQ1");
                    },
                },
                {
                    pat: Command::RenderUnshadedUntexturedPoly { .. },
                    arg_count: 3,
                    fn: || {
                        tracing::info!("Render unshaded untextured polygon");
                    },
                },
                {
                    pat: Command::RenderUnshadedTexturedPoly { .. },
                    arg_count: 6,
                    fn: || {
                        tracing::info!("Render unshaded textured polygon");
                    },
                },
                {
                    pat: Command::RenderShadedUntexturedPoly { .. },
                    arg_count: 5,
                    fn: || {
                        tracing::info!("Render shaded untextured polygon");
                    },
                },
                {
                    pat: Command::RenderShadedTexturedPoly { .. },
                    arg_count: 8,
                    fn: || {
                        tracing::info!("Render shaded textured polygon");
                    },
                },
                {
                    pat: Command::RenderUnshadedUntexturedLine { .. },
                    arg_count: 2,
                    fn: || {
                        tracing::info!("Render unshaded untextured line");
                    },
                },
                {
                    pat: Command::RenderUnshadedTexturedLine { .. },
                    arg_count: 0,
                    fn: || {
                        tracing::info!("Render unshaded textured line");
                    },
                },
                {
                    pat: Command::RenderUnshadedUntexturedPolyline { .. },
                    arg_count: 2,
                    fn: || {
                        tracing::info!("Render unshaded untextured polyline");
                    },
                },
                {
                    pat: Command::RenderUnshadedTexturedPolyline { .. },
                    arg_count: 0,
                    fn: || {
                        tracing::info!("Render unshaded textured polyline");
                    },
                },
                {
                    pat: Command::CopyRect,
                    arg_count: 3,
                    fn: || {
                        tracing::info!("Copy rectangle");
                    },
                },
                {
                    pat: Command::MoveRectToVram,
                    arg_count: 3,
                    fn: || {
                        tracing::info!("Copy rectangle to VRAM");
                    },
                },
                {
                    pat: Command::MoveRectToCpuBus,
                    arg_count: 3,
                    fn: || {
                        tracing::info!("Move rectangle to CPU");
                    },
                },
                {
                    pat: Command::SetTexpage,
                    arg_count: 0,
                    fn: || {
                        tracing::info!("Set texture page");
                    },
                },
                {
                    pat: Command::SetTexWindow,
                    arg_count: 0,
                    fn: || {
                        tracing::info!("Set texture window");
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
        // tracing::info!("GP1: {:?}", cmd);
    }
}
