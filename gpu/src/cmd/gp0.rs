// SPDX-License-Identifier: MPL-2.0

use std::fmt;

use noctane_util::BitStack as _;
use ringbuffer::{
    RingBuffer as _,
    RingBufferRead as _,
    RingBufferWrite as _,
};

use crate::Gpu;
use super::MachineCommand;

pub struct State {
    pub min_arg_count: usize,
    pub color: Color,
    pub execute: fn(&mut Gpu, Color),
}

impl fmt::Debug for State {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f
            .debug_struct("State")
            .finish()
    }
}

impl Gpu {
    pub fn queue_gp0_word(&mut self, word: u32) {
        // TODO: Some commands don't use queue space.
        self.gp0_queue.push(word);
        tracing::warn!("{:08x}", word);
    }

    pub fn execute_next_gp0_command(&mut self) {
        if let Some(state) = self.gp0_state.as_mut() {
            if self.gp0_queue.len() >= state.min_arg_count {
                let color = state.color;
                let execute = state.execute;
                self.gp0_state = None;

                // We now have enough arguments to execute the command.
                (execute)(self, color);
            }
        } else if let Some(mach) = self.gp0_queue.dequeue() {
            let mach = MachineCommand::decode(mach);
            tracing::info!("GP0({:02X}h)", mach.opcode);
            let cmd = Command::decode(mach);

            macro_rules! process_gp0_command {
                (
                    $(
                        {
                            name: $name:tt,
                            $(
                                min_arg_count: $min_arg_count:expr,
                            )?
                            fn: $fn:expr $(,)?
                        }
                    ),* $(,)?
                ) => {
                    match cmd.kind {
                        $(
                            CommandKind::$name => {
                                macro_rules! process_min_arg_count {
                                    () => {
                                        // If there aren't any arguments, we can skip some steps and
                                        // execute the command immediately.
                                        $fn(self, cmd.color)
                                    };
                                    ($count:expr) => {
                                        {
                                            self.gp0_state = Some(State {
                                                min_arg_count: $count,
                                                color: cmd.color,
                                                execute: $fn,
                                            });
                                        }
                                    };
                                }

                                process_min_arg_count!($($min_arg_count)?)
                            }
                        )*
                        _ => todo!(),
                    }
                };
            }

            process_gp0_command!(
                {
                    name: Nop,
                    fn: |_: &mut Gpu, _: Color| {
                        // Do nothing.
                    },
                },
                {
                    name: ClearTexCache,
                    fn: |_: &mut Gpu, _: Color| {
                        // TODO
                    },
                },
                {
                    name: QuickfillRect,
                    fn: |_: &mut Gpu, color: Color| {
                        // TODO
                        tracing::info!("Color: {:?}", color);
                    },
                },
                {
                    name: X03,
                    fn: |_: &mut Gpu, _: Color| {
                        todo!()
                    },
                },
                {
                    name: MoveRectToVram,
                    min_arg_count: 2,
                    fn: |this: &mut Gpu, _: Color| {
                        // TODO
                        this.gp0_queue.dequeue().unwrap();
                        this.gp0_queue.dequeue().unwrap();
                    },
                },
                {
                    name: MoveRectToCpuBus,
                    min_arg_count: 2,
                    fn: |this: &mut Gpu, _: Color| {
                        // TODO
                        this.gp0_queue.dequeue().unwrap();
                        this.gp0_queue.dequeue().unwrap();
                    },
                },
                {
                    name: SetTexpage,
                    fn: |_: &mut Gpu, _: Color| {
                        // TODO
                    },
                },
            );
        }
    }
}

pub struct Command {
    pub kind: CommandKind,
    pub color: Color,
}

#[derive(Clone, Copy, Debug)]
pub enum CommandKind {
    Nop,
    ClearTexCache,
    QuickfillRect,
    X03,
    RequestInt,
    RenderUnshadedUntexturedOpaqueTriangle,
    RenderUnshadedUntexturedOpaqueLine,
    RenderUnshadedUntexturedOpaqueQuad,
    RenderUnshadedUntexturedTranslucentTriangle,
    RenderUnshadedUntexturedTranslucentLine,
    RenderUnshadedUntexturedTranslucentQuad,
    RenderUnshadedBlendedOpaqueTriangle,
    RenderUnshadedBlendedOpaqueLine,
    RenderUnshadedBlendedOpaqueQuad,
    RenderUnshadedBlendedTranslucentTriangle,
    RenderUnshadedBlendedTranslucentLine,
    RenderUnshadedBlendedTranslucentQuad,
    RenderUnshadedRawOpaqueTriangle,
    RenderUnshadedRawOpaqueLine,
    RenderUnshadedRawOpaqueQuad,
    RenderUnshadedRawTranslucentTriangle,
    RenderUnshadedRawTranslucentLine,
    RenderUnshadedRawTranslucentQuad,
    RenderShadedUntexturedOpaqueTriangle,
    RenderShadedUntexturedOpaqueLine,
    RenderShadedUntexturedOpaqueQuad,
    RenderShadedUntexturedTranslucentTriangle,
    RenderShadedUntexturedTranslucentLine,
    RenderShadedUntexturedTranslucentQuad,
    RenderShadedBlendedOpaqueTriangle,
    RenderShadedBlendedOpaqueLine,
    RenderShadedBlendedOpaqueQuad,
    RenderShadedBlendedTranslucentTriangle,
    RenderShadedBlendedTranslucentLine,
    RenderShadedBlendedTranslucentQuad,
    RenderShadedRawOpaqueTriangle,
    RenderShadedRawOpaqueLine,
    RenderShadedRawOpaqueQuad,
    RenderShadedRawTranslucentTriangle,
    RenderShadedRawTranslucentLine,
    RenderShadedRawTranslucentQuad,
    CopyRect,
    MoveRectToVram,
    MoveRectToCpuBus,
    SetTexpage,
    SetTexWindow,
}

#[derive(Clone, Copy, Debug)]
pub struct Color {
    pub red: u8,
    pub green: u8,
    pub blue: u8,
}

impl Command {
    pub fn decode(mach: MachineCommand) -> Self {
        Self {
            kind: CommandKind::decode(mach.opcode),
            color: Color::decode(mach.param),
        }
    }
}

impl Color {
    pub fn decode(mut code: u32) -> Self {
        Self {
            red: code.pop_bits(8) as u8,
            green: code.pop_bits(8) as u8,
            blue: code.pop_bits(8) as u8,
        }
    }
}

impl CommandKind {
    pub fn decode(opcode: u8) -> Self {
        // Crossing my fingers that this gets optimized into a LUT...
        match opcode {
            0x00        => Self::Nop,
            0x01        => Self::ClearTexCache,
            0x02        => Self::QuickfillRect,
            0x03        => Self::X03,
            0x04..=0x1e => Self::Nop,
            0x1f        => Self::RequestInt,
            0x20 | 0x21 => Self::RenderUnshadedUntexturedOpaqueTriangle,
            0x22 | 0x23 => Self::RenderUnshadedUntexturedTranslucentTriangle,
            0x24        => Self::RenderUnshadedBlendedOpaqueTriangle,
            0x25        => Self::RenderUnshadedRawOpaqueTriangle,
            0x26        => Self::RenderUnshadedBlendedTranslucentTriangle,
            0x27        => Self::RenderUnshadedRawTranslucentTriangle,
            0x28 | 0x29 => Self::RenderUnshadedUntexturedOpaqueQuad,
            0x2a | 0x2b => Self::RenderUnshadedUntexturedTranslucentQuad,
            0x2c        => Self::RenderUnshadedBlendedOpaqueQuad,
            0x2d        => Self::RenderUnshadedRawOpaqueQuad,
            0x2e        => Self::RenderUnshadedBlendedTranslucentQuad,
            0x2f        => Self::RenderUnshadedRawTranslucentQuad,
            0x30 | 0x31 => Self::RenderShadedUntexturedOpaqueTriangle,
            0x32 | 0x33 => Self::RenderShadedUntexturedTranslucentTriangle,
            0x34        => Self::RenderShadedBlendedOpaqueTriangle,
            0x35        => Self::RenderShadedRawOpaqueTriangle,
            0x36        => Self::RenderShadedBlendedTranslucentTriangle,
            0x37        => Self::RenderShadedRawTranslucentTriangle,
            0x38 | 0x39 => Self::RenderShadedUntexturedOpaqueQuad,
            0x3a | 0x3b => Self::RenderShadedUntexturedTranslucentQuad,
            0x3c        => Self::RenderShadedBlendedOpaqueQuad,
            0x3d        => Self::RenderShadedRawOpaqueQuad,
            0x3e        => Self::RenderShadedBlendedTranslucentQuad,
            0x3f        => Self::RenderShadedRawTranslucentQuad,
            0x40 | 0x41 => Self::RenderUnshadedUntexturedOpaqueLine,
            0x42 | 0x43 => Self::RenderUnshadedUntexturedTranslucentLine,
            0x44        => Self::RenderUnshadedBlendedOpaqueLine,
            0x45        => Self::RenderUnshadedRawOpaqueLine,
            0x46        => Self::RenderUnshadedBlendedTranslucentLine,
            0x47        => Self::RenderUnshadedRawTranslucentLine,
            // 0x48 | 0x49 => Self::RenderUnshadedUntexturedPolyline {
            //     opacity: Opacity::Opaque,
            // },
            // 0x4a | 0x4b => Self::RenderUnshadedUntexturedPolyline {
            //     opacity: Opacity::Translucent,
            // },
            // 0x4c => Self::RenderUnshadedTexturedPolyline {
            //     texturing: Texturing::Blended(Color::decode(mach.param)),
            //     opacity: Opacity::Opaque,
            // },
            // 0x4d => Self::RenderUnshadedTexturedPolyline {
            //     texturing: Texturing::Raw,
            //     opacity: Opacity::Opaque,
            // },
            // 0x4e => Self::RenderUnshadedTexturedPolyline {
            //     texturing: Texturing::Blended(Color::decode(mach.param)),
            //     opacity: Opacity::Translucent,
            // },
            // 0x4f => Self::RenderUnshadedTexturedPolyline {
            //     texturing: Texturing::Raw,
            //     opacity: Opacity::Translucent,
            // },
            // 0x50 | 0x51 => Self::RenderShadedUntexturedLine {
            //     opacity: Opacity::Opaque,
            // },
            // 0x52 | 0x53 => Self::RenderShadedUntexturedLine {
            //     opacity: Opacity::Translucent,
            // },
            // 0x54 => Self::RenderShadedTexturedLine {
            //     texturing: Texturing::Blended(Color::decode(mach.param)),
            //     opacity: Opacity::Opaque,
            // },
            // 0x55 => Self::RenderShadedTexturedLine {
            //     texturing: Texturing::Raw,
            //     opacity: Opacity::Opaque,
            // },
            // 0x56 => Self::RenderShadedTexturedLine {
            //     texturing: Texturing::Blended(Color::decode(mach.param)),
            //     opacity: Opacity::Translucent,
            // },
            // 0x57 => Self::RenderShadedTexturedLine {
            //     texturing: Texturing::Raw,
            //     opacity: Opacity::Translucent,
            // },
            // 0x58 | 0x59 => Self::RenderShadedUntexturedPolyline {
            //     opacity: Opacity::Opaque,
            // },
            // 0x5a | 0x5b => Self::RenderShadedUntexturedPolyline {
            //     opacity: Opacity::Translucent,
            // },
            // 0x5c => Self::RenderShadedTexturedPolyline {
            //     texturing: Texturing::Blended(Color::decode(mach.param)),
            //     opacity: Opacity::Opaque,
            // },
            // 0x5d => Self::RenderShadedTexturedPolyline {
            //     texturing: Texturing::Raw,
            //     opacity: Opacity::Opaque,
            // },
            // 0x5e => Self::RenderShadedTexturedPolyline {
            //     texturing: Texturing::Blended(Color::decode(mach.param)),
            //     opacity: Opacity::Translucent,
            // },
            // 0x5f => Self::RenderShadedTexturedPolyline {
            //     texturing: Texturing::Raw,
            //     opacity: Opacity::Translucent,
            // },
            0x80..=0x9f => Self::CopyRect,
            0xa0..=0xbf => Self::MoveRectToVram,
            0xc0..=0xdf => Self::MoveRectToCpuBus,
            0xe0 => Self::Nop,
            0xe1 => Self::SetTexpage,
            0xe2 => Self::SetTexWindow,
            0xe7..=0xef => Self::Nop,
            _ => todo!("Unknown opcode: {:#04x}", opcode),
        }
    }
}
