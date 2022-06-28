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

#[derive(Clone, Debug)]
pub enum QueueStrategy {
    PushWord,
    BlitPixels {
        rem_pixels: u32,
    },
}

pub struct State {
    pub min_arg_count: usize,
    pub param: u32,
    pub execute: fn(&mut Gpu, u32),
}

impl fmt::Debug for State {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f
            .debug_struct("State")
            .finish()
    }
}

impl Gpu {
    pub fn queue_gp0_word(&mut self, word: u32) -> Result<(), ()> {
        match self.gp0_strat {
            QueueStrategy::PushWord => {
                self.push_gp0_word(word)
            }
            QueueStrategy::BlitPixels { ref mut rem_pixels } => {
                // There are two pixels in a word.
                *rem_pixels = rem_pixels.saturating_sub(2);
                if *rem_pixels == 0 {
                    self.gp0_strat = QueueStrategy::PushWord;
                }

                self.blit_pixels(word);

                Ok(())
            }
        }
    }

    fn push_gp0_word(&mut self, word: u32) -> Result<(), ()> {
        if self.gp0_queue.is_full() {
            Err(())
        } else {
            // TODO: Some commands don't use queue space.
            self.gp0_queue.push(word);
            tracing::warn!("{:08x}", word);

            Ok(())
        }
    }

    fn blit_pixels(&mut self, word: u32) {
        // TODO
    }

    pub fn execute_next_gp0_command(&mut self) {
        if let Some(state) = self.gp0_state.as_mut() {
            if self.gp0_queue.len() >= state.min_arg_count {
                let param = state.param;
                let execute = state.execute;
                self.gp0_state = None;

                // We now have enough arguments to execute the command.
                (execute)(self, param);
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
                                        $fn(self, cmd.param)
                                    };
                                    ($count:expr) => {
                                        {
                                            self.gp0_state = Some(State {
                                                min_arg_count: $count,
                                                param: cmd.param,
                                                execute: $fn,
                                            });
                                        }
                                    };
                                }

                                process_min_arg_count!($($min_arg_count)?)
                            }
                        )*
                        _ => {
                            tracing::error!("Unimplemented command: {:?}", cmd.kind);
                        }
                    }
                };
            }

            process_gp0_command!(
                {
                    name: Nop,
                    fn: |_: &mut Gpu, _: u32| {
                        // Do nothing.
                    },
                },
                {
                    name: ClearTexCache,
                    fn: |_: &mut Gpu, _: u32| {
                        // TODO
                    },
                },
                {
                    name: QuickfillRect,
                    fn: |this: &mut Gpu, color: u32| {
                        // TODO
                        let color = Color::decode(color);
                        tracing::info!("Color: {:?}", color);
                        this.gp0_queue.dequeue().unwrap();
                        this.gp0_queue.dequeue().unwrap();
                    },
                },
                {
                    name: X03,
                    fn: |_: &mut Gpu, _: u32| {
                        // TODO: Undocumented.
                    },
                },
                {
                    name: RenderUnshadedUntexturedOpaqueQuad,
                    min_arg_count: 4,
                    fn: |this: &mut Gpu, _: u32| {
                        let verts = this.decode_polygon::<4, false>();
                        this.gfx.draw_quad(verts);
                    },
                },
                {
                    name: RenderShadedUntexturedOpaqueTriangle,
                    min_arg_count: 5,
                    fn: |this: &mut Gpu, _: u32| {
                        let verts = this.decode_polygon::<3, true>();
                        this.gfx.draw_triangle(verts);
                    },
                },
                {
                    name: RenderShadedUntexturedOpaqueQuad,
                    min_arg_count: 7,
                    fn: |this: &mut Gpu, _: u32| {
                        let verts = this.decode_polygon::<4, true>();
                        this.gfx.draw_quad(verts);
                    },
                },
                {
                    name: RenderUntexturedOpaqueRect,
                    min_arg_count: 2,
                    fn: |this: &mut Gpu, _: u32| {
                        let verts = this.decode_rect::<false>();
                        this.gfx.draw_quad(verts);
                    },
                },
                {
                    name: RenderBlendedOpaqueRect,
                    min_arg_count: 3,
                    fn: |this: &mut Gpu, _: u32| {
                        let verts = this.decode_rect::<true>();
                        this.gfx.draw_quad(verts);
                    },
                },
                {
                    name: MoveRectToVram,
                    min_arg_count: 2,
                    fn: |this: &mut Gpu, _: u32| {
                        // TODO
                        let _ = this.gp0_queue.dequeue().unwrap();
                        let (width, height) = {
                            let mut size = this.gp0_queue.dequeue().unwrap();

                            (size.pop_bits(16), size)
                        };

                        let pixel_count = width * height;
                        this.gp0_strat = QueueStrategy::BlitPixels { rem_pixels: pixel_count };
                    },
                },
                {
                    name: MoveRectToCpuBus,
                    min_arg_count: 2,
                    fn: |this: &mut Gpu, _: u32| {
                        // TODO
                        this.gp0_queue.dequeue().unwrap();
                        this.gp0_queue.dequeue().unwrap();
                    },
                },
                {
                    name: SetTexpage,
                    fn: |_: &mut Gpu, _: u32| {
                        // TODO
                    },
                },
                {
                    name: SetTexWindow,
                    fn: |_: &mut Gpu, _: u32| {
                        // TODO
                    },
                },
                {
                    name: SetDrawingAreaTopLeft,
                    fn: |_: &mut Gpu, _: u32| {
                        // TODO
                    },
                },
                {
                    name: SetDrawingAreaBottomRight,
                    fn: |_: &mut Gpu, _: u32| {
                        // TODO
                    },
                },
                {
                    name: SetDrawingOffset,
                    fn: |_: &mut Gpu, _: u32| {
                        // TODO
                    },
                },
                {
                    name: SetMask,
                    fn: |_: &mut Gpu, _: u32| {
                        // TODO
                    },
                },
            );
        }
    }
}

pub enum Opacity {
    Opaque,
    Translucent,
}

impl Gpu {
    fn decode_polygon<const N: usize, const IS_SHADED: bool>(
        &mut self,
    ) -> [crate::gfx::Vertex; N] {
        let mut is_initial = true;
        [(); N].map(|_| {
            if IS_SHADED {
                if is_initial {
                    is_initial = false;
                } else {
                    // TODO: Shading color.
                    let _ = self.gp0_queue.dequeue().unwrap();
                }
            }
            let vert = crate::gfx::Vertex::decode(self.gp0_queue.dequeue().unwrap());

            vert
        })
    }

    fn decode_rect<const IS_TEXTURED: bool>(&mut self) -> [crate::gfx::Vertex; 4] {
        let top_left = crate::gfx::Vertex::decode(self.gp0_queue.dequeue().unwrap());
        if IS_TEXTURED {
            // TODO: Texture coordinate and palette.
            let _ = self.gp0_queue.dequeue().unwrap();
        }
        let size = crate::gfx::Vertex::decode(self.gp0_queue.dequeue().unwrap());

        let mut bottom_left = top_left;
        bottom_left.y += size.y;
        let mut top_right = top_left;
        top_right.x += size.x;
        let mut bottom_right = top_right;
        bottom_right.y += size.y;

        [
            top_left,
            bottom_left,
            top_right,
            bottom_right,
        ]
    }
}

pub struct Command {
    pub kind: CommandKind,
    pub param: u32,
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
    RenderUnshadedUntexturedOpaqueLinestrip,
    RenderUnshadedUntexturedOpaqueQuad,
    RenderUnshadedUntexturedTranslucentTriangle,
    RenderUnshadedUntexturedTranslucentLine,
    RenderUnshadedUntexturedTranslucentLinestrip,
    RenderUnshadedUntexturedTranslucentQuad,
    RenderUnshadedBlendedOpaqueTriangle,
    RenderUnshadedBlendedOpaqueLine,
    RenderUnshadedBlendedOpaqueLinestrip,
    RenderUnshadedBlendedOpaqueQuad,
    RenderUnshadedBlendedTranslucentTriangle,
    RenderUnshadedBlendedTranslucentLine,
    RenderUnshadedBlendedTranslucentLinestrip,
    RenderUnshadedBlendedTranslucentQuad,
    RenderUnshadedRawOpaqueTriangle,
    RenderUnshadedRawOpaqueLine,
    RenderUnshadedRawOpaqueLinestrip,
    RenderUnshadedRawOpaqueQuad,
    RenderUnshadedRawTranslucentTriangle,
    RenderUnshadedRawTranslucentLine,
    RenderUnshadedRawTranslucentLinestrip,
    RenderUnshadedRawTranslucentQuad,
    RenderShadedUntexturedOpaqueTriangle,
    RenderShadedUntexturedOpaqueLine,
    RenderShadedUntexturedOpaqueLinestrip,
    RenderShadedUntexturedOpaqueQuad,
    RenderShadedUntexturedTranslucentTriangle,
    RenderShadedUntexturedTranslucentLine,
    RenderShadedUntexturedTranslucentLinestrip,
    RenderShadedUntexturedTranslucentQuad,
    RenderShadedBlendedOpaqueTriangle,
    RenderShadedBlendedOpaqueLine,
    RenderShadedBlendedOpaqueLinestrip,
    RenderShadedBlendedOpaqueQuad,
    RenderShadedBlendedTranslucentTriangle,
    RenderShadedBlendedTranslucentLine,
    RenderShadedBlendedTranslucentLinestrip,
    RenderShadedBlendedTranslucentQuad,
    RenderShadedRawOpaqueTriangle,
    RenderShadedRawOpaqueLine,
    RenderShadedRawOpaqueLinestrip,
    RenderShadedRawOpaqueQuad,
    RenderShadedRawTranslucentTriangle,
    RenderShadedRawTranslucentLine,
    RenderShadedRawTranslucentLinestrip,
    RenderShadedRawTranslucentQuad,
    RenderUntexturedOpaqueDot,
    RenderUntexturedOpaqueRect,
    RenderUntexturedTranslucentDot,
    RenderUntexturedTranslucentRect,
    RenderBlendedOpaqueDot,
    RenderBlendedOpaqueRect,
    RenderBlendedTranslucentDot,
    RenderBlendedTranslucentRect,
    RenderRawOpaqueDot,
    RenderRawOpaqueRect,
    RenderRawTranslucentDot,
    RenderRawTranslucentRect,
    CopyRect,
    MoveRectToVram,
    MoveRectToCpuBus,
    SetTexpage,
    SetTexWindow,
    SetDrawingAreaTopLeft,
    SetDrawingAreaBottomRight,
    SetDrawingOffset,
    SetMask,
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
            param: mach.param,
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
            0x48 | 0x49 => Self::RenderUnshadedUntexturedOpaqueLinestrip,
            0x4a | 0x4b => Self::RenderUnshadedUntexturedTranslucentLinestrip,
            0x4c        => Self::RenderUnshadedBlendedOpaqueLinestrip,
            0x4d        => Self::RenderUnshadedRawOpaqueLinestrip,
            0x4e        => Self::RenderUnshadedBlendedTranslucentLinestrip,
            0x4f        => Self::RenderUnshadedRawTranslucentLinestrip,
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
            0x60 | 0x61 => Self::RenderUntexturedOpaqueRect,
            0x62 | 0x63 => Self::RenderUntexturedTranslucentRect,
            0x64        => Self::RenderBlendedOpaqueRect,
            0x65        => Self::RenderRawOpaqueRect,
            0x66        => Self::RenderBlendedTranslucentRect,
            0x67        => Self::RenderRawTranslucentRect,
            0x68 | 0x69 => Self::RenderUntexturedOpaqueDot,
            0x6a | 0x6b => Self::RenderUntexturedTranslucentDot,
            0x80..=0x9f => Self::CopyRect,
            0xa0..=0xbf => Self::MoveRectToVram,
            0xc0..=0xdf => Self::MoveRectToCpuBus,
            0xe0 => Self::Nop,
            0xe1 => Self::SetTexpage,
            0xe2 => Self::SetTexWindow,
            0xe3 => Self::SetDrawingAreaTopLeft,
            0xe4 => Self::SetDrawingAreaBottomRight,
            0xe5 => Self::SetDrawingOffset,
            0xe6 => Self::SetMask,
            0xe7..=0xef => Self::Nop,
            _ => todo!("Unknown opcode: {:#04x}", opcode),
        }
    }
}
