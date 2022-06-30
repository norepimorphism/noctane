// SPDX-License-Identifier: MPL-2.0

use std::fmt;

use noctane_util::BitStack as _;
use ringbuffer::{
    RingBuffer as _,
    RingBufferRead as _,
    RingBufferWrite as _,
};
use stackvec::StackVec;

use crate::{gfx::Vertex, Gpu};
use super::MachineCommand;

#[derive(Clone, Debug)]
pub enum QueueStrategy {
    PushWord,
    BlitPixels {
        rem_pixels: u32,
        top_left: Vertex,
        size: Vertex,
        data: Vec<u8>,
    },
}

pub struct State {
    pub arg_count: usize,
    pub param: u32,
    pub execute: fn(&mut Gpu, u32, Arguments),
}

pub type Arguments = StackVec<[u32; 15]>;

impl fmt::Debug for State {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f
            .debug_struct("State")
            .finish()
    }
}

impl Gpu {
    pub fn queue_gp0_word(&mut self, mut word: u32) -> Result<(), ()> {
        match self.gp0_strat {
            QueueStrategy::PushWord => {
                self.push_gp0_word(word)
            }
            QueueStrategy::BlitPixels {
                ref mut rem_pixels,
                top_left,
                size,
                ref mut data,
            } => {
                let pixel_1 = Self::convert_rgb5_to_rgba8(word.pop_bits(16) as u16);
                let pixel_0 = Self::convert_rgb5_to_rgba8(word as u16);
                data.extend(pixel_0);
                data.extend(pixel_1);

                // There are two pixels in a word.
                *rem_pixels = rem_pixels.saturating_sub(2);

                if *rem_pixels == 0 {
                    self.gfx.blit(top_left, size, data);
                    self.gp0_strat = QueueStrategy::PushWord;
                }

                Ok(())
            }
        }
    }

    fn convert_rgb5_to_rgba8(mut halfword: u16) -> [u8; 4] {
        let b = halfword.pop_bits(5);
        let g = halfword.pop_bits(5);
        let r = halfword.pop_bits(5);

        [
            r as u8,
            g as u8,
            b as u8,
            u8::MAX,
        ]
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

    pub fn execute_next_gp0_command(&mut self) {
        if let Some(state) = self.gp0_state.as_mut() {
            if self.gp0_queue.len() >= state.arg_count {
                // We now have enough arguments to execute the command.

                let param = state.param;
                let args = self.gp0_queue
                    .drain()
                    .take(state.arg_count)
                    .collect::<Arguments>();
                let execute = state.execute;
                self.gp0_state = None;

                (execute)(self, param, args);
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
                                arg_count: $arg_count:expr,
                            )?
                            fn: $fn:expr $(,)?
                        }
                    ),* $(,)?
                ) => {
                    match cmd.kind {
                        $(
                            CommandKind::$name => {
                                macro_rules! process_arg_count {
                                    () => {
                                        // If there aren't any arguments, we can skip some steps and
                                        // execute the command immediately.
                                        $fn(self, cmd.param, Arguments::new())
                                    };
                                    ($count:expr) => {
                                        {
                                            self.gp0_state = Some(State {
                                                arg_count: $count,
                                                param: cmd.param,
                                                execute: $fn,
                                            });
                                        }
                                    };
                                }

                                process_arg_count!($($arg_count)?)
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
                    fn: |_: &mut Gpu, _: u32, _: Arguments| {
                        // Do nothing.
                    },
                },
                {
                    name: ClearTexCache,
                    fn: |_: &mut Gpu, _: u32, _: Arguments| {
                        // TODO
                    },
                },
                {
                    name: QuickfillRect,
                    arg_count: 2,
                    fn: |_: &mut Gpu, _: u32, _: Arguments| {
                        // TODO
                    },
                },
                {
                    name: X03,
                    fn: |_: &mut Gpu, _: u32, _: Arguments| {
                        // TODO: Undocumented.
                    },
                },
                {
                    name: RenderUnshadedUntexturedOpaqueQuad,
                    arg_count: 4,
                    fn: |this: &mut Gpu, _: u32, args: Arguments| {
                        this.gfx.draw_quad(Self::decode_polygon::<4, false, false>(args));
                    },
                },
                {
                    name: RenderUnshadedBlendedOpaqueQuad,
                    arg_count: 8,
                    fn: |this: &mut Gpu, _: u32, args: Arguments| {
                        this.gfx.draw_quad(Self::decode_polygon::<4, false, true>(args));
                    },
                },
                {
                    name: RenderShadedUntexturedOpaqueTriangle,
                    arg_count: 5,
                    fn: |this: &mut Gpu, _: u32, args: Arguments| {
                        this.gfx.draw_triangle(Self::decode_polygon::<3, true, false>(args));
                    },
                },
                {
                    name: RenderShadedUntexturedOpaqueQuad,
                    arg_count: 7,
                    fn: |this: &mut Gpu, _: u32, args: Arguments| {
                        this.gfx.draw_quad(Self::decode_polygon::<4, true, false>(args));
                    },
                },
                {
                    name: RenderUntexturedOpaqueRect,
                    arg_count: 2,
                    fn: |this: &mut Gpu, _: u32, args: Arguments| {
                        this.gfx.draw_quad(Self::decode_rect::<false>(args));
                    },
                },
                {
                    name: RenderBlendedOpaqueRect,
                    arg_count: 3,
                    fn: |this: &mut Gpu, _: u32, args: Arguments| {
                        this.gfx.draw_quad(Self::decode_rect::<true>(args));
                    },
                },
                {
                    name: RenderUntexturedOpaqueBigSquare,
                    arg_count: 1,
                    fn: |this: &mut Gpu, _: u32, args: Arguments| {
                        this.gfx.draw_quad(Self::decode_big_square::<false>(args));
                    },
                },
                {
                    name: RenderBlendedTranslucentSmallSquare,
                    arg_count: 2,
                    fn: |this: &mut Gpu, _: u32, args: Arguments| {
                        this.gfx.draw_quad(Self::decode_small_square::<true>(args));
                    },
                },
                {
                    name: MoveRectToVram,
                    arg_count: 2,
                    fn: |this: &mut Gpu, _: u32, args: Arguments| {
                        let top_left = Vertex::decode(args[0]);
                        let size = Vertex::decode(args[1]);
                        this.gfx.draw_quad(Self::create_rect(top_left, size));

                        let pixel_count = u32::from(size.x) * u32::from(size.y);
                        this.gp0_strat = QueueStrategy::BlitPixels {
                            rem_pixels: pixel_count,
                            top_left,
                            size,
                            data: Vec::with_capacity(
                                std::mem::size_of::<u16>() * (pixel_count as usize)
                            ),
                        };
                    },
                },
                {
                    name: MoveRectToCpuBus,
                    arg_count: 2,
                    fn: |_: &mut Gpu, _: u32, _: Arguments| {
                        // TODO
                    },
                },
                {
                    name: SetTexpage,
                    fn: |_: &mut Gpu, _: u32, _: Arguments| {
                        // TODO
                    },
                },
                {
                    name: SetTexWindow,
                    fn: |_: &mut Gpu, _: u32, _: Arguments| {
                        // TODO
                    },
                },
                {
                    name: SetDrawingAreaTopLeft,
                    fn: |_: &mut Gpu, _: u32, _: Arguments| {
                        // TODO
                    },
                },
                {
                    name: SetDrawingAreaBottomRight,
                    fn: |_: &mut Gpu, _: u32, _: Arguments| {
                        // TODO
                    },
                },
                {
                    name: SetDrawingOffset,
                    fn: |_: &mut Gpu, _: u32, _: Arguments| {
                        // TODO
                    },
                },
                {
                    name: SetMask,
                    fn: |_: &mut Gpu, _: u32, _: Arguments| {
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
    fn decode_polygon<const N: usize, const IS_SHADED: bool, const IS_TEXTURED: bool>(
        args: Arguments,
    ) -> [Vertex; N] {
        let mut skip = 0;
        if IS_SHADED {
            skip += 1;
        }
        if IS_TEXTURED {
            skip += 1;
        }
        let mut vert_idx = 0;
        [(); N].map(|_| {
            let vert = Vertex::decode(args[vert_idx]);
            // Skip the shading argument.
            vert_idx += 1 + skip;

            vert
        })
    }

    fn decode_dot<const IS_TEXTURED: bool>(args: Arguments) -> [Vertex; 4] {
        Self::create_rect(Vertex::decode(args[0]), Vertex::new(1, 1))
    }

    fn decode_small_square<const IS_TEXTURED: bool>(args: Arguments) -> [Vertex; 4] {
        Self::create_rect(Vertex::decode(args[0]), Vertex::new(8, 8))
    }

    fn decode_big_square<const IS_TEXTURED: bool>(args: Arguments) -> [Vertex; 4] {
        Self::create_rect(Vertex::decode(args[0]), Vertex::new(16, 16))
    }

    fn decode_rect<const IS_TEXTURED: bool>(args: Arguments) -> [Vertex; 4] {
        Self::create_rect(Vertex::decode(args[0]), Vertex::decode(args[2]))
    }

    const fn create_rect(top_left: Vertex, size: Vertex) -> [Vertex; 4] {
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
    RenderUntexturedOpaqueSmallSquare,
    RenderUntexturedOpaqueBigSquare,
    RenderUntexturedOpaqueRect,
    RenderUntexturedTranslucentDot,
    RenderUntexturedTranslucentSmallSquare,
    RenderUntexturedTranslucentBigSquare,
    RenderUntexturedTranslucentRect,
    RenderBlendedOpaqueDot,
    RenderBlendedOpaqueSmallSquare,
    RenderBlendedOpaqueBigSquare,
    RenderBlendedOpaqueRect,
    RenderBlendedTranslucentDot,
    RenderBlendedTranslucentSmallSquare,
    RenderBlendedTranslucentBigSquare,
    RenderBlendedTranslucentRect,
    RenderRawOpaqueDot,
    RenderRawOpaqueSmallSquare,
    RenderRawOpaqueBigSquare,
    RenderRawOpaqueRect,
    RenderRawTranslucentDot,
    RenderRawTranslucentSmallSquare,
    RenderRawTranslucentBigSquare,
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
            0x6c        => Self::RenderBlendedOpaqueDot,
            0x6d        => Self::RenderRawOpaqueDot,
            0x6e        => Self::RenderBlendedTranslucentDot,
            0x6f        => Self::RenderRawTranslucentDot,
            0x70 | 0x71 => Self::RenderUntexturedOpaqueSmallSquare,
            0x72 | 0x73 => Self::RenderUntexturedTranslucentSmallSquare,
            0x74        => Self::RenderBlendedOpaqueSmallSquare,
            0x75        => Self::RenderRawOpaqueSmallSquare,
            0x76        => Self::RenderBlendedTranslucentSmallSquare,
            0x77        => Self::RenderRawTranslucentSmallSquare,
            0x78 | 0x79 => Self::RenderUntexturedOpaqueBigSquare,
            0x7a | 0x7b => Self::RenderUntexturedTranslucentBigSquare,
            0x7c        => Self::RenderBlendedOpaqueBigSquare,
            0x7d        => Self::RenderRawOpaqueBigSquare,
            0x7e        => Self::RenderBlendedTranslucentBigSquare,
            0x7f        => Self::RenderRawTranslucentBigSquare,
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
