// SPDX-License-Identifier: MPL-2.0

use std::fmt;

use noctane_util::BitStack as _;
use ringbuffer::{
    ConstGenericRingBuffer,
    RingBuffer as _,
    RingBufferRead as _,
    RingBufferWrite as _,
};
use stackvec::StackVec;

use crate::{gfx::{SampleStrategy, Vertex, VertexBufferEntry}, Gpu};
use super::MachineCommand;

impl Default for State {
    fn default() -> Self {
        Self {
            strat: Default::default(),
            vec: None,
            queue: ConstGenericRingBuffer::new(),
        }
    }
}

#[derive(Debug)]
pub struct State {
    pub strat: QueueStrategy,
    pub vec: Option<CommandVector>,
    pub queue: ConstGenericRingBuffer<u32, 16>,
}

#[derive(Clone, Debug, Default)]
pub enum QueueStrategy {
    #[default]
    PushWord,
    BlitPixels {
        rem_pixels: u32,
        top_left: Vertex,
        size: [u16; 2],
        data: Vec<u8>,
    },
}

pub struct CommandVector {
    pub arg_count: usize,
    pub param: u32,
    pub execute: fn(&mut Gpu, u32, Arguments),
}

pub type Arguments = StackVec<[u32; 15]>;

impl fmt::Debug for CommandVector {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f
            .debug_struct("CommandVector")
            .finish()
    }
}

impl Gpu {
    pub fn queue_gp0_word(&mut self, word: u32) -> Result<(), ()> {
        match self.gp0.strat {
            QueueStrategy::PushWord => {
                self.gp0.push_word(word)
            }
            QueueStrategy::BlitPixels {
                ref mut rem_pixels,
                top_left,
                size,
                ref mut data,
            } => {
                let bytes = word.to_be_bytes();
                let pixels = [
                    u16::from_be_bytes([bytes[0], bytes[1]]),
                    u16::from_be_bytes([bytes[2], bytes[3]]),
                ]
                .map(Self::decode_rgb5_to_rgba8);
                data.extend(pixels[1]);
                data.extend(pixels[0]);

                // There are two pixels in a word.
                *rem_pixels = rem_pixels.saturating_sub(2);

                if *rem_pixels == 0 {
                    self.gfx.blit(top_left, size, data);
                    self.gp0.strat = QueueStrategy::PushWord;
                }

                Ok(())
            }
        }
    }

    fn decode_rgb5_to_rgba8(mut code: u16) -> [u8; 4] {
        let mut pop_component = || code.pop_bits(5) as u8;

        [
            pop_component(),
            pop_component(),
            pop_component(),
            u8::MAX,
        ]
    }
}

impl State {
    fn push_word(&mut self, word: u32) -> Result<(), ()> {
        if self.queue.is_full() {
            Err(())
        } else {
            // TODO: Some commands don't use queue space.
            self.queue.push(word);
            tracing::warn!("{:08x}", word);

            Ok(())
        }
    }
}

impl Gpu {
    pub fn execute_next_gp0_command(&mut self) {
        if let Some(vec) = self.gp0.vec.as_mut() {
            if self.gp0.queue.len() >= vec.arg_count {
                // We now have enough arguments to execute the command.

                let param = vec.param;
                let args = self.gp0.queue
                    .drain()
                    .take(vec.arg_count)
                    .collect::<Arguments>();
                let execute = vec.execute;
                self.gp0.vec = None;

                (execute)(self, param, args);
            }
        } else if let Some(mach) = self.gp0.queue.dequeue() {
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
                                            self.gp0.vec = Some(CommandVector {
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
                    fn: |this: &mut Gpu, param: u32, args: Arguments| {
                        this.gfx.draw_quad(this.decode_poly_entries::<4>(args, param, false, None));
                    },
                },
                {
                    name: RenderUnshadedBlendedOpaqueQuad,
                    arg_count: 8,
                    fn: |this: &mut Gpu, param: u32, args: Arguments| {
                        this.gfx.draw_quad(this.decode_poly_entries::<4>(
                            args,
                            param,
                            false,
                            Some(Texturing::Blended),
                        ));
                    },
                },
                {
                    name: RenderShadedUntexturedOpaqueTriangle,
                    arg_count: 5,
                    fn: |this: &mut Gpu, param: u32, args: Arguments| {
                        this.gfx.draw_triangle(this.decode_poly_entries::<3>(args, param, true, None));
                    },
                },
                {
                    name: RenderShadedUntexturedOpaqueQuad,
                    arg_count: 7,
                    fn: |this: &mut Gpu, param: u32, args: Arguments| {
                        this.gfx.draw_quad(this.decode_poly_entries::<4>(args, param, true, None));
                    },
                },
                {
                    name: RenderUntexturedOpaqueDynamicRect,
                    arg_count: 2,
                    fn: |this: &mut Gpu, param: u32, args: Arguments| {
                        this.gfx.draw_quad(this.decode_rect_entries(
                            args,
                            param,
                            RectangleKind::Dynamic,
                            None,
                        ));
                    },
                },
                {
                    name: RenderBlendedOpaqueDynamicRect,
                    arg_count: 3,
                    fn: |this: &mut Gpu, param: u32, args: Arguments| {
                        this.gfx.draw_quad(this.decode_rect_entries(
                            args,
                            param,
                            RectangleKind::Dynamic,
                            Some(Texturing::Blended),
                        ));
                    },
                },
                {
                    name: RenderUntexturedOpaqueBigSquare,
                    arg_count: 1,
                    fn: |this: &mut Gpu, param: u32, args: Arguments| {
                        this.gfx.draw_quad(this.decode_rect_entries(
                            args,
                            param,
                            RectangleKind::BigSquare,
                            None,
                        ));
                    },
                },
                {
                    name: RenderBlendedTranslucentSmallSquare,
                    arg_count: 2,
                    fn: |this: &mut Gpu, param: u32, args: Arguments| {
                        this.gfx.draw_quad(this.decode_rect_entries(
                            args,
                            param,
                            RectangleKind::SmallSquare,
                            Some(Texturing::Blended),
                        ));
                    },
                },
                {
                    name: MoveRectToVram,
                    arg_count: 2,
                    fn: |this: &mut Gpu, _: u32, args: Arguments| {
                        let decode_size = |code: u16, mask: u16| {
                            (code.wrapping_sub(1) & mask).wrapping_add(1)
                        };

                        let top_left = Vertex::decode(args[0]);
                        let mut size_word = args[1];
                        let size = [
                            decode_size(size_word.pop_bits(16) as u16, 0x3ff),
                            decode_size(size_word as u16, 0x1ff),
                        ];
                        let pixel_count = u32::from(size[0]) * u32::from(size[1]);
                        this.gp0.strat = QueueStrategy::BlitPixels {
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
                    name: SetTexPage,
                    fn: |this: &mut Gpu, param: u32, _: Arguments| {
                        this.tex_page = TexturePage::decode(param as u16);
                    },
                },
                {
                    name: SetTexWindow,
                    fn: |this: &mut Gpu, param: u32, _: Arguments| {
                        this.tex_window = TextureWindow::decode(param);
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

#[derive(Clone, Copy, Debug)]
pub enum Opacity {
    Opaque,
    Translucent,
}

#[derive(Clone, Copy, Debug)]
enum Texturing {
    Raw,
    Blended,
}

impl From<Texturing> for SampleStrategy {
    fn from(texturing: Texturing) -> Self {
        match texturing {
            Texturing::Raw => Self::RawTexture,
            Texturing::Blended => Self::BlendedTexture,
        }
    }
}

impl Gpu {
    fn decode_poly_entries<const N: usize>(
        &self,
        mut args: Arguments,
        param: u32,
        is_shaded: bool,
        texturing: Option<Texturing>,
    ) -> [VertexBufferEntry; N] {
        struct IncompleteVertexBufferEntry {
            vert: Vertex,
            tex_vert_data: u16,
            sample_strat: SampleStrategy,
            color: [u8; 4],
        }

        struct ArgumentStack<'a> {
            args: &'a Arguments,
            idx: usize,
        }

        impl ArgumentStack<'_> {
            fn pop(&mut self) -> u32 {
                let value = self.args[self.idx];
                self.idx += 1;

                value
            }
        }

        let mut stack = ArgumentStack {
            args: &mut args,
            idx: 0,
        };

        let sample_strat = texturing
            .map(SampleStrategy::from)
            .unwrap_or(SampleStrategy::Constant);
        let mut pal_vert = Vertex::default();
        let mut tex_page = TexturePage::default();
        let mut vert_idx = 0;

        let entries = [(); N].map(|_| {
            let color = decode_bgr8_to_rgba8(if is_shaded && (vert_idx > 0) {
                stack.pop()
            } else {
                param
            });
            let vert = Vertex::decode(stack.pop());

            let tex_vert_data = texturing.map(|_| {
                let mut code = stack.pop();
                let tex_vert_data = code.pop_bits(16) as u16;
                match vert_idx {
                    0 => {
                        pal_vert = decode_palette_vertex(code as u16);
                    }
                    1 => {
                        tex_page = TexturePage::decode(code as u16);
                    }
                    _ => {},
                }

                tex_vert_data
            })
            .unwrap_or_default();
            vert_idx += 1;

            IncompleteVertexBufferEntry {
                vert,
                tex_vert_data,
                sample_strat,
                color,
            }
        })
        .map(|incomplete| {
            let tex_vert = self.tex_window.decode_texture_vertex(
                &tex_page,
                incomplete.tex_vert_data,
            );
            tracing::error!("Palette: ({},{})", pal_vert.x, pal_vert.y);
            tracing::error!("Texture: ({},{})", tex_vert.x, tex_vert.y);

            VertexBufferEntry {
                vert: incomplete.vert,
                tex_vert,
                sample_strat: incomplete.sample_strat,
                color: incomplete.color,
            }
        });

        entries
    }
}

enum RectangleKind {
    Dot,
    SmallSquare,
    BigSquare,
    Dynamic,
}

impl Gpu {
    fn decode_rect_entries(
        &self,
        args: Arguments,
        param: u32,
        kind: RectangleKind,
        texturing: Option<Texturing>,
    ) -> [VertexBufferEntry; 4] {
        let size = match kind {
            RectangleKind::Dot => [1, 1],
            RectangleKind::SmallSquare => [8, 8],
            RectangleKind::BigSquare => [16, 16],
            RectangleKind::Dynamic => {
                let idx = if texturing.is_some() { 2 } else { 1 };
                let mut word = args[idx];

                [
                    word.pop_bits(16) as u16,
                    word as u16,
                ]
            }
        };
        let sample_strat = texturing
            .map(SampleStrategy::from)
            .unwrap_or(SampleStrategy::Constant);
        let color = decode_bgr8_to_rgba8(param);

        let mut tex_data = args[1];
        let tex_top_left = self.tex_window.decode_texture_vertex(
            &self.tex_page,
            tex_data.pop_bits(16) as u16,
        );

        let tex_rect = create_rect(tex_top_left, size);
        let rect = create_rect(Vertex::decode(args[0]), size);
        let mut idx = 0;

        [(); 4].map(|_| {
            let entry = VertexBufferEntry {
                vert: rect[idx],
                tex_vert: tex_rect[idx],
                sample_strat,
                color,
            };
            idx += 1;

            entry
        })

    }
}

const fn create_rect(top_left: Vertex, size: [u16; 2]) -> [Vertex; 4] {
    let mut bottom_left = top_left;
    bottom_left.y += size[1];
    let mut top_right = top_left;
    top_right.x += size[0];
    let mut bottom_right = top_right;
    bottom_right.y += size[1];

    [
        top_left,
        bottom_left,
        top_right,
        bottom_right,
    ]
}

fn decode_bgr8_to_rgba8(code: u32) -> [u8; 4] {
    let bytes = code.to_be_bytes();
    let get_component = |i| bytes[i] as u8;

    [
        get_component(3),
        get_component(2),
        get_component(1),
        u8::MAX,
    ]
}

impl TexturePage {
    fn decode(mut code: u16) -> Self {
        let base = Vertex::new(
            64 * code.pop_bits(4) as u16,
            256 * code.pop_bits(1) as u16,
        );
        // TODO: Opacity
        let _ = code.pop_bits(2);
        let color_depth = ColorDepth::decode(code.pop_bits(2) as u8);
        // TODO

        Self { base, color_depth }
    }
}

#[derive(Debug, Default)]
pub struct TexturePage {
    pub base: Vertex,
    pub color_depth: ColorDepth,
}

impl ColorDepth {
    fn decode(code: u8) -> Self {
        match code & 0b11 {
            0 => Self::Bpp4,
            1 => Self::Bpp8,
            2 | 3 => Self::Bpp15,
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub enum ColorDepth {
    #[default]
    Bpp4,
    Bpp8,
    Bpp15,
}

impl TextureWindow {
    fn decode(mut code: u32) -> Self {
        Self {
            mask: TextureWindowMask::decode(code.pop_bits(10) as u16),
            base: Vertex::new(
                code.pop_bits(5) as u16,
                code.pop_bits(5) as u16,
            ),
        }
    }
}

#[derive(Debug, Default)]
pub struct TextureWindow {
    pub mask: TextureWindowMask,
    pub base: Vertex,
}

impl TextureWindow {
    pub fn decode_texture_vertex(&self, page: &TexturePage, code: u16) -> Vertex {
        let make_texture_coord = |coord, window_base, mask: u16, page_base| {
            ((coord & !(mask << 3)) | ((window_base & mask) << 3)) + page_base
        };
        let [y, x] = code.to_be_bytes().map(u16::from);

        Vertex::new(
            make_texture_coord(
                x,
                self.base.x,
                self.mask.x as u16,
                page.base.x,
            ),
            make_texture_coord(
                y,
                self.base.y,
                self.mask.y as u16,
                page.base.y,
            ),
        )
    }
}

impl TextureWindowMask {
    pub fn decode(mut code: u16) -> Self {
        Self {
            x: code.pop_bits(5) as u8,
            y: code.pop_bits(5) as u8,
        }
    }
}

#[derive(Debug, Default)]
pub struct TextureWindowMask {
    x: u8,
    y: u8,
}

fn decode_palette_vertex(mut code: u16) -> Vertex {
    Vertex::new(
        16 * code.pop_bits(6),
        code.pop_bits(9),
    )
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
    RenderUntexturedOpaqueDynamicRect,
    RenderUntexturedTranslucentDot,
    RenderUntexturedTranslucentSmallSquare,
    RenderUntexturedTranslucentBigSquare,
    RenderUntexturedTranslucentDynamicRect,
    RenderBlendedOpaqueDot,
    RenderBlendedOpaqueSmallSquare,
    RenderBlendedOpaqueBigSquare,
    RenderBlendedOpaqueDynamicRect,
    RenderBlendedTranslucentDot,
    RenderBlendedTranslucentSmallSquare,
    RenderBlendedTranslucentBigSquare,
    RenderBlendedTranslucentDynamicRect,
    RenderRawOpaqueDot,
    RenderRawOpaqueSmallSquare,
    RenderRawOpaqueBigSquare,
    RenderRawOpaqueDynamicRect,
    RenderRawTranslucentDot,
    RenderRawTranslucentSmallSquare,
    RenderRawTranslucentBigSquare,
    RenderRawTranslucentDynamicRect,
    CopyRect,
    MoveRectToVram,
    MoveRectToCpuBus,
    SetTexPage,
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
            0x50 | 0x51 => Self::RenderShadedUntexturedOpaqueLine,
            0x52 | 0x53 => Self::RenderShadedUntexturedTranslucentLine,
            0x54        => Self::RenderShadedBlendedOpaqueLine,
            0x55        => Self::RenderShadedRawOpaqueLine,
            0x56        => Self::RenderShadedBlendedTranslucentLine,
            0x57        => Self::RenderShadedRawTranslucentLine,
            0x58 | 0x59 => Self::RenderShadedUntexturedOpaqueLinestrip,
            0x5a | 0x5b => Self::RenderShadedUntexturedTranslucentLinestrip,
            0x5c        => Self::RenderShadedBlendedOpaqueLinestrip,
            0x5d        => Self::RenderShadedRawOpaqueLinestrip,
            0x5e        => Self::RenderShadedBlendedTranslucentLinestrip,
            0x5f        => Self::RenderShadedRawTranslucentLinestrip,
            0x60 | 0x61 => Self::RenderUntexturedOpaqueDynamicRect,
            0x62 | 0x63 => Self::RenderUntexturedTranslucentDynamicRect,
            0x64        => Self::RenderBlendedOpaqueDynamicRect,
            0x65        => Self::RenderRawOpaqueDynamicRect,
            0x66        => Self::RenderBlendedTranslucentDynamicRect,
            0x67        => Self::RenderRawTranslucentDynamicRect,
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
            0xe1 => Self::SetTexPage,
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
