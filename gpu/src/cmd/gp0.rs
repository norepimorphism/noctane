// SPDX-License-Identifier: MPL-2.0

use noctane_util::BitStack as _;

use super::MachineCommand;

#[derive(Clone, Debug)]
pub struct State {
    pub arg_count: usize,
}

#[derive(Clone, Debug)]
pub enum Command {
    Nop,
    ClearTexCache,
    QuickfillRect,
    X03,
    RequestInt,
    RenderPoly {
        coloring: Coloring,
        kind: PolygonKind,
        opacity: Opacity,
        is_shaded: bool,
    },
    CopyRect,
    MoveRectToVram,
    MoveRectToCpuBus,
    SetTexpage,
    SetTexWindow,
}

#[derive(Clone, Debug)]
pub enum Coloring {
    Monochrome(Color),
    Textured(Texturing),
}

#[derive(Clone, Copy, Debug)]
pub enum Texturing {
    Blended(Color),
    Raw,
}

#[derive(Clone, Copy, Debug)]
pub struct Color {
    pub red: u8,
    pub green: u8,
    pub blue: u8,
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
pub enum PolygonKind {
    Triangle,
    Quad,
}

#[derive(Clone, Copy, Debug)]
pub enum Opacity {
    Opaque,
    Translucent,
}

impl Command {
    pub fn decode(mach: MachineCommand) -> Self {
        // Crossing my fingers that this gets optimized into a LUT...
        match mach.opcode {
            0x00 => Self::Nop,
            0x01 => Self::ClearTexCache,
            0x02 => Self::QuickfillRect,
            0x03 => Self::X03,
            0x04..=0x1e => Self::Nop,
            0x1f => Self::RequestInt,
            0x20 | 0x21 => Self::RenderPoly {
                coloring: Coloring::Monochrome(Color::decode(mach.param)),
                kind: PolygonKind::Triangle,
                opacity: Opacity::Opaque,
                is_shaded: false,
            },
            0x22 | 0x23 => Self::RenderPoly {
                coloring: Coloring::Monochrome(Color::decode(mach.param)),
                kind: PolygonKind::Triangle,
                opacity: Opacity::Translucent,
                is_shaded: false,
            },
            0x24 => Self::RenderPoly {
                coloring: Coloring::Textured(Texturing::Blended(Color::decode(mach.param))),
                kind: PolygonKind::Triangle,
                opacity: Opacity::Opaque,
                is_shaded: false,
            },
            0x25 => Self::RenderPoly {
                coloring: Coloring::Textured(Texturing::Raw),
                kind: PolygonKind::Triangle,
                opacity: Opacity::Opaque,
                is_shaded: false,
            },
            0x26 => Self::RenderPoly {
                coloring: Coloring::Textured(Texturing::Blended(Color::decode(mach.param))),
                kind: PolygonKind::Triangle,
                opacity: Opacity::Translucent,
                is_shaded: false,
            },
            0x27 => Self::RenderPoly {
                coloring: Coloring::Textured(Texturing::Raw),
                kind: PolygonKind::Triangle,
                opacity: Opacity::Translucent,
                is_shaded: false,
            },
            0x28 | 0x29 => Self::RenderPoly {
                coloring: Coloring::Monochrome(Color::decode(mach.param)),
                kind: PolygonKind::Quad,
                opacity: Opacity::Opaque,
                is_shaded: false,
            },
            0x2a | 0x2b => Self::RenderPoly {
                coloring: Coloring::Monochrome(Color::decode(mach.param)),
                kind: PolygonKind::Quad,
                opacity: Opacity::Translucent,
                is_shaded: false,
            },
            0x2c => Self::RenderPoly {
                coloring: Coloring::Textured(Texturing::Blended(Color::decode(mach.param))),
                kind: PolygonKind::Quad,
                opacity: Opacity::Opaque,
                is_shaded: false,
            },
            0x2d => Self::RenderPoly {
                coloring: Coloring::Textured(Texturing::Raw),
                kind: PolygonKind::Quad,
                opacity: Opacity::Opaque,
                is_shaded: false,
            },
            0x2e => Self::RenderPoly {
                coloring: Coloring::Textured(Texturing::Blended(Color::decode(mach.param))),
                kind: PolygonKind::Quad,
                opacity: Opacity::Translucent,
                is_shaded: false,
            },
            0x2f => Self::RenderPoly {
                coloring: Coloring::Textured(Texturing::Raw),
                kind: PolygonKind::Quad,
                opacity: Opacity::Translucent,
                is_shaded: false,
            },
            0x30 | 0x31 => Self::RenderPoly {
                coloring: Coloring::Monochrome(Color::decode(mach.param)),
                kind: PolygonKind::Triangle,
                opacity: Opacity::Opaque,
                is_shaded: true,
            },
            0x32 | 0x33 => Self::RenderPoly {
                coloring: Coloring::Monochrome(Color::decode(mach.param)),
                kind: PolygonKind::Triangle,
                opacity: Opacity::Translucent,
                is_shaded: true,
            },
            0x34 => Self::RenderPoly {
                coloring: Coloring::Textured(Texturing::Blended(Color::decode(mach.param))),
                kind: PolygonKind::Triangle,
                opacity: Opacity::Opaque,
                is_shaded: true,
            },
            0x35 => Self::RenderPoly {
                coloring: Coloring::Textured(Texturing::Raw),
                kind: PolygonKind::Triangle,
                opacity: Opacity::Opaque,
                is_shaded: true,
            },
            0x36 => Self::RenderPoly {
                coloring: Coloring::Textured(Texturing::Blended(Color::decode(mach.param))),
                kind: PolygonKind::Triangle,
                opacity: Opacity::Translucent,
                is_shaded: true,
            },
            0x37 => Self::RenderPoly {
                coloring: Coloring::Textured(Texturing::Raw),
                kind: PolygonKind::Triangle,
                opacity: Opacity::Translucent,
                is_shaded: true,
            },
            0x38 | 0x39 => Self::RenderPoly {
                coloring: Coloring::Monochrome(Color::decode(mach.param)),
                kind: PolygonKind::Quad,
                opacity: Opacity::Opaque,
                is_shaded: true,
            },
            0x3a | 0x3b => Self::RenderPoly {
                coloring: Coloring::Monochrome(Color::decode(mach.param)),
                kind: PolygonKind::Quad,
                opacity: Opacity::Translucent,
                is_shaded: true,
            },
            0x3c => Self::RenderPoly {
                coloring: Coloring::Textured(Texturing::Blended(Color::decode(mach.param))),
                kind: PolygonKind::Quad,
                opacity: Opacity::Opaque,
                is_shaded: true,
            },
            0x3d => Self::RenderPoly {
                coloring: Coloring::Textured(Texturing::Raw),
                kind: PolygonKind::Quad,
                opacity: Opacity::Opaque,
                is_shaded: true,
            },
            0x3e => Self::RenderPoly {
                coloring: Coloring::Textured(Texturing::Blended(Color::decode(mach.param))),
                kind: PolygonKind::Quad,
                opacity: Opacity::Translucent,
                is_shaded: true,
            },
            0x3f => Self::RenderPoly {
                coloring: Coloring::Textured(Texturing::Raw),
                kind: PolygonKind::Quad,
                opacity: Opacity::Translucent,
                is_shaded: true,
            },
            0x80..=0x9f => Self::CopyRect,
            0xa0..=0xbf => Self::MoveRectToVram,
            0xc0..=0xdf => Self::MoveRectToCpuBus,
            0xe0 => Self::Nop,
            0xe1 => Self::SetTexpage,
            0xe2 => Self::SetTexWindow,
            0xe7..=0xef => Self::Nop,
            _ => todo!("Unknown opcode: {:#04x}", mach.opcode),
        }
    }
}
