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
    RenderUnshadedUntexturedPoly {
        color: Color,
        kind: PolygonKind,
        opacity: Opacity,
    },
    RenderUnshadedTexturedPoly {
        texturing: Texturing,
        kind: PolygonKind,
        opacity: Opacity,
    },
    RenderShadedUntexturedPoly {
        color: Color,
        kind: PolygonKind,
        opacity: Opacity,
    },
    RenderShadedTexturedPoly {
        texturing: Texturing,
        kind: PolygonKind,
        opacity: Opacity,
    },
    RenderUnshadedUntexturedLine {
        opacity: Opacity,
    },
    RenderUnshadedTexturedLine {
        texturing: Texturing,
        opacity: Opacity,
    },
    RenderUnshadedUntexturedPolyline {
        opacity: Opacity,
    },
    RenderUnshadedTexturedPolyline {
        texturing: Texturing,
        opacity: Opacity,
    },
    CopyRect,
    MoveRectToVram,
    MoveRectToCpuBus,
    SetTexpage,
    SetTexWindow,
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
            0x20 | 0x21 => Self::RenderUnshadedUntexturedPoly {
                color: Color::decode(mach.param),
                kind: PolygonKind::Triangle,
                opacity: Opacity::Opaque,
            },
            0x22 | 0x23 => Self::RenderUnshadedUntexturedPoly {
                color: Color::decode(mach.param),
                kind: PolygonKind::Triangle,
                opacity: Opacity::Translucent,
            },
            0x24 => Self::RenderUnshadedTexturedPoly {
                texturing: Texturing::Blended(Color::decode(mach.param)),
                kind: PolygonKind::Triangle,
                opacity: Opacity::Opaque,
            },
            0x25 => Self::RenderUnshadedTexturedPoly {
                texturing: Texturing::Raw,
                kind: PolygonKind::Triangle,
                opacity: Opacity::Opaque,
            },
            0x26 => Self::RenderUnshadedTexturedPoly {
                texturing: Texturing::Blended(Color::decode(mach.param)),
                kind: PolygonKind::Triangle,
                opacity: Opacity::Translucent,
            },
            0x27 => Self::RenderUnshadedTexturedPoly {
                texturing: Texturing::Raw,
                kind: PolygonKind::Triangle,
                opacity: Opacity::Translucent,
            },
            0x28 | 0x29 => Self::RenderUnshadedUntexturedPoly {
                color: Color::decode(mach.param),
                kind: PolygonKind::Quad,
                opacity: Opacity::Opaque,
            },
            0x2a | 0x2b => Self::RenderUnshadedUntexturedPoly {
                color: Color::decode(mach.param),
                kind: PolygonKind::Quad,
                opacity: Opacity::Translucent,
            },
            0x2c => Self::RenderUnshadedTexturedPoly {
                texturing: Texturing::Blended(Color::decode(mach.param)),
                kind: PolygonKind::Quad,
                opacity: Opacity::Opaque,
            },
            0x2d => Self::RenderUnshadedTexturedPoly {
                texturing: Texturing::Raw,
                kind: PolygonKind::Quad,
                opacity: Opacity::Opaque,
            },
            0x2e => Self::RenderUnshadedTexturedPoly {
                texturing: Texturing::Blended(Color::decode(mach.param)),
                kind: PolygonKind::Quad,
                opacity: Opacity::Translucent,
            },
            0x2f => Self::RenderUnshadedTexturedPoly {
                texturing: Texturing::Raw,
                kind: PolygonKind::Quad,
                opacity: Opacity::Translucent,
            },
            0x30 | 0x31 => Self::RenderShadedUntexturedPoly {
                color: Color::decode(mach.param),
                kind: PolygonKind::Triangle,
                opacity: Opacity::Opaque,
            },
            0x32 | 0x33 => Self::RenderShadedUntexturedPoly {
                color: Color::decode(mach.param),
                kind: PolygonKind::Triangle,
                opacity: Opacity::Translucent,
            },
            0x34 => Self::RenderShadedTexturedPoly {
                texturing: Texturing::Blended(Color::decode(mach.param)),
                kind: PolygonKind::Triangle,
                opacity: Opacity::Opaque,
            },
            0x35 => Self::RenderShadedTexturedPoly {
                texturing: Texturing::Raw,
                kind: PolygonKind::Triangle,
                opacity: Opacity::Opaque,
            },
            0x36 => Self::RenderShadedTexturedPoly {
                texturing: Texturing::Blended(Color::decode(mach.param)),
                kind: PolygonKind::Triangle,
                opacity: Opacity::Translucent,
            },
            0x37 => Self::RenderShadedTexturedPoly {
                texturing: Texturing::Raw,
                kind: PolygonKind::Triangle,
                opacity: Opacity::Translucent,
            },
            0x38 | 0x39 => Self::RenderShadedUntexturedPoly {
                color: Color::decode(mach.param),
                kind: PolygonKind::Quad,
                opacity: Opacity::Opaque,
            },
            0x3a | 0x3b => Self::RenderShadedUntexturedPoly {
                color: Color::decode(mach.param),
                kind: PolygonKind::Quad,
                opacity: Opacity::Translucent,
            },
            0x3c => Self::RenderShadedTexturedPoly {
                texturing: Texturing::Blended(Color::decode(mach.param)),
                kind: PolygonKind::Quad,
                opacity: Opacity::Opaque,
            },
            0x3d => Self::RenderShadedTexturedPoly {
                texturing: Texturing::Raw,
                kind: PolygonKind::Quad,
                opacity: Opacity::Opaque,
            },
            0x3e => Self::RenderShadedTexturedPoly {
                texturing: Texturing::Blended(Color::decode(mach.param)),
                kind: PolygonKind::Quad,
                opacity: Opacity::Translucent,
            },
            0x3f => Self::RenderShadedTexturedPoly {
                texturing: Texturing::Raw,
                kind: PolygonKind::Quad,
                opacity: Opacity::Translucent,
            },
            0x40 | 0x41 => Self::RenderUnshadedUntexturedLine {
                opacity: Opacity::Opaque,
            },
            0x42 | 0x43 => Self::RenderUnshadedUntexturedLine {
                opacity: Opacity::Translucent,
            },
            0x44 => Self::RenderUnshadedTexturedLine {
                texturing: Texturing::Blended(Color::decode(mach.param)),
                opacity: Opacity::Opaque,
            },
            0x45 => Self::RenderUnshadedTexturedLine {
                texturing: Texturing::Raw,
                opacity: Opacity::Opaque,
            },
            0x46 => Self::RenderUnshadedTexturedLine {
                texturing: Texturing::Blended(Color::decode(mach.param)),
                opacity: Opacity::Translucent,
            },
            0x47 => Self::RenderUnshadedTexturedLine {
                texturing: Texturing::Raw,
                opacity: Opacity::Translucent,
            },
            0x48 | 0x49 => Self::RenderUnshadedUntexturedPolyline {
                opacity: Opacity::Opaque,
            },
            0x4a | 0x4b => Self::RenderUnshadedUntexturedPolyline {
                opacity: Opacity::Translucent,
            },
            0x4c => Self::RenderUnshadedTexturedPolyline {
                texturing: Texturing::Blended(Color::decode(mach.param)),
                opacity: Opacity::Opaque,
            },
            0x4d => Self::RenderUnshadedTexturedPolyline {
                texturing: Texturing::Raw,
                opacity: Opacity::Opaque,
            },
            0x4e => Self::RenderUnshadedTexturedPolyline {
                texturing: Texturing::Blended(Color::decode(mach.param)),
                opacity: Opacity::Translucent,
            },
            0x4f => Self::RenderUnshadedTexturedPolyline {
                texturing: Texturing::Raw,
                opacity: Opacity::Translucent,
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
