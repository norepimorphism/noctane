// SPDX-License-Identifier: MPL-2.0

use super::MachineCommand;

#[derive(Clone, Debug)]
pub enum Command {
    Nop,
    ClearTexCache,
    QuickfillRect,
    _3,
    RequestInt,
    RenderPoly {
        coloring: Coloring,
        kind: PolygonKind,
        opacity: Opacity,
    },
}

impl Command {
    pub fn decode(mach: MachineCommand) -> Self {
        // Crossing my fingers that this gets optimized into a LUT...
        match mach.opcode {
            0x00 => Self::Nop,
            0x01 => Self::ClearTexCache,
            0x02 => Self::QuickfillRect,
            0x03 => Self::_3,
            0x04..=0x1e => Self::Nop,
            0x1f => Self::RequestInt,
            0x20 | 0x21 => Self::RenderPoly {
                coloring: Coloring::Monochrome,
                kind: PolygonKind::Triangle,
                opacity: Opacity::Opaque,
            },
            0x22 | 0x23 => Self::RenderPoly {
                coloring: Coloring::Monochrome,
                kind: PolygonKind::Triangle,
                opacity: Opacity::SemiTrans,
            },
            0x24 => Self::RenderPoly {
                coloring: Coloring::Textured(Texturing::Blended),
                kind: PolygonKind::Triangle,
                opacity: Opacity::Opaque,
            },
            0x25 => Self::RenderPoly {
                coloring: Coloring::Textured(Texturing::Raw),
                kind: PolygonKind::Triangle,
                opacity: Opacity::Opaque,
            },
            0x26 => Self::RenderPoly {
                coloring: Coloring::Textured(Texturing::Blended),
                kind: PolygonKind::Triangle,
                opacity: Opacity::SemiTrans,
            },
            0x27 => Self::RenderPoly {
                coloring: Coloring::Textured(Texturing::Raw),
                kind: PolygonKind::Triangle,
                opacity: Opacity::SemiTrans,
            },
            0x28 | 0x29 => Self::RenderPoly {
                coloring: Coloring::Monochrome,
                kind: PolygonKind::Quad,
                opacity: Opacity::Opaque,
            },
            0x2a | 0x2b => Self::RenderPoly {
                coloring: Coloring::Monochrome,
                kind: PolygonKind::Quad,
                opacity: Opacity::SemiTrans,
            },
            0x2c => Self::RenderPoly {
                coloring: Coloring::Textured(Texturing::Blended),
                kind: PolygonKind::Quad,
                opacity: Opacity::Opaque,
            },
            0x2d => Self::RenderPoly {
                coloring: Coloring::Textured(Texturing::Raw),
                kind: PolygonKind::Quad,
                opacity: Opacity::Opaque,
            },
            0x2e => Self::RenderPoly {
                coloring: Coloring::Textured(Texturing::Blended),
                kind: PolygonKind::Quad,
                opacity: Opacity::SemiTrans,
            },
            0x2f => Self::RenderPoly {
                coloring: Coloring::Textured(Texturing::Raw),
                kind: PolygonKind::Quad,
                opacity: Opacity::SemiTrans,
            },
            _ => todo!(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Coloring {
    Monochrome,
    Textured(Texturing),
}

#[derive(Clone, Copy, Debug)]
pub enum Texturing {
    Blended,
    Raw,
}

#[derive(Clone, Copy, Debug)]
pub enum PolygonKind {
    Triangle,
    Quad,
}

#[derive(Clone, Copy, Debug)]
pub enum Opacity {
    Opaque,
    SemiTrans,
}
