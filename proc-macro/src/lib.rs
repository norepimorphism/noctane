// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

#![feature(entry_insert, let_else, proc_macro_diagnostic)]

use std::collections::HashMap;

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{
    parse_macro_input,
};

/// Generates the `noctane_cpu::bus::io` lookup tables (LUTs).
#[proc_macro]
pub fn gen_cpu_bus_io(input: TokenStream) -> TokenStream {
    let lut_strukts = parse_macro_input!(input as cpu::bus::io::LutStructSeq);

    let mut lut_mods = TokenStream2::new();
    let mut reg_entries = TokenStream2::new();
    // The *raison d'etre* of this table is to memorize previously-seen register structs such that
    // they can be referenced by [`cpu::bus::io::RegStruct::Mirror`]s.
    let mut reg_table = HashMap::new();

    for lut_strukt in lut_strukts.0 {
        let lut_name = lut_strukt.name;
        let lut_base_addr = lut_strukt.base_addr;
        let mut lut_entries = TokenStream2::new();

        for reg_strukt in lut_strukt.regs {
            match reg_strukt {
                cpu::bus::io::RegStruct::Normal { name, read, write } => {
                    // This is performant as [`HashMap::len`] reads from a single field and doesn't
                    // iterate the table; see
                    // <https://docs.rs/hashbrown/0.12.1/src/hashbrown/raw/mod.rs.html#388>.
                    let reg_index = reg_table.len();

                    // We assume that `read.kind.len()` and `write.kind.len()` are equivalent.
                    // TODO: Don't make that assumption; assert that it is true.
                    let reg_len = read.kind.len();

                    for byte_idx in 0..reg_len {
                        lut_entries.extend(quote! {
                            // Remember: we're inside the LUT module, so we must use `super`.
                            super::LutEntry { reg_idx: #reg_index, byte_idx: #byte_idx },
                        });
                    }

                    let cpu::bus::io::AccessFns {
                        _8: read_8,
                        _16: read_16,
                        _32: read_32,
                    } = read.gen_access_fns();
                    let cpu::bus::io::AccessFns {
                        _8: write_8,
                        _16: write_16,
                        _32: write_32,
                    } = write.gen_access_fns();

                    reg_entries.extend(quote! {
                        Register {
                            read_8: #read_8,
                            read_16: #read_16,
                            read_32: #read_32,
                            write_8: #write_8,
                            write_16: #write_16,
                            write_32: #write_32,
                        },
                    });

                    reg_table.entry(name).insert_entry((reg_index, reg_len));
                }
                cpu::bus::io::RegStruct::Mirror { name } => {
                    // We can afford to clone here as these are merely two machine words.
                    let (index, len) = reg_table.get(&name).cloned().unwrap();

                    for byte_idx in 0..len {
                        lut_entries.extend(quote! {
                            super::LutEntry { reg_idx: #index, byte_idx: #byte_idx },
                        });
                    }
                }
            };

        }

        lut_mods.extend(quote! {
            mod #lut_name {
                /// The base address, relative to the start of the I/O region, of the group of I/O
                /// registers represented by [`LUT`].
                pub(super) const BASE_ADDR: usize = #lut_base_addr;

                /// A lookup table (LUT) of [`LutEntry`]s, indexed by an address relative to the
                /// start of the I/O region.
                pub(super) static LUT: &[super::LutEntry] = &[#lut_entries];
            }
        });
    }

    quote! {
        /// All I/O registers.
        static REGISTERS: &[Register] = &[#reg_entries];

        #lut_mods
    }
    .into()
}

mod cpu {
    pub mod bus {
        pub mod io {
            use proc_macro2::TokenStream as TokenStream2;
            use quote::quote;
            use syn::{
                parse::{Parse, ParseStream},
                punctuated::Punctuated,
                Expr,
                ExprStruct,
                Member,
                Token,
            };

            impl Parse for LutStructSeq {
                fn parse(input: ParseStream) -> syn::Result<Self> {
                    input.parse_terminated(LutStruct::parse).map(Self)
                }
            }

            /// A comma-separated sequence of [`LutStruct`]s.
            pub struct LutStructSeq(pub Punctuated<LutStruct, Token![,]>);

            impl Parse for LutStruct {
                fn parse(input: ParseStream) -> syn::Result<Self> {
                    let strukt = input.parse::<ExprStruct>()?;

                    let Some(ident) = strukt.path.get_ident() else {
                        return Err(input.error("struct path should be a single identifier"));
                    };
                    if ident != "Lut" {
                        return Err(input.error("expected 'Lut' struct"));
                    }

                    let mut fields = strukt.fields.into_iter();

                    let name_field = fields.nth(0).ok_or_else(|| {
                        input.error("expected 'name' field")
                    })?;
                    let base_field = fields.nth(0).ok_or_else(|| {
                        input.error("expected 'base_addr' field")
                    })?;
                    let regs_field = fields.nth(0).ok_or_else(|| {
                        input.error("expected 'regs' field")
                    })?;
                    if fields.next().is_some() {
                        input.span().unwrap().warning("found extraneous fields");
                    }

                    let name = name_field.expr;
                    let base_addr = base_field.expr;

                    let Expr::Array(regs_array) = regs_field.expr else {
                        return Err(input.error("'regs' should be an array"));
                    };
                    let regs = regs_array.elems
                        .into_iter()
                        .map(|elem| {
                            let Expr::Struct(strukt) = elem else {
                                return Err(input.error("expected struct"));
                            };

                            let Some(ident) = strukt.path.get_ident() else {
                                return Err(
                                    input.error("struct path should be a single identifier")
                                );
                            };

                            let mut fields = strukt.fields.into_iter();

                            let name_field = fields.nth(0).ok_or_else(|| {
                                input.error("expected 'name' field")
                            })?;
                            let name = name_field.expr;

                            match ident.to_string().as_str() {
                                "Register" => {
                                    let read_field = fields.nth(0).ok_or_else(|| {
                                        input.error("expected 'read_*' field")
                                    })?;
                                    let write_field = fields.nth(0).ok_or_else(|| {
                                        input.error("expected 'write_*' field")
                                    })?;
                                    if fields.next().is_some() {
                                        input.span().unwrap().warning("found extraneous fields");
                                    }

                                    let read_kind = AccessKind::try_from_read(read_field.member)
                                        .ok_or_else(|| {
                                            input.error("failed to parse 'read' identifier")
                                        })?;
                                    let read_expr = read_field.expr;
                                    let read = ReadFn {
                                        kind: read_kind,
                                        expr: quote!(#read_expr),
                                    };

                                    let write_kind = AccessKind::try_from_write(write_field.member)
                                        .ok_or_else(|| {
                                            input.error("failed to parse 'write' identifier")
                                        })?;
                                    let write_expr = write_field.expr;
                                    let write = WriteFn {
                                        kind: write_kind,
                                        expr: quote!(#write_expr),
                                    };

                                    Ok(RegStruct::Normal { name, read, write })
                                }
                                "Mirror" => {
                                    Ok(RegStruct::Mirror { name })
                                }
                                _ => Err(input.error("expected 'Register' or 'Mirror' struct")),
                            }
                        })
                        .collect::<syn::Result<Vec<RegStruct>>>()?;

                    Ok(Self { name, base_addr, regs })
                }
            }

            pub struct LutStruct {
                pub name: Expr,
                pub base_addr: Expr,
                pub regs: Vec<RegStruct>,
            }

            pub enum RegStruct {
                Normal {
                    name: Expr,
                    read: ReadFn,
                    write: WriteFn,
                },
                Mirror {
                    name: Expr,
                },
            }

            pub struct AccessFns {
                pub _8: TokenStream2,
                pub _16: TokenStream2,
                pub _32: TokenStream2,
            }

            macro_rules! def_try_access_kind_from_member {
                ($fn_name:ident $name:ident $name_8:ident $name_16:ident $name_32:ident) => {
                    fn $fn_name(member: Member) -> Option<Self> {
                        let Member::Named(ident) = member else {
                            return None;
                        };

                        match ident.to_string().as_str() {
                            stringify!($name_8) => Some(Self::_8),
                            stringify!($name_16) => Some(Self::_16),
                            stringify!($name_32) => Some(Self::_32),
                            _ => {
                                ident
                                    .span()
                                    .unwrap()
                                    .error(format!(
                                        "'{0}' field doesn't match any of: '{0}_8', '{0}_16', '{0}_32'",
                                        stringify!($name),
                                    ))
                                    .emit();

                                None
                            }
                        }
                    }
                };
            }

            impl AccessKind {
                def_try_access_kind_from_member!(try_from_read read read_8 read_16 read_32);
                def_try_access_kind_from_member!(try_from_write write write_8 write_16 write_32);
            }

            #[derive(Clone, Copy)]
            pub enum AccessKind {
                _8,
                _16,
                _32,
            }

            impl AccessKind {
                pub fn len(self) -> usize {
                    match self {
                        Self::_8 => 1,
                        Self::_16 => 2,
                        Self::_32 => 4,
                    }
                }
            }

            pub struct ReadFn {
                pub kind: AccessKind,
                pub expr: TokenStream2,
            }

            impl ReadFn {
                pub fn gen_access_fns(self) -> AccessFns {
                    match self.kind {
                        AccessKind::_8 => AccessFns {
                            _8: self.expr,
                            _16: quote! {
                                |this, io, offset| {
                                    u16::from_be_bytes([
                                        (this.read_8)(this, io, offset),
                                        (this.read_8)(this, io, offset.wrapping_add(1)),
                                    ])
                                }
                            },
                            _32: quote! {
                                |this, io| {
                                    u32::from_be_bytes([
                                        (this.read_8)(this, io, 0),
                                        (this.read_8)(this, io, 1),
                                        (this.read_8)(this, io, 2),
                                        (this.read_8)(this, io, 3),
                                    ])
                                }
                            },
                        },
                        AccessKind::_16 => AccessFns {
                            _8: quote! {
                                |this, io, offset| {
                                    (this.read_16)(this, io, (offset >> 1) & 0b1)
                                        .to_be_bytes()
                                        [offset & 0b1]
                                }
                            },
                            _16: self.expr,
                            _32: quote! {
                                |this, io| {
                                    let [a, b] = (this.read_16)(this, io, 0).to_be_bytes();
                                    let [c, d] = (this.read_16)(this, io, 1).to_be_bytes();

                                    u32::from_be_bytes([a, b, c, d])
                                }
                            },
                        },
                        AccessKind::_32 => AccessFns {
                            _8: quote! {
                                |this, io, offset| (this.read_32)(this, io).to_be_bytes()[offset]
                            },
                            _16: quote! {
                                |this, io, offset| {
                                    u16::from_be_bytes({
                                        (this.read_32)(this, io)
                                            .to_be_bytes()
                                            .as_chunks::<2>()
                                            .0
                                            [offset]
                                    })

                                }
                            },
                            _32: self.expr,
                        },
                    }
                }
            }

            pub struct WriteFn {
                pub kind: AccessKind,
                pub expr: TokenStream2,
            }

            impl WriteFn {
                pub fn gen_access_fns(self) -> AccessFns {
                    match self.kind {
                        AccessKind::_8 => AccessFns {
                            _8: self.expr,
                            _16: quote! {
                                |this, io, offset, value| {
                                    let [hi, lo] = value.to_be_bytes();
                                    (this.write_8)(
                                        this,
                                        io,
                                        offset,
                                        hi,
                                    );
                                    (this.write_8)(
                                        this,
                                        io,
                                        offset.wrapping_add(1),
                                        lo,
                                    );
                                }
                            },
                            _32: quote! {
                                |this, io, value| {
                                    let [a, b, c, d] = value.to_be_bytes();
                                    (this.write_8)(this, io, 0, a);
                                    (this.write_8)(this, io, 1, b);
                                    (this.write_8)(this, io, 2, c);
                                    (this.write_8)(this, io, 3, d);
                                }
                            },
                        },
                        AccessKind::_16 => AccessFns {
                            _8: quote! {
                                |this, io, offset, value| {
                                    todo!()
                                }
                            },
                            _16: self.expr,
                            _32: quote! {
                                |this, io, value| {
                                    todo!()
                                }
                            },
                        },
                        AccessKind::_32 => AccessFns {
                            _8: quote! {
                                |this, io, offset, value| {
                                    todo!()
                                }
                            },
                            _16: quote! {
                                |this, io, offset, value| {
                                    todo!()
                                }
                            },
                            _32: self.expr,
                        },
                    }
                }
            }
        }
    }
}
