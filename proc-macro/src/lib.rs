// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

#![feature(let_else, proc_macro_diagnostic)]

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{
    parse_macro_input,
};

#[proc_macro]
pub fn gen_cpu_bus_io(input: TokenStream) -> TokenStream {
    let entries = parse_macro_input!(input as cpu::bus::io::Entries).0;

    let regs = entries.into_iter().map(|entry| {
        let cpu::bus::io::AccessFns {
            _8: read_8,
            _16: read_16,
            _32: read_32,
        } = entry.read.gen_access_fns();
        let cpu::bus::io::AccessFns {
            _8: write_8,
            _16: write_16,
            _32: write_32,
        } = entry.write.gen_access_fns();

        quote! {
            Register {
                read_8: #read_8,
                read_16: #read_16,
                read_32: #read_32,
                write_8: #write_8,
                write_16: #write_16,
                write_32: #write_32,
            }
        }
    })
    .collect::<TokenStream2>();

    quote! {
        static REGISTERS: &[Register] = &[#regs];

        static LUT: &[LutEntry] = &[

        ];
    }
    .into()
}

mod cpu {
    pub mod bus {
        pub mod io {
            use proc_macro2::TokenStream as TokenStream2;
            use quote::quote;
            use syn::{
                braced,
                parse::{Parse, ParseStream},
                punctuated::Punctuated,
                Expr,
                FieldValue,
                Member,
                Token,
            };

            impl Parse for Entries {
                fn parse(input: ParseStream) -> syn::Result<Self> {
                    input
                        .parse_terminated(Entry::parse)
                        .map(Self)
                }
            }

            pub struct Entries(pub Punctuated<Entry, Token![,]>);

            impl Parse for Entry {
                fn parse(input: ParseStream) -> syn::Result<Self> {
                    let content;
                    braced!(content in input);

                    let mut fields = content
                        .parse_terminated::<_, Token![,]>(FieldValue::parse)?
                        .into_iter();

                    let name_field = fields.nth(0).ok_or_else(|| {
                        content.error("expected 'name' field")
                    })?;
                    let read_field = fields.nth(0).ok_or_else(|| {
                        content.error("expected 'read_*' field")
                    })?;
                    let write_field = fields.nth(0).ok_or_else(|| {
                        content.error("expected 'write_*' field")
                    })?;
                    if fields.next().is_some() {
                        content.span().unwrap().warning("found extraneous fields");
                    }

                    let name = name_field.expr;

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

                    Ok(Self { name, read, write })
                }
            }

            pub struct Entry {
                pub name: Expr,
                pub read: ReadFn,
                pub write: WriteFn,
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

            pub enum AccessKind {
                _8,
                _16,
                _32,
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
                                    let [a, b] = (this.read_16)(this, io, 0);
                                    let [c, d] = (this.read_16)(this, io, 1);

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
