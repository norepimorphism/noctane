use std::fmt;

use derivative::Derivative;
use serde::{de, Deserialize};
use serde_repr::Deserialize_repr;

#[derive(Debug)]
pub enum Error {
    Message(String),
    ExpectedU8,
    ExpectedU16,
    ExpectedU32,
    ExpectedU64,
}

impl de::Error for Error {
    fn custom<T>(msg: T) -> Self
    where
        T:fmt::Display,
    {
        Self::Message(msg.to_string())
    }
}

impl de::StdError for Error {}

impl fmt::Display for Error {
    fn fmt(&self, _: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

impl<'de> Deserializer<'de> {
    pub fn from_bytes(input: &'de [u8]) -> Self {
        Self { input }
    }
}

pub struct Deserializer<'de> {
    input: &'de [u8],
}

macro_rules! def_deserialize_unimpl {
    ($lt:lifetime $($fn:ident($($arg:ty),* $(,)?))*) => {
        $(
            fn $fn<V>(self, $(_: $arg,)* _: V) -> Result<V::Value, Error>
            where
                V: de::Visitor<$lt>,
            {
                unimplemented!(stringify!($fn))
            }
        )*
    };
}

macro_rules! def_deserialize_native_endian {
    (
        fn: $fn:ident<$lt:lifetime> -> Result<$ty:ty, $exp:ident $(,)?>,
        visit: $visit:ident $(,)?
    ) => {
        fn $fn<V>(self, visitor: V) -> Result<V::Value, Error>
        where
            V: de::Visitor<$lt>,
        {
            const TYPE_SIZE: usize = std::mem::size_of::<$ty>();

            let it = self.input
                .get(0..TYPE_SIZE)
                .ok_or(Error::$exp)
                .map(|it| <[u8; TYPE_SIZE]>::try_from(it).expect("type size mismatch"))
                .map(|it| <$ty>::from_ne_bytes(it))?;
            self.input = &self.input[TYPE_SIZE..];

            visitor.$visit(it)
        }
    };
}

impl<'de, 'a> de::Deserializer<'de> for &'a mut Deserializer<'de> {
    type Error = Error;

    def_deserialize_unimpl! {
        'de
        deserialize_any()
        deserialize_bool()
        deserialize_i8()
        deserialize_i16()
        deserialize_i32()
        deserialize_i64()
        deserialize_f32()
        deserialize_f64()
        deserialize_char()
        deserialize_str()
        deserialize_string()
        deserialize_byte_buf()
        deserialize_option()
        deserialize_unit()
        deserialize_tuple_struct(&'static str, usize)
        deserialize_enum(&'static str, &'static [&'static str])
        deserialize_unit_struct(&'static str)
        deserialize_newtype_struct(&'static str)
        deserialize_seq()
        deserialize_map()
        deserialize_identifier()
        deserialize_ignored_any()
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        let it = self.input.get(0).ok_or(Error::ExpectedU8)?;
        self.input = &self.input[1..];

        visitor.visit_u8(*it)
    }

    // While the spec. indicates that either little-, big- or middle-endian representations
    // for multi-byte integers may be used, I have only seen them in middle-endian. It is
    // probably erroneous to assume that of all integer fields, but it works for now.

    def_deserialize_native_endian! {
        fn: deserialize_u16<'de> -> Result<u16, ExpectedU16>,
        visit: visit_u16,
    }

    def_deserialize_native_endian! {
        fn: deserialize_u32<'de> -> Result<u32, ExpectedU32>,
        visit: visit_u32,
    }

    def_deserialize_native_endian! {
        fn: deserialize_u64<'de> -> Result<u64, ExpectedU64>,
        visit: visit_u64,
    }

    fn deserialize_struct<V>(
        self,
        _: &'static str,
        fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        // A struct is essentially a named tuple.
        self.deserialize_tuple(fields.len(), visitor)
    }

    fn deserialize_tuple<V>(mut self, len: usize, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        struct Access<'a, 'de> {
            de: &'a mut Deserializer<'de>,
            len: usize,
        }

        impl<'de> de::SeqAccess<'de> for Access<'_, 'de> {
            type Error = Error;

            fn next_element_seed<T>(
                &mut self,
                seed: T,
            ) -> Result<Option<T::Value>, Error>
            where
                T: de::DeserializeSeed<'de>,
            {
                if self.len > 0 {
                    self.len -= 1;
                    let value = seed.deserialize(&mut *self.de)?;

                    Ok(Some(value))
                } else {
                    Ok(None)
                }
            }
        }

        visitor.visit_seq(Access {
            de: &mut self,
            len,
        })
    }

    fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        let len = usize::from(*self.input.get(0).ok_or(Error::ExpectedU8)?);
        self.input = &self.input[1..];

        let data = self.input.get(..len).ok_or(Error::ExpectedU8)?;
        self.input = &self.input[len..];

        visitor.visit_borrowed_bytes(data)
    }
}

/// See Section 8.1.
#[derive(Debug, Deserialize)]
pub struct DescriptorHeader {
    pub kind: DescriptorKind,
    pub std_id: [u8; 5],
    pub version: u8,
}

/// See Section 8.1.1.
#[derive(Debug, Deserialize_repr)]
#[repr(u8)]
pub enum DescriptorKind {
    BootRecord = 0,
    Primary = 1,
    Secondary = 2,
    Partition = 3,
    SetTerminator = 255,
}

#[derive(Deserialize)]
pub struct StrBuf<const SIZE: usize> {
    // TODO: We shouldn't need to copy this array; this is a limitation of *serde*.
    #[serde(with = "serde_arrays")]
    inner: [u8; SIZE],
}

impl<const SIZE: usize> StrBuf<SIZE> {
    pub fn to_str(&self) -> Result<&str, crate::Error> {
        parse_ascii(&self.inner)
    }
}

impl<const SIZE: usize> fmt::Debug for StrBuf<SIZE> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Ok(string) = self.to_str() {
            write!(f, "\"{}\"", string)
        } else {
            <[u8; SIZE] as fmt::Debug>::fmt(&self.inner, f)
        }
    }
}

/// A Pascal string padded to an even size.
#[derive(Deserialize)]
pub struct PaddedPStr<'a> {
    inner: &'a [u8],
}

impl PaddedPStr<'_> {
    pub fn to_str(&self) -> Result<&str, crate::Error> {
        parse_ascii(self.inner)
    }
}

impl fmt::Debug for PaddedPStr<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Ok(string) = self.to_str() {
            write!(f, "\"{}\"", string)
        } else {
            <[u8] as fmt::Debug>::fmt(self.inner, f)
        }
    }
}

fn parse_ascii(bytes: &[u8]) -> Result<&str, crate::Error> {
    // This is OK because a- and d-characters are encoded in a restricted set of ASCII.
    // Assuming the input is valid ASCII, a UTF-8 representation should be equivalent. If it
    // isn't valid ASCII, the output will probably look garbled, but it was going to look
    // garbled anyway. ¯\_(ツ)_/¯
    let string = std::str::from_utf8(bytes).map_err(crate::Error::DecodeUtf8)?;

    // ISO 9660 and ECMA-119 use ASCII code `0x32`, the space, to denote filler characters at
    // the end of a string buffer. As these are only noise, we will strip them out.
    //
    // TODE: This is inefficient as we are parsing the entire string and then removing
    // characters from the end. Ideally, we should be able to halt the parsing once a filler
    // character is reached.
    let string = string.trim_end_matches(" ");

    Ok(string)
}

pub struct DualEndian<T>(T);

impl<T: fmt::Debug> fmt::Debug for DualEndian<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

macro_rules! impl_deserialize_for_dual_endian {
    (
        ty: $dual_ty:ty => $ty:ty,
        visit: $visit:ident,
        de: $de:ident $(,)?
    ) => {
        impl<'de> de::Deserialize<'de> for DualEndian<$ty> {
            fn deserialize<D>(de: D) -> Result<Self, D::Error>
            where
                D: serde::Deserializer<'de>,
            {
                const BITS: usize = std::mem::size_of::<$ty>() * 8;

                struct Visitor;

                impl<'de> de::Visitor<'de> for Visitor {
                    type Value = DualEndian<$ty>;

                    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
                        write!(f, "a {}-bit dual-endian integer", BITS)
                    }

                    fn $visit<E>(self, v: $dual_ty) -> Result<Self::Value, E>
                    where
                        E: de::Error,
                    {
                        Ok(DualEndian(v as $ty))
                    }
                }

                de.$de(Visitor)
            }
        }

        impl From<DualEndian<$ty>> for $ty {
            fn from(value: DualEndian<$ty>) -> Self {
                value.0
            }
        }
    };
}

impl_deserialize_for_dual_endian! {
    ty: u32 => u16,
    visit: visit_u32,
    de: deserialize_u32,
}

impl_deserialize_for_dual_endian! {
    ty: u64 => u32,
    visit: visit_u64,
    de: deserialize_u64,
}

/// See Section 8.2.
#[derive(Debug, Deserialize)]
pub struct BootRecord {
    pub boot_sys_id: StrBuf<32>,
    pub boot_id: StrBuf<32>,
}

/// See Section 8.4.
#[derive(Derivative, Deserialize)]
#[derivative(Debug)]
pub struct PrimaryDescriptor {
    #[derivative(Debug = "ignore")]
    _unused_0: u8,
    pub sys_id: StrBuf<32>,
    pub vol_id: StrBuf<32>,
    #[derivative(Debug = "ignore")]
    _unused_3: [u8; 8],
    pub vol_space_size: DualEndian<u32>,
    #[derivative(Debug = "ignore")]
    _unused_5: [u8; 32],
    pub vol_set_size: DualEndian<u16>,
    pub vol_seq_num: DualEndian<u16>,
    pub logical_block_size: DualEndian<u16>,
    pub path_table_size: DualEndian<u32>,
    pub l_path_table_addr: DualEndian<u16>,
    // TODO: Make this `Option`.
    pub opt_l_path_table_addr: DualEndian<u16>,
    pub m_path_table_addr: DualEndian<u16>,
    // TODO: Make this `Option`.
    pub opt_m_path_table_addr: DualEndian<u16>,
    // This field isn't [`EntryRecord`] as that has a variable size, but we know that this field
    // must be exactly 34 bytes, so we should fail if that's not that case.
    #[derivative(Debug = "ignore")]
    #[serde(with = "serde_arrays")]
    pub root_dir: [u8; 34],
    pub vol_set_id: StrBuf<128>,
    pub publisher_id: StrBuf<128>,
    pub data_preparer_id: StrBuf<128>,
    pub app_id: StrBuf<128>,
    pub copyright_file_id: StrBuf<37>,
    pub abstract_file_id: StrBuf<37>,
    pub biblio_file_id: StrBuf<37>,
    pub vol_creation_time: AlphaTimestamp,
    pub vol_mod_time: AlphaTimestamp,
    pub vol_expiration_time: AlphaTimestamp,
    pub vol_effective_time: AlphaTimestamp,
    pub file_struct_version: u8,
    #[derivative(Debug = "ignore")]
    _unused_27: u8,
    #[derivative(Debug = "ignore")]
    #[serde(with = "serde_arrays")]
    pub app_data: [u8; 512],
    // The rest is reserved according to the standard.
}

/// See Section 8.4.26.1.
#[derive(Debug, Deserialize)]
pub struct AlphaTimestamp {
    pub year: StrBuf<4>,
    pub month: StrBuf<2>,
    pub day: StrBuf<2>,
    pub hour: StrBuf<2>,
    pub minute: StrBuf<2>,
    pub second: StrBuf<2>,
    pub centisecond: StrBuf<2>,
    pub gmt_offset: u8,
}

/// See Section 9.1.
#[derive(Debug, Deserialize)]
pub struct EntryRecord<'a> {
    pub len: u8,
    pub ext_attr_len: u8,
    pub extent_addr: DualEndian<u32>,
    pub data_len: DualEndian<u32>,
    pub timestamp: NumTimestamp,
    pub file_flags: u8,
    pub file_unit_size: u8,
    pub inter_gap_size: u8,
    pub vol_seq_num: DualEndian<u16>,
    #[serde(borrow)]
    pub file_id: PaddedPStr<'a>,
}

/// See Section 9.1.5.
#[derive(Debug, Deserialize)]
pub struct NumTimestamp {
    pub year: u8,
    pub month: u8,
    pub day: u8,
    pub hour: u8,
    pub minute: u8,
    pub second: u8,
    pub gmt_offset: u8,
}

#[derive(Derivative, Deserialize)]
#[derivative(Debug)]
pub struct PartitionDescriptor {
    #[derivative(Debug = "ignore")]
    _unused_0: u8,
    pub sys_id: StrBuf<32>,
    pub vol_part_id: StrBuf<32>,
    pub vol_part_addr: DualEndian<u32>,
    pub vol_part_size: DualEndian<u32>,
}
