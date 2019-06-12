use crate::int_traits::{NativeInteger, NativeUnsigned, NativeSigned};
use packed_simd::*;

pub trait SimdBase {
    const LANES: usize;
    const LANE_WIDTH: u32;
    const BIT_WIDTH: usize = Self::LANES * Self::LANE_WIDTH as usize;
    const BYTE_WIDTH: usize = Self::BIT_WIDTH * 8;
}
pub trait SimdInteger: SimdBase {
    type LaneType: NativeInteger;
}

pub trait SimdUnsigned: SimdInteger {
    type UnsignedLaneType: NativeUnsigned;
}

pub trait SimdSigned: SimdInteger {
    type SignedLaneType: NativeSigned;
}

pub trait SimdMask: SimdBase {
}

macro_rules! impl_simd_integer {
    ($simd:ty, $int:ident) => {
        impl SimdBase for $simd {
            const LANES: usize = <$simd>::lanes();
            const LANE_WIDTH: u32 = <$int>::WIDTH;
        }
        impl SimdInteger for $simd {
            type LaneType = $int;
        }
    }
}

macro_rules! impl_simd_unsigned {
    ($simd:ty, $int:ident) => {
        impl_simd_integer!($simd, $int);
        impl SimdUnsigned for $simd {
            type UnsignedLaneType = $int;
        }
    }
}

macro_rules! impl_simd_signed {
    ($simd:ty, $int:ident) => {
        impl_simd_integer!($simd, $int);
        impl SimdSigned for $simd {
            type SignedLaneType = $int;
        }
    }
}

macro_rules! impl_simd_mask {
    ($simd:ty, $width:expr) => {
        impl SimdBase for $simd {
            const LANES: usize = <$simd>::lanes();
            const LANE_WIDTH: u32 = $width;
        }
        impl SimdMask for $simd {
        }
    }
}


impl_simd_unsigned!(u8x2, u8);
impl_simd_unsigned!(u8x4, u8);
impl_simd_unsigned!(u8x8, u8);
impl_simd_unsigned!(u8x16, u8);
impl_simd_unsigned!(u8x32, u8);
impl_simd_unsigned!(u8x64, u8);

impl_simd_unsigned!(u16x2, u16);
impl_simd_unsigned!(u16x4, u16);
impl_simd_unsigned!(u16x8, u16);
impl_simd_unsigned!(u16x16, u16);
impl_simd_unsigned!(u16x32, u16);

impl_simd_unsigned!(u32x2, u32);
impl_simd_unsigned!(u32x4, u32);
impl_simd_unsigned!(u32x8, u32);
impl_simd_unsigned!(u32x16, u32);

impl_simd_unsigned!(u64x2, u64);
impl_simd_unsigned!(u64x4, u64);
impl_simd_unsigned!(u64x8, u64);

impl_simd_unsigned!(u128x2, u128);
impl_simd_unsigned!(u128x4, u128);

impl_simd_signed!(i8x2, i8);
impl_simd_signed!(i8x4, i8);
impl_simd_signed!(i8x8, i8);
impl_simd_signed!(i8x16, i8);
impl_simd_signed!(i8x32, i8);
impl_simd_signed!(i8x64, i8);

impl_simd_signed!(i16x2, i16);
impl_simd_signed!(i16x4, i16);
impl_simd_signed!(i16x8, i16);
impl_simd_signed!(i16x16, i16);
impl_simd_signed!(i16x32, i16);

impl_simd_signed!(i32x2, i32);
impl_simd_signed!(i32x4, i32);
impl_simd_signed!(i32x8, i32);
impl_simd_signed!(i32x16, i32);

impl_simd_signed!(i64x2, i64);
impl_simd_signed!(i64x4, i64);
impl_simd_signed!(i64x8, i64);

impl_simd_signed!(i128x2, i128);
impl_simd_signed!(i128x4, i128);

impl_simd_mask!(m8x2, 8);
impl_simd_mask!(m8x4, 8);
impl_simd_mask!(m8x8, 8);
impl_simd_mask!(m8x16, 8);
impl_simd_mask!(m8x32, 8);
impl_simd_mask!(m8x64, 8);

impl_simd_mask!(m16x2, 16);
impl_simd_mask!(m16x4, 16);
impl_simd_mask!(m16x8, 16);
impl_simd_mask!(m16x16, 16);
impl_simd_mask!(m16x32, 16);

impl_simd_mask!(m32x2, 32);
impl_simd_mask!(m32x4, 32);
impl_simd_mask!(m32x8, 32);
impl_simd_mask!(m32x16, 32);

impl_simd_mask!(m64x2, 64);
impl_simd_mask!(m64x4, 64);
impl_simd_mask!(m64x8, 64);

impl_simd_mask!(m128x2, 128);
impl_simd_mask!(m128x4, 128);
