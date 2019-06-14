use crate::int_traits::*;
use packed_simd::*;
use std::fmt::*;
use std::iter::{Product,Sum};
use std::ops::*;
use std::hash::Hash;


pub trait SimdBase: Copy+Clone+Default+Sized
                   +PartialEq<Self>+Eq
                   +Debug
                   +Not
                   +BitAnd<Self>+BitOr<Self>+BitXor<Self>
                   +BitAndAssign<Self>+BitOrAssign<Self>+BitXorAssign<Self>
                //    +From<[Self::LaneType;Self::LANES]>
                //    +BitAnd<Self::LaneType>+BitOr<Self::LaneType>+BitXor<Self::LaneType>
                //    +BitAndAssign<Self::LaneType>+BitOrAssign<Self::LaneType>+BitXorAssign<Self::LaneType>
{
// FromCast<Simd<[i8; 8]>>
// FromCast<Simd<[u8; 8]>>
// FromCast<Simd<[m8; 8]>>
// FromCast<Simd<[i16; 8]>>
// FromCast<Simd<[u16; 8]>>
// FromCast<Simd<[m16; 8]>>
// FromCast<Simd<[i32; 8]>>
// FromCast<Simd<[u32; 8]>>
// FromCast<Simd<[f32; 8]>>
// FromCast<Simd<[m32; 8]>>
// FromCast<Simd<[i64; 8]>>
// FromCast<Simd<[f64; 8]>>
// FromCast<Simd<[m64; 8]>>
// FromCast<Simd<[isize; 8]>>
// FromCast<Simd<[usize; 8]>>
// FromCast<Simd<[msize; 8]>>
// From<Simd<[u8; 8]>>
// From<Simd<[u16; 8]>>
// From<Simd<[u32; 8]>>
    const LANES: usize;
    const BIT_WIDTH: u32 = Self::LANES as u32 * Self::LaneStorageType::NUM_BITS;
    const BYTE_WIDTH: usize = Self::LANES * Self::LaneStorageType::NUM_BYTES;
    type LaneStorageType: PrimitiveBase;
    type LaneType;
    type Mask: SimdMask;
    type BitmaskResult: PrimitiveUnsigned;

    fn lanes() -> usize;
    fn splat(value: Self::LaneType) -> Self;
    fn extract(self, index: usize) -> Self::LaneType;
    unsafe fn extract_unchecked(self, index: usize) -> Self::LaneType;
    fn replace(self, index: usize, new_value: Self::LaneType) -> Self;
    unsafe fn replace_unchecked(self, index: usize, new_value: Self::LaneType) -> Self;
    fn and(self) -> Self::LaneType;
    fn or(self) -> Self::LaneType;
    fn xor(self) -> Self::LaneType;
    // fn shuffle1_dyn<I>(self, indices: I) -> Self
    // where
    //     Self: Shuffle1Dyn<Indices = I>;
    fn eq(self, other: Self) -> Self::Mask;
    fn ne(self, other: Self) -> Self::Mask;
    fn lt(self, other: Self) -> Self::Mask;
    fn le(self, other: Self) -> Self::Mask;
    fn gt(self, other: Self) -> Self::Mask;
    fn ge(self, other: Self) -> Self::Mask;
    fn partial_lex_ord(&self) -> LexicographicallyOrdered<Self>;
    fn lex_ord(&self) -> LexicographicallyOrdered<Self>;
    fn bitmask(self) -> Self::BitmaskResult;
}

pub trait SimdInteger: SimdBase
        +Hash
        +Octal+Binary+LowerHex+UpperHex
        +Add<Self>+Sub<Self>+Mul<Self>+Div<Self>+Rem<Self>
        +AddAssign<Self>+SubAssign<Self>+MulAssign<Self>+DivAssign<Self>+RemAssign<Self>
        +Shl<Self>+Shr<Self>
        +ShlAssign<Self>+ShrAssign<Self>
        +Shl<u32>+Shr<u32>
        +ShlAssign<u32>+ShrAssign<u32>
        +Product<Self>+Sum<Self>
    //    +Add<Self::LaneType>+Sub<Self::LaneType>+Mul<Self::LaneType>+Div<Self::LaneType>+Rem<Self::LaneType>
    //    +AddAssign<Self::LaneType>+SubAssign<Self::LaneType>+MulAssign<Self::LaneType>+DivAssign<Self::LaneType>+RemAssign<Self::LaneType>
    //    +Product<&Self>+Sum<&Self>
{
    fn rotate_left(self, n: Self) -> Self;
    fn rotate_right(self, n: Self) -> Self;
    fn min(self, x: Self) -> Self;
    fn max(self, x: Self) -> Self;
    fn wrapping_sum(self) -> Self::LaneType;
    fn wrapping_product(self) -> Self::LaneType;
    fn max_element(self) -> Self::LaneType;
    fn min_element(self) -> Self::LaneType;
    fn from_slice_aligned(slice: &[Self::LaneType]) -> Self;
    fn from_slice_unaligned(slice: &[Self::LaneType]) -> Self;
    unsafe fn from_slice_aligned_unchecked(slice: &[Self::LaneType]) -> Self;
    unsafe fn from_slice_unaligned_unchecked(slice: &[Self::LaneType]) -> Self;
    fn write_to_slice_aligned(self, slice: &mut [Self::LaneType]);
    fn write_to_slice_unaligned(self, slice: &mut [Self::LaneType]);
    unsafe fn write_to_slice_aligned_unchecked(self, slice: &mut [Self::LaneType]);
    unsafe fn write_to_slice_unaligned_unchecked(self, slice: &mut [Self::LaneType]);
    fn swap_bytes(self) -> Self;
    fn to_le(self) -> Self;
    fn to_be(self) -> Self;
    fn from_le(x: Self) -> Self;
    fn from_be(x: Self) -> Self;
    fn count_ones(self) -> Self;
    fn count_zeros(self) -> Self;
    fn leading_zeros(self) -> Self;
    fn trailing_zeros(self) -> Self;
}

pub trait SimdUnsigned: SimdInteger {
}

pub trait SimdSigned: SimdInteger {
}

pub trait SimdMask: SimdBase {
    fn all(self) -> bool;
    fn any(self) -> bool;
    fn none(self) -> bool;
    // fn select<T>(self, a: Simd<T>, b: Simd<T>) -> Simd<T>;
}

macro_rules! impl_simd_base {
    ($simd:ty, $storage:ident, $value:ident, $mask:ty, $bitmask:ident) => {
        impl SimdBase for $simd {
            type LaneStorageType = $storage;
            type LaneType = $value;
            type Mask = $mask;
            type BitmaskResult = $bitmask;
            // type Mask = $mask;
            const LANES: usize = Self::lanes();

            fn lanes() -> usize { <$simd>::lanes() }
            fn splat(value: Self::LaneType) -> Self { <$simd>::splat(value) }
            fn extract(self, index: usize) -> Self::LaneType { self.extract(index) }
            unsafe fn extract_unchecked(self, index: usize) -> Self::LaneType { self.extract_unchecked(index) }
            fn replace(self, index: usize, new_value: Self::LaneType) -> Self { self.replace(index, new_value) }
            unsafe fn replace_unchecked(self, index: usize, new_value: Self::LaneType) -> Self { self.replace_unchecked(index, new_value) }
            fn and(self) -> Self::LaneType { self.and() }
            fn or(self) -> Self::LaneType { self.or() }
            fn xor(self) -> Self::LaneType { self.xor() }
            // fn shuffle1_dyn<I>(self, indices: I) -> Self
            // where
            //     Self: Shuffle1Dyn<Indices = I> { self.shuffle1_dyn(indices) }
            fn eq(self, other: Self) -> Self::Mask { self.eq(other) }
            fn ne(self, other: Self) -> Self::Mask { self.ne(other) }
            fn lt(self, other: Self) -> Self::Mask { self.lt(other) }
            fn le(self, other: Self) -> Self::Mask { self.le(other) }
            fn gt(self, other: Self) -> Self::Mask { self.gt(other) }
            fn ge(self, other: Self) -> Self::Mask { self.ge(other) }
            fn partial_lex_ord(&self) -> LexicographicallyOrdered<Self> { self.partial_lex_ord() }
            fn lex_ord(&self) -> LexicographicallyOrdered<Self> { self.lex_ord() }
            fn bitmask(self) -> Self::BitmaskResult { self.bitmask() }
        }
    }
}

macro_rules! impl_simd_integer {
    ($simd:ty, $int:ident, $mask:ty, $bitmask:ident) => {
        impl_simd_base!($simd, $int, $int, $mask, $bitmask);
        impl SimdInteger for $simd {
            fn rotate_left(self, n: Self) -> Self { self.rotate_left(n) }
            fn rotate_right(self, n: Self) -> Self { self.rotate_right(n) }
            fn min(self, x: Self) -> Self { self.min(x) }
            fn max(self, x: Self) -> Self { self.max(x) }
            fn wrapping_sum(self) -> Self::LaneType { self.wrapping_sum() }
            fn wrapping_product(self) -> Self::LaneType { self.wrapping_product() }
            fn max_element(self) -> Self::LaneType { self.max_element() }
            fn min_element(self) -> Self::LaneType { self.min_element() }
            fn from_slice_aligned(slice: &[Self::LaneType]) -> Self { <$simd>::from_slice_aligned(slice) }
            fn from_slice_unaligned(slice: &[Self::LaneType]) -> Self { <$simd>::from_slice_unaligned(slice) }
            unsafe fn from_slice_aligned_unchecked(slice: &[Self::LaneType]) -> Self { <$simd>::from_slice_aligned_unchecked(slice) }
            unsafe fn from_slice_unaligned_unchecked(slice: &[Self::LaneType]) -> Self { <$simd>::from_slice_unaligned_unchecked(slice) }
            fn write_to_slice_aligned(self, slice: &mut [Self::LaneType]) { self.write_to_slice_aligned(slice) }
            fn write_to_slice_unaligned(self, slice: &mut [Self::LaneType]) { self.write_to_slice_unaligned(slice) }
            unsafe fn write_to_slice_aligned_unchecked(self, slice: &mut [Self::LaneType]) { self.write_to_slice_aligned_unchecked(slice) }
            unsafe fn write_to_slice_unaligned_unchecked(self, slice: &mut [Self::LaneType]) { self.write_to_slice_unaligned_unchecked(slice) }
            fn swap_bytes(self) -> Self { self.swap_bytes() }
            fn to_le(self) -> Self { self.to_le() }
            fn to_be(self) -> Self { self.to_be() }
            fn from_le(x: Self) -> Self { <$simd>::from_le(x) }
            fn from_be(x: Self) -> Self { <$simd>::from_be(x) }
            fn count_ones(self) -> Self { self.count_ones() }
            fn count_zeros(self) -> Self { self.count_zeros() }
            fn leading_zeros(self) -> Self { self.leading_zeros() }
            fn trailing_zeros(self) -> Self { self.trailing_zeros() }
        }
    }
}

macro_rules! impl_simd_unsigned {
    ($simd:ty, $int:ident, $mask:ty, $bitmask:ident) => {
        impl_simd_integer!($simd, $int, $mask, $bitmask);
        impl SimdUnsigned for $simd {
        }
    }
}

macro_rules! impl_simd_signed {
    ($simd:ty, $int:ident, $mask:ty, $bitmask:ident) => {
        impl_simd_integer!($simd, $int, $mask, $bitmask);
        impl SimdSigned for $simd {
        }
    }
}

macro_rules! impl_simd_mask {
    ($simd:ty, $prim:ident, $bitmask:ident) => {
        impl_simd_base!($simd, $prim, bool, $simd, $bitmask);
        impl SimdMask for $simd {
            fn all(self) -> bool { self.all() }
            fn any(self) -> bool { self.any() }
            fn none(self) -> bool { self.none() }
            // fn select<T>(self, a: Simd<T>, b: Simd<T>) -> Simd<T> { self.select(a, b) }
        }
    }
}

impl_simd_unsigned!(u8x2, u8, m8x2, u8);
impl_simd_unsigned!(u8x4, u8, m8x4, u8);
impl_simd_unsigned!(u8x8, u8, m8x8, u8);
impl_simd_unsigned!(u8x16, u8, m8x16, u16);
impl_simd_unsigned!(u8x32, u8, m8x32, u32);
impl_simd_unsigned!(u8x64, u8, m8x64, u64);

impl_simd_unsigned!(u16x2, u16, m16x2, u8);
impl_simd_unsigned!(u16x4, u16, m16x4, u8);
impl_simd_unsigned!(u16x8, u16, m16x8, u8);
impl_simd_unsigned!(u16x16, u16, m16x16, u16);
impl_simd_unsigned!(u16x32, u16, m16x32, u32);

impl_simd_unsigned!(u32x2, u32, m32x2, u8);
impl_simd_unsigned!(u32x4, u32, m32x4, u8);
impl_simd_unsigned!(u32x8, u32, m32x8, u8);
impl_simd_unsigned!(u32x16, u32, m32x16, u16);

impl_simd_unsigned!(u64x2, u64, m64x2, u8);
impl_simd_unsigned!(u64x4, u64, m64x4, u8);
impl_simd_unsigned!(u64x8, u64, m64x8, u8);

impl_simd_unsigned!(u128x2, u128, m128x2, u8);
impl_simd_unsigned!(u128x4, u128, m128x4, u8);

impl_simd_signed!(i8x2, i8, m8x2, u8);
impl_simd_signed!(i8x4, i8, m8x4, u8);
impl_simd_signed!(i8x8, i8, m8x8, u8);
impl_simd_signed!(i8x16, i8, m8x16, u16);
impl_simd_signed!(i8x32, i8, m8x32, u32);
impl_simd_signed!(i8x64, i8, m8x64, u64);

impl_simd_signed!(i16x2, i16, m16x2, u8);
impl_simd_signed!(i16x4, i16, m16x4, u8);
impl_simd_signed!(i16x8, i16, m16x8, u8);
impl_simd_signed!(i16x16, i16, m16x16, u16);
impl_simd_signed!(i16x32, i16, m16x32, u32);

impl_simd_signed!(i32x2, i32, m32x2, u8);
impl_simd_signed!(i32x4, i32, m32x4, u8);
impl_simd_signed!(i32x8, i32, m32x8, u8);
impl_simd_signed!(i32x16, i32, m32x16, u16);

impl_simd_signed!(i64x2, i64, m64x2, u8);
impl_simd_signed!(i64x4, i64, m64x4, u8);
impl_simd_signed!(i64x8, i64, m64x8, u8);

impl_simd_signed!(i128x2, i128, m128x2, u8);
impl_simd_signed!(i128x4, i128, m128x4, u8);

impl_simd_mask!(m8x2, m8, u8);
impl_simd_mask!(m8x4, m8, u8);
impl_simd_mask!(m8x8, m8, u8);
impl_simd_mask!(m8x16, m8, u16);
impl_simd_mask!(m8x32, m8, u32);
impl_simd_mask!(m8x64, m8, u64);

impl_simd_mask!(m16x2, m16, u8);
impl_simd_mask!(m16x4, m16, u8);
impl_simd_mask!(m16x8, m16, u8);
impl_simd_mask!(m16x16, m16, u16);
impl_simd_mask!(m16x32, m16, u32);

impl_simd_mask!(m32x2, m32, u8);
impl_simd_mask!(m32x4, m32, u8);
impl_simd_mask!(m32x8, m32, u8);
impl_simd_mask!(m32x16, m32, u16);

impl_simd_mask!(m64x2, m64, u8);
impl_simd_mask!(m64x4, m64, u8);
impl_simd_mask!(m64x8, m64, u8);

impl_simd_mask!(m128x2, m128, u8);
impl_simd_mask!(m128x4, m128, u8);
