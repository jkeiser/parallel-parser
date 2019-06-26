use crate::streamable_bitmask::*;
use packed_simd::*;
use std::ops::{Bound,RangeBounds};

///
/// A series of bytes of a particular size which can be
/// efficiently searched and turned into masks.
/// 
pub trait Maskable<T: StreamableBitmask>: Sized {
    ///
    /// Match all bytes with the given value
    /// 
    fn where_eq(&self, value: u8) -> T;

    ///
    /// Match all bytes with the given value
    /// 
    fn where_ne(&self, value: u8) -> T;

    ///
    /// Match all bytes greater than the given value
    /// 
    fn where_gt(&self, min: u8) -> T;

    ///
    /// Match all bytes greater than or equal to the given value
    /// 
    fn where_ge(&self, min: u8) -> T;

    ///
    /// Match all bytes less than the given value
    /// 
    fn where_lt(&self, max: u8) -> T;

    ///
    /// Match all bytes less than or equal to the given value
    /// 
    fn where_le(&self, max: u8) -> T;

    ///
    /// Match all bytes within the given range
    ///
    fn where_within(&self, range: impl RangeBounds<u8>) -> T;

    ///
    /// Match all bytes where the 1's in u8 are set
    ///
    fn where_bits_set(&self, bits: u8) -> T;

    ///
    /// Match all bytes where the 1's in u8 are not set
    ///
    fn where_bits_not_set(&self, bits: u8) -> T;
}

impl Maskable<u64> for u8x64 {
    fn where_eq(&self, value: u8) -> u64 { Self::eq(*self, Self::splat(value)).bitmask() }
    fn where_ne(&self, value: u8) -> u64 { Self::ne(*self, Self::splat(value)).bitmask() }
    fn where_gt(&self, value: u8) -> u64 { Self::gt(*self, Self::splat(value)).bitmask() }
    fn where_ge(&self, value: u8) -> u64 { Self::ge(*self, Self::splat(value)).bitmask() }
    fn where_lt(&self, value: u8) -> u64 { Self::lt(*self, Self::splat(value)).bitmask() }
    fn where_le(&self, value: u8) -> u64 { Self::le(*self, Self::splat(value)).bitmask() }
    fn where_within(&self, range: impl RangeBounds<u8>) -> u64 {
        use Bound::*;
        match (range.start_bound(), range.end_bound()) {
            (Unbounded, Unbounded) => return u64::ALL_BITS,
            (Unbounded, Included(max)) => self.where_le(*max),
            (Unbounded, Excluded(max)) => self.where_lt(*max),
            (Included(min), Unbounded) => self.where_ge(*min),
            (Excluded(min), Unbounded) => self.where_gt(*min),
            (Included(min), Included(max)) => self.where_ge(*min) & self.where_le(*max),
            (Included(min), Excluded(max)) => self.where_ge(*min) & self.where_lt(*max),
            (Excluded(min), Included(max)) => self.where_gt(*min) & self.where_le(*max),
            (Excluded(min), Excluded(max)) => self.where_gt(*min) & self.where_lt(*max),
        }
    }
    fn where_bits_set(&self, bits: u8) -> u64 {
        (*self & bits).eq(Self::splat(0x00)).bitmask()
    }
    fn where_bits_not_set(&self, bits: u8) -> u64 {
        (!*self & bits).eq(Self::splat(0x00)).bitmask()
    }
}

trait SimdFrom<T> {
    fn simd_from_le(from: T) -> Self;
}
impl SimdFrom<&[u8]> for u8x64 {
    fn simd_from_le(from: &[u8]) -> Self {
        assert!(from.len() <= 64);
        let result = if from.len() < 64 {
            let mut chunk64: [u8;64] = [0;64];
            chunk64[..from.len()].copy_from_slice(from);
            unsafe { u8x64::from_slice_unaligned_unchecked(&chunk64) }
        } else {
            unsafe { u8x64::from_slice_unaligned_unchecked(from) }
        };
        u8x64::from_le(result)
    }
}
trait IntoSimd<T> {
    fn into_simd_le(self) -> T;
}
impl<From, To: SimdFrom<From>> IntoSimd<To> for From {
    fn into_simd_le(self) -> To {
        To::simd_from_le(self)
    }
}
pub fn chunks64<'a>(input: &'a [u8]) -> impl Iterator<Item=u8x64>+'a {
    input.chunks(64).map(SimdFrom::simd_from_le)
}
pub fn scan_overflow<'a, Input: Iterator+'a, Output, Overflow: Default+Copy+'a, F: 'a+FnMut(Input::Item, Overflow) -> (Output, Overflow)>(input: Input, mut f: F) -> impl Iterator<Item=Output>+'a {
    input.scan(Overflow::default(), move |overflow, item| { let (result, new_overflow) = f(item, *overflow); *overflow = new_overflow; Some(result) })
}

