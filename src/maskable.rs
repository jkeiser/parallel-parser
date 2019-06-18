use crate::streamable_bitmask::*;
use packed_simd::*;
use std::ops::{RangeBounds,Bound};

///
/// A series of bytes of a particular size which can be
/// efficiently searched and turned into masks.
/// 
pub trait Maskable<Bitmask>: Sized {
    ///
    /// Match all bytes with the given value
    /// 
    fn where_eq(&self, value: u8) -> Bitmask;

    ///
    /// Match all bytes with the given value
    /// 
    fn where_ne(&self, value: u8) -> Bitmask;

    ///
    /// Match all bytes greater than the given value
    /// 
    fn where_gt(&self, min: u8) -> Bitmask;

    ///
    /// Match all bytes greater than or equal to the given value
    /// 
    fn where_ge(&self, min: u8) -> Bitmask;

    ///
    /// Match all bytes less than the given value
    /// 
    fn where_lt(&self, max: u8) -> Bitmask;

    ///
    /// Match all bytes less than or equal to the given value
    /// 
    fn where_le(&self, max: u8) -> Bitmask;

    ///
    /// Match all bytes within the given range
    ///
    fn where_within(&self, range: impl RangeBounds<u8>) -> Bitmask;

    ///
    /// Match all bytes where the 1's in u8 are set
    ///
    fn where_bits_set(&self, bits: u8) -> Bitmask;

    ///
    /// Match all bytes where the 1's in u8 are not set
    ///
    fn where_bits_not_set(&self, bits: u8) -> Bitmask;
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

#[cfg(test)]
mod tests {
    use super::*;

    fn str_from_mask<T: StreamableBitmask>(mask: T) -> String {
        String::from_utf8(each_bit(mask).map(|bit| if bit { b'X' } else { b' ' }).collect()).unwrap()
    }

    fn expected_prev<'a>(input: &'a [u8], count: usize) -> Vec<u8> {
        let mut expected = vec![0 as u8; count];
        expected.extend_from_slice(input);
        expected
    }

    fn test_prev(input: &str) {
        let input = input.as_bytes();
        let expected = expected_prev(input, 1);
        scan_overflow(chunks64(input).zip(chunks64(&expected)), |(chunk,expected), overflow| {
            let expected_mask = expected.where_eq(b'X');
            let chunk_mask = chunk.where_eq(b'X');
            let (actual_mask, overflow) = chunk_mask.prev(overflow);
            assert_eq!(actual_mask, expected_mask, "\n{:>8}: {}\n{:>8}: {}", "actual", str_from_mask(actual_mask), "expected", str_from_mask(expected_mask));
            (actual_mask, overflow)
        }).for_each(|_|());
    }

    fn test_back(input: &str, count: usize) {
        let input = input.as_bytes();
        let expected = expected_prev(input, count);
        scan_overflow(chunks64(input).zip(chunks64(&expected)), |(chunk, expected), overflow| {
            let expected_mask = expected.where_eq(b'X');
            let chunk_mask = chunk.where_eq(b'X');
            let (actual_mask, overflow) = chunk_mask.back(count as u32, overflow);
            assert_eq!(actual_mask, expected_mask, "\n{:>8}: {}\n{:>8}: {}", "actual", str_from_mask(actual_mask), "expected", str_from_mask(expected_mask));
            (actual_mask, overflow)
        }).for_each(|_|());
    }

    fn test_prev_3(input: &str) {
        println!("-----------------------------");
        println!("{:>9}: {}", "input", input);
        let input = input.as_bytes();
        let expected = expected_prev(input, 1);
        let expected2 = expected_prev(input, 2);
        let expected3 = expected_prev(input, 3);
        use std::str;
        use std::mem::transmute;
        println!("{:>9}: {}", "expected", str::from_utf8(&expected).unwrap());
        println!("{:>9}: {}", "expected2", str::from_utf8(&expected2).unwrap());
        println!("{:>9}: {}", "expected3", str::from_utf8(&expected3).unwrap());

        scan_overflow(chunks64(input).zip(chunks64(&expected)).zip(chunks64(&expected2)).zip(chunks64(&expected3)), |(((chunk, expected), expected2), expected3), overflow| {
            println!("-----------------------------");
            println!("{:>9}: {}", "chunk", str::from_utf8(unsafe { &transmute::<u8x64,[u8;64]>(chunk) }).unwrap());
            println!("{:>9}: {}", "expected", str::from_utf8(unsafe { &transmute::<u8x64,[u8;64]>(expected) }).unwrap());
            println!("{:>9}: {}", "expected2", str::from_utf8(unsafe { &transmute::<u8x64,[u8;64]>(expected2) }).unwrap());
            println!("{:>9}: {}", "expected3", str::from_utf8(unsafe { &transmute::<u8x64,[u8;64]>(expected3) }).unwrap());
            let expected_mask = expected.where_eq(b'X');
            let expected2_mask = expected2.where_eq(b'X');
            let expected3_mask = expected3.where_eq(b'X');
            let chunk_mask = chunk.where_eq(b'X');
            println!("{:>9}: {}", "chunk", str_from_mask(chunk_mask));
            let (actual_mask, actual2_mask, actual3_mask, overflow) = chunk_mask.prev_3(overflow);
            println!("{:>9}: {}", "actual", str_from_mask(actual_mask));
            println!("{:>9}: {}", "actual2", str_from_mask(actual2_mask));
            println!("{:>9}: {}", "actual3", str_from_mask(actual3_mask));
            println!("{:>9}: {}", "expected", str_from_mask(expected_mask));
            println!("{:>9}: {}", "expected2", str_from_mask(expected2_mask));
            println!("{:>9}: {}", "expected3", str_from_mask(expected3_mask));
            assert_eq!(actual_mask, expected_mask, "\n{:>9}: {}\n{:>9}: {}", "actual", str_from_mask(actual_mask), "expected", str_from_mask(expected_mask));
            assert_eq!(actual2_mask, expected2_mask, "\n{:>9}: {}\n{:>9}: {}", "actual2", str_from_mask(actual2_mask), "expected2", str_from_mask(expected2_mask));
            assert_eq!(actual3_mask, expected3_mask, "\n{:>9}: {}\n{:>9}: {}", "actual3", str_from_mask(actual3_mask), "expected3", str_from_mask(expected3_mask));
            (actual_mask, overflow)
        }).for_each(|_|());
    }

    #[test]
    fn prev() {
        test_prev("     X   X X    X X X  X X X X X    X X  X X    X X   X   X X X   X    X  X  X    X    X     X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X ");
        test_prev("     X   X X    X X X  X X X X X    X X  X X    X X   X   X X X   X    X  X  X    X    X    X X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X X");
        test_prev(" X    X   X X    X X X  X X X X X    X X  X X    X X   X   X X X   X    X  X  X    X    X     X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X X");
        test_prev(" X    X   X X    X X X  X X X X X    X X  X X    X X   X   X X X   X    X  X  X    X    X    X X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X X");
    }

    #[test]
    fn back() {
        test_back(" X X     X   X X    X X X  X X X X X    X X  X X    X X   X   X X X   X    X  X  X    X    X   X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X ", 2);
        test_back("   X    X   X X    X X X  X X X X X    X X  X X    X X   X   X X X   X    X  X  X    X    X    X X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X X", 2);
        test_back(" X X X    X   X X    X X X  X X X X X    X X  X X    X X   X   X X X   X    X  X  X    X    X  X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X X", 2);
        test_back("       X   X X    X X X  X X X X X    X X  X X    X X   X   X X X   X    X  X  X    X    X    X_ X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X X", 2);
        test_back(" X X     X   X X    X X X  X X X X X    X X  X X    X X   X   X X X   X    X  X  X    X    X   X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X ", 3);
        test_back("   X    X   X X    X X X  X X X X X    X X  X X    X X   X   X X X   X    X  X  X    X    X    X X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X X", 3);
        test_back(" X X X    X   X X    X X X  X X X X X    X X  X X    X X   X   X X X   X    X  X  X    X    X  X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X X", 3);
        test_back("       X   X X    X X X  X X X X X    X X  X X    X X   X   X X X   X    X  X  X    X    X    X_ X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X X", 3);
    }
    #[test]
    fn prev_3() {
        test_prev_3(" X X     X   X X    X X X  X X X X X    X X  X X    X X   X   X X X   X    X  X  X    X    X   X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X ");
        test_prev_3("   X    X   X X    X X X  X X X X X    X X  X X    X X   X   X X X   X    X  X  X    X    X    X X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X X");
        test_prev_3(" X X X    X   X X    X X X  X X X X X    X X  X X    X X   X   X X X   X    X  X  X    X    X  X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X X");
        test_prev_3("       X   X X    X X X  X X X X X    X X  X X    X X   X   X X X   X    X  X  X    X    X    X_ X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X X");
    }
}
