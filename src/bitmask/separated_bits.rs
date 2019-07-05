use super::maskable::*;
use super::streamable_bitmask::*;
use packed_simd::*;
use std::ops::{Bound,Index,IndexMut,RangeBounds,RangeInclusive};

#[derive(Debug,Default)]
pub struct SeparatedBits([u64x8;8]);

fn to_inclusive_range(range: &impl RangeBounds<u8>) -> RangeInclusive<u8> {
    use Bound::*;
    let start = match range.start_bound() {
        Unbounded => 0,
        Included(&min) => min,
        Excluded(&min) => min+1,
    };
    let end = match range.end_bound() {
        Unbounded => u8::max_value(),
        Included(&max) => max,
        Excluded(&max) => max-1,
    };
    start..=end
}

pub fn separate_bits(bytes: &[u8;512]) -> SeparatedBits {
    // TODO it takes 64 operations to turn the bits into separate masks. There MUST be a better way.
    let segments = [
        u8x64::from_slice_unaligned(&bytes[64*0..64*1]),
        u8x64::from_slice_unaligned(&bytes[64*1..64*2]),
        u8x64::from_slice_unaligned(&bytes[64*2..64*3]),
        u8x64::from_slice_unaligned(&bytes[64*3..64*4]),
        u8x64::from_slice_unaligned(&bytes[64*4..64*5]),
        u8x64::from_slice_unaligned(&bytes[64*5..64*6]),
        u8x64::from_slice_unaligned(&bytes[64*6..64*7]),
        u8x64::from_slice_unaligned(&bytes[64*7..64*8]),
    ];
    let where_bit_set = |n| {
        let bit: u8 = (1 as u8) << n;
        u64x8::new(
            (segments[0] & bit).ne(u8x64::splat(0)).bitmask(),
            (segments[1] & bit).ne(u8x64::splat(0)).bitmask(),
            (segments[2] & bit).ne(u8x64::splat(0)).bitmask(),
            (segments[3] & bit).ne(u8x64::splat(0)).bitmask(),
            (segments[4] & bit).ne(u8x64::splat(0)).bitmask(),
            (segments[5] & bit).ne(u8x64::splat(0)).bitmask(),
            (segments[6] & bit).ne(u8x64::splat(0)).bitmask(),
            (segments[7] & bit).ne(u8x64::splat(0)).bitmask(),
        )
    };
    SeparatedBits([
        where_bit_set(0),
        where_bit_set(1),
        where_bit_set(2),
        where_bit_set(3),
        where_bit_set(4),
        where_bit_set(5),
        where_bit_set(6),
        where_bit_set(7),
    ])
}

impl SeparatedBits {
    pub fn bit(&self, n: usize) -> u64x8 {
        self.0[n]
    }

    pub fn where_bits_eq(&self, b: u8, bits: impl Iterator<Item=usize>) -> u64x8 {
        bits.fold(u64x8::ALL_BITS, |acc, n| acc & !(self[n] ^ mask_from_bool(b.bit(n))))
    }
    pub fn where_bits_ne(&self, b: u8, bits: impl Iterator<Item=usize>) -> u64x8 {
        bits.fold(u64x8::ALL_BITS, |acc, n| acc & (self[n] ^ mask_from_bool(b.bit(n))))
    }
    pub fn where_bits_le(&self, b: u8, bits: impl DoubleEndedIterator<Item=usize>) -> u64x8 {
        !self.where_bits_gt(b, bits)
    }
    pub fn where_bits_lt(&self, b: u8, bits: impl DoubleEndedIterator<Item=usize>) -> u64x8 {
        !self.where_bits_ge(b, bits)
    }
    pub fn where_bits_ge(&self, b: u8, bits: impl DoubleEndedIterator<Item=usize>) -> u64x8 {
        let (result, decided) = self.bits_gt(b, bits);
        result | !decided
    }
    pub fn where_bits_gt(&self, b: u8, bits: impl DoubleEndedIterator<Item=usize>) -> u64x8 {
        let (result, _) = self.bits_gt(b, bits);
        result
    }
    pub fn where_bits_within(&self, range: impl RangeBounds<u8>, bits: impl DoubleEndedIterator<Item=usize>+Clone) -> u64x8 {
        let range = to_inclusive_range(&range);
        // XOR them so we get a 1 wherever the bits are different
        let eq_bits = (0..8).find(|&n| range.start().bit(n) != range.end().bit(n)).unwrap_or(8);
        // If the remaining bits are all different *and* == 0/1, they are full range comparisons.
        let all_full_range = (eq_bits..8).all(|n| !range.start().bit(n) && range.end().bit(n));
        let within_bits = if all_full_range { 8 } else { eq_bits };
        let eq_bits = bits.clone().filter(|&n| n < eq_bits);
        let within_bits = bits.filter(|&n| n >= within_bits);

        // The special case here is for bounds, 
        use Bound::*;
        let min = match range.start_bound() { Unbounded => 0, Included(&min) => min, Excluded(&min) => min+1 };
        self.where_bits_eq(min, eq_bits) & match (range.start_bound(), range.end_bound()) {
            (Unbounded, Unbounded) => return u64x8::ALL_BITS,
            (Unbounded, Included(&max)) => return self.where_bits_le(max, within_bits),
            (Unbounded, Excluded(&max)) => return self.where_bits_lt(max, within_bits),
            (Included(&min), Unbounded) => return self.where_bits_ge(min, within_bits),
            (Excluded(&min), Unbounded) => return self.where_bits_gt(min, within_bits),
            (Included(&min), Included(&max)) => self.where_bits_ge(min, within_bits.clone()) & self.where_bits_le(max, within_bits),
            (Included(&min), Excluded(&max)) => self.where_bits_ge(min, within_bits.clone()) & self.where_bits_lt(max, within_bits),
            (Excluded(&min), Included(&max)) => self.where_bits_gt(min, within_bits.clone()) & self.where_bits_le(max, within_bits),
            (Excluded(&min), Excluded(&max)) => self.where_bits_gt(min, within_bits.clone()) & self.where_bits_lt(max, within_bits),
        }
    }
    pub fn where_bits_same(&self, bits: impl Iterator<Item=usize>) -> u64x8 {
        !self.where_bits_different(bits)
    }
    pub fn where_bits_different(&self, mut bits: impl Iterator<Item=usize>) -> u64x8 {
        let first = self[bits.next().unwrap()];
        let mut result = u64x8::NO_BITS;
        for bit in bits {
            result |= first ^ self[bit];
        }
        result
    }
    pub fn where_within_ascii_case_insensitive(&self, range: impl RangeBounds<u8>) -> u64x8 {
        // The 5th bit is the only difference between lower and uppercase
        self.where_bits_within(range, (0..=4).chain(6..=7))
    }
    fn bits_gt(&self, b: u8, bits: impl DoubleEndedIterator<Item=usize>) -> (u64x8, u64x8) {
        let mut result = u64x8::NO_BITS;
        let mut decided = u64x8::NO_BITS;

        // BRANCH NOTE: we're assuming the compiler is smart enough to unroll the loop and
        // optimize this if away for constant values of b. Test that assumption.
        for n in bits.rev() {
            let a = self[n];
            println!("Comparing to bit {} of {}: {}", n, b, b.bit(n));
            // println!("a: {:?}", a);
            println!("{:10}: {:064b}", "a", a.extract(0).reverse_bits());
            if b.bit(n) {
                // b == 1
                //  a | b | decided | result | Case    |
                // ---|---|---------|--------|---------|
                //  0 | 1 | 0 -> 1  | 0      | a <  b  |
                //  0 | 1 | 1       | 0      | a <  b  |
                //  0 | 1 | 1       | 1      | a >  b  |
                //  1 | 1 | 0       | 0      | a == b? |
                //  1 | 1 | 1       | 0      | a <  b  |
                //  1 | 1 | 1       | 1      | a >  b  |
                // Note in the above table, result doesn't ever get flipped here (because it
                // defaults to 0, so we just have to set decided if we've figured out the answer)
                decided |= !a;
            } else {
                // In all cases here, b == 0
                //  a | b | decided | result | Case    |
                // ---|---|---------|--------|---------|
                //  0 | 0 | 0       | 0      | a == b? |
                //  0 | 0 | 1       | 0      | a <  b  |
                //  0 | 0 | 1       | 1      | a >  b  |
                //  1 | 0 | 0 -> 1  | 0 -> 1 | a >  b  |
                //  1 | 0 | 1       | 0      | a <  b  |
                //  1 | 0 | 1       | 1      | a >  b  |
                result = (result & decided) | (a & !decided);
                decided |= a;
            }
            println!("{:10}: {:064b}", "result", result.extract(0).reverse_bits());
            println!("{:10}: {:064b}", "decided", decided.extract(0).reverse_bits());
            // println!("result: {:?}", result);
            // println!("decided: {:?}", decided);
        }

        // NOTE: we're assuming that any work on any low zeroes will be optimized away as well,
        // since zeroes can only affect decided, not the result.
        (result, decided)
    }
}

trait GetBit {
    fn bit(&self, n: usize) -> bool;
}

impl GetBit for u8 {
    fn bit(&self, n: usize) -> bool {
        let mask = (1 as u8) << n;
        (self & mask) > 0
    }
}

fn mask_from_bool(b: bool) -> u64x8 {
    m64x8::splat(b).into_bits()
}

impl Maskable<u64x8> for SeparatedBits {
    fn where_eq(&self, b: u8) -> u64x8 {
        self.where_bits_eq(b, 0..8)
    }
    fn where_ne(&self, b: u8) -> u64x8 {
        self.where_bits_ne(b, 0..8)
    }
    fn where_gt(&self, b: u8) -> u64x8 {
        self.where_bits_gt(b, 0..8)
    }
    fn where_ge(&self, b: u8) -> u64x8 {
        self.where_bits_ge(b, 0..8)
    }
    fn where_lt(&self, b: u8) -> u64x8 {
        self.where_bits_lt(b, 0..8)
    }
    fn where_le(&self, b: u8) -> u64x8 {
        self.where_bits_le(b, 0..8)
    }
    fn where_within(&self, range: impl RangeBounds<u8>) -> u64x8 {
        self.where_bits_within(range, 0..8)
    }
    fn where_bits_set(&self, bits: u8) -> u64x8 {
        let check_bit = |n| {
            if bits.bit(n) {
                self[n] ^ u64x8::NO_BITS
            } else {
                u64x8::ALL_BITS
            }
        };
        check_bit(0) & check_bit(1) & check_bit(2) & check_bit(3) & check_bit(4) & check_bit(5) & check_bit(6) & check_bit(7)
    }
    fn where_bits_not_set(&self, bits: u8) -> u64x8 {
        let check_bit = |n| {
            if bits.bit(n) {
                self[n] ^ u64x8::ALL_BITS
            } else {
                u64x8::ALL_BITS
            }
        };
        check_bit(0) & check_bit(1) & check_bit(2) & check_bit(3) & check_bit(4) & check_bit(5) & check_bit(6) & check_bit(7)
    }
}

impl Index<usize> for SeparatedBits {
    type Output = u64x8;
    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

impl IndexMut<usize> for SeparatedBits {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.0[index]
    }
}
