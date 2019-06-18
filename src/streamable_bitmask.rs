use crate::int_traits::*;
use crate::carryless_mul::*;
use packed_simd::*;
use std::ops::{BitAnd,BitOr,BitXor,Not,BitAndAssign,BitOrAssign,BitXorAssign};
use std::fmt::Debug;

pub trait StreamableBitmask: Sized
    +Copy+Clone+Debug
    +Not<Output=Self>
    +BitAnd<Self,Output=Self>+BitOr<Self,Output=Self>+BitXor<Self,Output=Self>
    +BitAndAssign<Self>+BitOrAssign<Self>+BitXorAssign<Self> {
    const NUM_BITS: u32;
    const ALL_BITS: Self;
    const NO_BITS: Self;
    const EVEN_BITS: Self;
    const ODD_BITS: Self;
    const TOP_BIT: Self;

    type Overflow: PrimitiveInteger;

    fn streaming_shift_forward(self, count: u32, overflow: Self::Overflow, overflow_count: u32) -> (Self, Self::Overflow);
    fn streaming_add(self, other: Self, overflow: bool) -> (Self, bool);
    fn streaming_clmul(self, other: Self::Overflow, overflow: Self::Overflow) -> (Self, Self::Overflow);
    fn from_overflow(overflow: Self::Overflow) -> Self;
    fn from_bool_overflow(overflow: bool) -> Self;

    ///
    /// The *previous* value (shifted over 1).
    /// 
    /// Zeroes are shifted in at the very beginning.
    /// 
    /// e.g. prev(01111010) = 00111101...
    /// 
    fn prev(self, overflow: Self::Overflow) -> (Self, Self::Overflow) {
        self.streaming_shift_forward(1, overflow, 1)
    }

    ///
    /// The *previous* value (shifted over 1).
    /// 
    /// Zeroes are shifted in at the very beginning.
    /// 
    /// e.g. prev(01111010) = 00111101...
    /// 
    fn back(self, count: u32, overflow: Self::Overflow) -> (Self, Self::Overflow) {
        self.streaming_shift_forward(count, overflow, count)
    }

    ///
    /// The values 1 and 2 places back.
    /// 
    /// Zeroes are shifted in at the very beginning.
    /// 
    /// e.g. back(01111010, 3) = 00001111...
    ///
    fn prev_2(self, overflow: Self::Overflow) -> (Self, Self, Self::Overflow) {
        let (prev1, _) = self.streaming_shift_forward(1, overflow, 2);
        let (prev2, overflow) = self.streaming_shift_forward(2, overflow, 2);
        (prev1, prev2, overflow)
    }

    ///
    /// The values 1, 2, and 3 places back.
    /// 
    /// Zeroes are shifted in at the very beginning.
    /// 
    /// e.g. back(01111010, 3) = 00001111...
    ///
    fn prev_3(self, overflow: Self::Overflow) -> (Self, Self, Self, Self::Overflow) {
        let (prev1, _) = self.streaming_shift_forward(1, overflow, 3);
        let (prev2, _) = self.streaming_shift_forward(2, overflow, 3);
        let (prev3, overflow) = self.streaming_shift_forward(3, overflow, 3);
        (prev1, prev2, prev3, overflow)
    }

    ///
    /// Determine whether any bits are set.
    /// 
    /// Returns true if any bits are set, false otherwise.
    ///
    fn any(self) -> bool;

    ///
    /// Determine whether any bits are set.
    /// 
    /// Returns true if any bits are set, false otherwise.
    ///
    fn all(self) -> bool;
    
    ///
    /// The starts of any series of 1's: flips on the bit *at* the beginning of a run of 1's.
    ///
    /// e.g. starts(01111010) = 01000010
    /// 
    fn starts(self, overflow: Self::Overflow) -> (Self, Self::Overflow) {
        let (prev, overflow) = self.prev(overflow);
        (self & !prev, overflow)
    }

    ///
    /// The ends of any series of 1's: flips on the bit *after* any run of 1's.
    ///
    /// e.g. after_series(01111010) = 00000101
    /// 
    fn after_series(self, overflow: Self::Overflow) -> (Self, Self::Overflow) {
        let (prev, overflow) = self.prev(overflow);
        (!self & prev, overflow)
    }

    ///
    /// The ends of any *odd-length* series of 1's.
    /// 
    /// Flips on the bit *after* such series.
    /// 
    /// e.g. after_odd_series(01111010) = 00000001
    /// 
    fn after_odd_series(self, overflow: AfterOddSeriesOverflow<Self::Overflow>) -> (Self, AfterOddSeriesOverflow<Self::Overflow>) {
        // NOTE: the overflow could be done more efficiently with an Option<bool> where None indicates no overflow, false indicates even overflow and true indicates odd overflow.
        // It could be done even MORE efficiently with a single bool ("was there odd overflow") as long as we're willing to sometimes mark the middle of the series as "the end of an odd run." (Could mask it back off, too.)
        let (starts, start_overflow) = self.starts(overflow.start_overflow);
        let (even_ends, even_overflow) = (starts & Self::EVEN_BITS).streaming_add(self, overflow.even_overflow);
        let (odd_ends, odd_overflow) = (starts & Self::ODD_BITS).streaming_add(self, overflow.odd_overflow);
        let after_odd_series = (even_ends & Self::ODD_BITS) | (odd_ends & Self::EVEN_BITS);
        (after_odd_series, AfterOddSeriesOverflow { start_overflow, even_overflow, odd_overflow })
    }

    ///
    /// Fills in 1's between every pair of 1's.
    /// 
    /// Includes the *starting* 1 of each pair, but does not include the *trailing* 1.
    /// 
    /// e.g. between_pairs(0100100110) = (0111000100)
    /// 
    fn between_pairs(self, overflow: Self::Overflow) -> (Self, Self::Overflow) {
        let zero: Self::Overflow = false.into();
        self.streaming_clmul(!zero, overflow)
    }

    ///
    /// Get the value of a single bit.
    /// 
    fn get_bit(self, at: u32) -> bool;
}

pub fn each_bit<'a, T: StreamableBitmask+'a>(mask: T) -> impl Iterator<Item=bool>+'a {
    (0..T::NUM_BITS).map(move |i| mask.get_bit(i))
}

#[derive(Clone,Debug,Default)]
pub struct AfterOddSeriesOverflow<I: PrimitiveInteger> {
    pub(crate) start_overflow: I,
    pub(crate) odd_overflow: bool,
    pub(crate) even_overflow: bool
}

impl StreamableBitmask for u64x8 {
    const NUM_BITS: u32 = 512;
    const NO_BITS: Self = Self::splat(u64::NO_BITS);
    const ALL_BITS: Self = Self::splat(u64::ALL_BITS);
    const EVEN_BITS: Self = Self::splat(u64::EVEN_BITS);
    const ODD_BITS: Self = Self::splat(u64::ODD_BITS);
    const TOP_BIT: Self = u64x8::new(u64::TOP_BIT, 0, 0, 0, 0, 0, 0, 0);
    type Overflow = u64;

    fn streaming_shift_forward(self, count: u32, overflow: Self::Overflow, overflow_count: u32) -> (Self, Self::Overflow) {
        assert!(count < u64x8::lanes() as u32*64);
        assert!(overflow_count <= Self::Overflow::BIT_WIDTH);
        assert!(count <= overflow_count);
        let prev_overflow = overflow >> (overflow_count-count);

        // Example we'll look at throughout:
        //
        //  1111111111111111_2222222222222222_3333333333333333_4444444444444444_5555555555555555_6666666666666666_7777777777777777_8888888888888888
        //  ror 128
        //  -------------------------------------------------------------------
        //  777777777777777_88888888888888888_1111111111111111_2222222222222222_3333333333333333_4444444444444444_5555555555555555_6666666666666666
        //

        // First, shift the bits that will stay in their current spot
        //  1111111111111100_2222222222222200_3333333333333300_4444444444444400_5555555555555500_6666666666666600_7777777777777700_8888888888888800
        let shifted_in_place = self >> count;
        // Next, shift the bits that will carry WAAAAY over to the right, which is where they will end up in the next word.
        //  0000000000000002_0000000000000003_0000000000000004_0000000000000005_0000000000000006_0000000000000007_8000000000000008_0000000000000001
        let  carried = self << (64 - count);
        // Grab the overflow and replace it with the new carry
        let overflow = carried.extract(0);
        let carried = carried.replace(0, prev_overflow);
        // (we're actually moving the words to the left here, because >> means "make
        // each bit less significant," and little-endian has the least significant stuff on the left.)
        let carried = shuffle!(carried, [1,2,3,4,5,6,7,0]);
        (shifted_in_place | carried, overflow)
    }
    fn streaming_add(self, other: Self, overflow: bool) -> (Self, bool) {
        // Instructions: 3 (+ > select)
        let half_add = self + other;
        // Collect bits that actually carried, and put them into an 8 byte mask, with 1 in each byte that carried.
        let carries = self.gt(half_add).bitmask() | overflow as u8;

        // Carry the 1s
        // NOTE: when 64 1's is uncommon, this is a lot of extra instructions. When it's not, it's
        // better than looping.
        let (carry_add, overflowed) = {
            // Collect the "carrythrough mask"--i.e. any lane with FFFFFFFF_FFFFFFFF becomes FF, and anything else becomes 00.
            let carrythroughs = half_add.eq(Self::ALL_BITS).bitmask();
            // Add the carry bits to the FF's, which makes them travel *through*, landing on the first non-FF.
            let (carried, carried_overflow) = carrythroughs.overflowing_add(carries << 1);
            // Any bits that *change* need to be carried (either FF's that get flipped due to
            // carrythrough, or non-FF's that have a carry bit on them), thus we xor with the original.
            let carried = carried ^ carrythroughs;
            // Turn the bitmask into a series of 1's
            // TODO better way to turn a bitmask back into a series of 1's?
            let carry_add = (u64x8::splat(carried.into()) >> u64x8::new(0,1,2,3,4,5,6,7)) & u64x8::splat(1);
            (carry_add, carries & 0x80 > 0 || carried_overflow)
        };

        // Add the carries to the result (lane-wise) and return
        (half_add + carry_add, overflowed)
    }
    fn streaming_clmul(self, other: Self::Overflow, overflow: Self::Overflow) -> (Self, Self::Overflow) {
        let mut result = u64x8::splat(0);
        let mut overflow = overflow;
        for i in 0..8 {
            let cmul = result.extract(i).streaming_clmul(other, overflow);
            result = result.replace(i, cmul.0);
            overflow = cmul.1;
        }
        (result, overflow)
    }
    fn from_overflow(overflow: Self::Overflow) -> Self {
        Self::NO_BITS.replace(64-1, overflow)
    }
    fn from_bool_overflow(overflow: bool) -> Self {
        Self::from_overflow(Self::Overflow::from_bool_overflow(overflow))
    }
    fn all(self) -> bool {
        self == Self::ALL_BITS
    }
    fn any(self) -> bool {
        self != Self::NO_BITS
    }
    fn get_bit(self, at: u32) -> bool {
        self.extract(at as usize / 64).get_bit(at % 64)
    }
}

impl StreamableBitmask for u64 {
    const NUM_BITS: u32 = 64;
    const NO_BITS: Self = 0;
    const ALL_BITS: Self = !Self::NO_BITS;
    const EVEN_BITS: Self = Self::ODD_BITS << 1;
    const ODD_BITS: Self = Self::ALL_BITS / 3; // odd + even = all. even = odd * 2. Therefore, odd * 3 = all, and all/3 = odd.
    const TOP_BIT: Self = 1 << (Self::BIT_WIDTH - 1);

    type Overflow = Self;

    fn streaming_shift_forward(self, count: u32, overflow: Self::Overflow, overflow_count: u32) -> (Self, Self::Overflow) {
        assert!(overflow_count <= Self::Overflow::BIT_WIDTH);
        assert!(count <= overflow_count);
        let overflow = overflow >> (overflow_count-count);

        println!("overflow     :  {:064b}", overflow.reverse_bits());
        println!("self         :  {:064b}", self.reverse_bits());
        println!("out          : {:064b}", ((self << count) | overflow).reverse_bits());
        println!("overflow     : {:64}{:064b}", "", (self >> (64-count)).reverse_bits());
        ((self << count) | overflow, self >> (64-count))
    }
    fn streaming_add(self, other: Self, overflow: bool) -> (Self, bool) {
        self.overflowing_add(other + Self::from(overflow))
    }
    fn streaming_clmul(self, other: Self::Overflow, overflow: Self::Overflow) -> (Self, Self::Overflow) {
        let result = self.clmul(other) ^ overflow;
        (result.extract(0), result.extract(1))
    }
    fn from_overflow(overflow: Self::Overflow) -> Self {
        overflow
    }
    fn from_bool_overflow(overflow: bool) -> Self {
        overflow.into()
    }
    fn all(self) -> bool {
        self == Self::ALL_BITS
    }
    fn any(self) -> bool {
        self != Self::NO_BITS
    }
    fn get_bit(self, at: u32) -> bool {
        (self & (1 << at)) > 0
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::maskable::*;

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
