use crate::carryless_mul::*;
use packed_simd::*;
use std::ops::{BitAnd,BitOr,BitXor,Not,BitAndAssign,BitOrAssign,BitXorAssign};
use std::fmt::Debug;

pub trait StreamableBitmask: Sized
    +Copy+Clone+Debug
    +Not<Output=Self>
    +BitAnd<Self,Output=Self>+BitOr<Self,Output=Self>+BitXor<Self,Output=Self>
    +BitAndAssign<Self>+BitOrAssign<Self>+BitXorAssign<Self>
    +PartialEq<Self> {
    const NUM_BITS: u32;
    const ALL_BITS: Self;
    const NO_BITS: Self;
    const EVEN_BITS: Self;
    const ODD_BITS: Self;
    const TOP_BIT: Self;

    type Overflow: StreamableBitmask+From<bool>;

    fn streaming_shift_forward(self, count: u32, overflow: &mut Self::Overflow, overflow_count: u32) -> Self;
    fn streaming_add(self, other: Self, overflow: &mut bool) -> Self;
    fn streaming_clmul(self, other: Self::Overflow, overflow: &mut Self::Overflow) -> Self;
    fn from_overflow(overflow: Self::Overflow) -> Self;
    fn from_bool_overflow(overflow: bool) -> Self;

    ///
    /// The *previous* value (shifted over 1).
    /// 
    /// Zeroes are shifted in at the very beginning.
    /// 
    /// e.g. prev(01111010) = 00111101...
    /// 
    fn prev(self, overflow: &mut Self::Overflow) -> Self {
        self.streaming_shift_forward(1, overflow, 1)
    }

    ///
    /// The *previous* value (shifted over 1).
    /// 
    /// Zeroes are shifted in at the very beginning.
    /// 
    /// e.g. prev(01111010) = 00111101...
    /// 
    fn back(self, count: u32, overflow: &mut Self::Overflow) -> Self {
        self.streaming_shift_forward(count, overflow, count)
    }

    ///
    /// The values 1 and 2 places back.
    /// 
    /// Zeroes are shifted in at the very beginning.
    /// 
    /// e.g. back(01111010, 3) = 00001111...
    ///
    fn prev_2(self, overflow: &mut Self::Overflow) -> (Self, Self) {
        let mut ignore_overflow = *overflow;
        let prev1 = self.streaming_shift_forward(1, &mut ignore_overflow, 2);
        let prev2 = self.streaming_shift_forward(2, overflow, 2);
        (prev1, prev2)
    }

    ///
    /// The values 1, 2, and 3 places back.
    /// 
    /// Zeroes are shifted in at the very beginning.
    /// 
    /// e.g. back(01111010, 3) = 00001111...
    ///
    fn prev_3(self, overflow: &mut Self::Overflow) -> (Self, Self, Self) {
        let mut ignore_overflow = *overflow;
        let prev1 = self.streaming_shift_forward(1, &mut ignore_overflow, 3);
        let mut ignore_overflow = *overflow;
        let prev2 = self.streaming_shift_forward(2, &mut ignore_overflow, 3);
        let prev3 = self.streaming_shift_forward(3, overflow, 3);
        (prev1, prev2, prev3)
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
    /// The is_series_start of any series of 1's: flips on the bit *at* the beginning of a run of 1's.
    ///
    /// e.g. is_series_start(01111010) = 01000010
    /// 
    fn is_series_start(self, overflow: &mut Self::Overflow) -> Self {
        let prev = self.prev(overflow);
        self & !prev
    }

    ///
    /// The ends of any series of 1's: flips on the bit *after* any run of 1's.
    ///
    /// e.g. after_series_end(01111010) = 00000101
    /// 
    fn after_series_end(self, overflow: &mut Self::Overflow) -> Self {
        let prev = self.prev(overflow);
        !self & prev
    }

    ///
    /// Any character preceded by (primary)(primary|self)*.
    /// 
    /// ## Examples
    /// 
    /// Find identifiers from alpha and num:
    /// 
    ///     let alpha = bytes.where_within(b'A'..=b'Z') | bytes.where_within(b'a'..=b'z') | bytes.where_eq(b'_');
    ///     let digit = bytes.where_within(b'0'..=b'9');
    ///     digit.after_series_starting_with(alpha, overflow)
    ///
    fn after_series_starting_with(self, primary: Self, overflow: &mut bool) -> Self {
        let added = primary.streaming_add(self | primary, overflow);
        added ^ self
    }

    ///
    /// The ends of any *odd-length* series of 1's.
    /// 
    /// Flips on the bit *after* such series.
    /// 
    /// e.g. after_odd_series(01111010) = 00000001
    /// 
    ///             01111010
    /// even_starts 00000010
    ///           + 01111010
    /// even_ends   01111001
    /// & odd_bits  01010001
    /// odd_starts  01000000
    ///           + 01111010
    /// odd_ends    00000110
    /// & even_bits 00000010
    ///           | 01010011
    ///         & ! 01111010
    ///           = 00000001
    /// 
    /// ## Examples
    /// 
    /// Find non-backslashes that are after an *odd* sequence of backslashes:
    /// 
    ///     let backslash = bytes.where_eq(rb'\');
    ///     backslash.after_odd_series(overflow)
    /// 
    fn after_odd_series_end(self, overflow: &mut AfterOddSeriesOverflow<Self::Overflow>) -> Self {
        // NOTE: the overflow could be done more efficiently with an Option<bool> where None indicates no overflow, false indicates even overflow and true indicates odd overflow.
        // It could be done even MORE efficiently with a single bool ("was there odd overflow") as long as we're willing to sometimes mark the middle of the series as "the end of an odd run." (Could mask it back off, too.)
        let is_series_start = self.is_series_start(&mut overflow.start_overflow);
        let even_ends = (is_series_start & Self::EVEN_BITS).streaming_add(self, &mut overflow.even_overflow);
        let odd_ends = (is_series_start & Self::ODD_BITS).streaming_add(self, &mut overflow.odd_overflow);
        let after_odd_series = (even_ends & Self::ODD_BITS) | (odd_ends & Self::EVEN_BITS);
        after_odd_series & !self
    }

    ///
    /// Add an extra bit to any odd-length sequence, to even it out.
    /// 
    /// Flips on the bit *after* odd-length series.
    /// 
    /// e.g. after_odd_series(01111010) = 01111011
    /// 
    ///             01111010
    /// even_starts 00000010
    ///           + 01111010
    /// even_ends   01111001
    /// & odd_bits  01010001
    /// odd_starts  01000000
    ///           + 01111010
    /// odd_ends    00000110
    /// & even_bits 00000010
    ///           | 01010011
    ///           | 01111010
    ///           = 01111011
    /// 
    /// ## Examples
    /// 
    /// Find non-backslashes that are after an *odd* sequence of backslashes:
    /// 
    ///     let backslash = bytes.where_eq(rb'\');
    ///     backslash.after_odd_series(overflow)
    /// 
    fn complete_even_series(self, overflow: &mut AfterOddSeriesOverflow<Self::Overflow>) -> Self {
        // NOTE: the overflow could be done more efficiently with an Option<bool> where None indicates no overflow, false indicates even overflow and true indicates odd overflow.
        // It could be done even MORE efficiently with a single bool ("was there odd overflow") as long as we're willing to sometimes mark the middle of the series as "the end of an odd run." (Could mask it back off, too.)
        let is_series_start = self.is_series_start(&mut overflow.start_overflow);
        let even_ends = (is_series_start & Self::EVEN_BITS).streaming_add(self, &mut overflow.even_overflow);
        let odd_ends = (is_series_start & Self::ODD_BITS).streaming_add(self, &mut overflow.odd_overflow);
        let after_odd_series = (even_ends & Self::ODD_BITS) | (odd_ends & Self::EVEN_BITS);
        after_odd_series
    }

    ///
    /// Fills in 1's between every pair of 1's.
    /// 
    /// Includes the *starting* 1 of each pair, but does not include the *trailing* 1.
    /// 
    /// e.g. between_pairs(0100100110) = (0111000100)
    /// 
    fn between_pairs(self, overflow: &mut Self::Overflow) -> Self {
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

#[derive(Copy,Clone,Debug,Default)]
pub struct AfterOddSeriesOverflow<I> {
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

    fn streaming_shift_forward(self, count: u32, overflow: &mut Self::Overflow, overflow_count: u32) -> Self {
        assert!(count < u64x8::lanes() as u32*64);
        assert!(overflow_count <= 64);
        assert!(count <= overflow_count);
        let prev_overflow = *overflow >> (overflow_count-count);

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
        *overflow = carried.extract(0);
        let carried = carried.replace(0, prev_overflow);
        // (we're actually moving the words to the left here, because >> means "make
        // each bit less significant," and little-endian has the least significant stuff on the left.)
        let carried = shuffle!(carried, [1,2,3,4,5,6,7,0]);
        shifted_in_place | carried
    }
    fn streaming_add(self, other: Self, overflow: &mut bool) -> Self {
        // Instructions: 3 (+ > select)
        let half_add = self + other;
        // Collect bits that actually carried, and put them into an 8 byte mask, with 1 in each byte that carried.
        let carries = self.gt(half_add).bitmask() | *overflow as u8;

        // Carry the 1s
        // NOTE: when 64 1's is uncommon, this is a lot of extra instructions. When it's not, it's
        // better than looping.
        let carry_add = {
            // Collect the "carrythrough mask"--i.e. any lane with FFFFFFFF_FFFFFFFF becomes FF, and anything else becomes 00.
            let carrythroughs = half_add.eq(Self::ALL_BITS).bitmask();
            // Add the carry bits to the FF's, which makes them travel *through*, landing on the first non-FF.
            let (carried, carried_overflow) = carrythroughs.overflowing_add(carries << 1);
            *overflow = carries & 0x80 > 0 || carried_overflow;
            // Any bits that *change* need to be carried (either FF's that get flipped due to
            // carrythrough, or non-FF's that have a carry bit on them), thus we xor with the original.
            let carried = carried ^ carrythroughs;
            // Turn the bitmask into a series of 1's
            // TODO better way to turn a bitmask back into a series of 1's?
            (u64x8::splat(carried.into()) >> u64x8::new(0,1,2,3,4,5,6,7)) & u64x8::splat(1)
        };

        // Add the carries to the result (lane-wise) and return
        half_add + carry_add
    }
    fn streaming_clmul(self, other: Self::Overflow, overflow: &mut Self::Overflow) -> Self {
        let mut result = u64x8::splat(0);
        for i in 0..8 {
            let cmul = result.extract(i).streaming_clmul(other, overflow);
            result = result.replace(i, cmul);
        }
        result
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
    const TOP_BIT: Self = 1 << (64 - 1);

    type Overflow = Self;

    fn streaming_shift_forward(self, count: u32, overflow: &mut Self::Overflow, overflow_count: u32) -> Self {
        assert!(overflow_count <= 64);
        assert!(count <= overflow_count);
        let prev_overflow = *overflow >> (overflow_count-count);
        *overflow = self >> (64-count);
        (self << count) | prev_overflow
    }
    fn streaming_add(self, other: Self, overflow: &mut bool) -> Self {
        let (result, new_overflow) = self.overflowing_add(other + Self::from(*overflow));
        *overflow = new_overflow;
        result
    }
    fn streaming_clmul(self, other: Self::Overflow, overflow: &mut Self::Overflow) -> Self {
        let result = self.clmul(other) ^ *overflow;
        *overflow = result.extract(1);
        result.extract(0)
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

    fn test_prev(input: &[u8]) {
        let expected = expected_prev(input, 1);
        chunks64(input).zip(chunks64(&expected)).scan(Default::default(), |overflow, (chunk,expected)| {
            let expected_mask = expected.where_eq(b'X');
            let chunk_mask = chunk.where_eq(b'X');
            let actual_mask = chunk_mask.prev(overflow);
            assert_eq!(actual_mask, expected_mask, "\n{:>8}: {}\n{:>8}: {}", "actual", str_from_mask(actual_mask), "expected", str_from_mask(expected_mask));
            Some(actual_mask)
        }).for_each(|_|());
    }

    fn test_back(input: &[u8], count: usize) {
        let expected = expected_prev(input, count);
        chunks64(input).zip(chunks64(&expected)).scan(Default::default(), |overflow, (chunk, expected)| {
            let expected_mask = expected.where_eq(b'X');
            let chunk_mask = chunk.where_eq(b'X');
            let actual_mask = chunk_mask.back(count as u32, overflow);
            assert_eq!(actual_mask, expected_mask, "\n{:>8}: {}\n{:>8}: {}", "actual", str_from_mask(actual_mask), "expected", str_from_mask(expected_mask));
            Some(actual_mask)
        }).for_each(|_|());
    }

    fn test_prev_3(input: &[u8]) {
        println!("-----------------------------");
        println!("{:>9}: {}", "input", str::from_utf8(input).unwrap());
        let expected = expected_prev(input, 1);
        let expected2 = expected_prev(input, 2);
        let expected3 = expected_prev(input, 3);
        use std::str;
        use std::mem::transmute;
        println!("{:>9}: {}", "expected", str::from_utf8(&expected).unwrap());
        println!("{:>9}: {}", "expected2", str::from_utf8(&expected2).unwrap());
        println!("{:>9}: {}", "expected3", str::from_utf8(&expected3).unwrap());

        chunks64(input).zip(chunks64(&expected)).zip(chunks64(&expected2)).zip(chunks64(&expected3)).scan(Default::default(), |overflow, (((chunk, expected), expected2), expected3)| {
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
            let (actual_mask, actual2_mask, actual3_mask) = chunk_mask.prev_3(overflow);
            println!("{:>9}: {}", "actual", str_from_mask(actual_mask));
            println!("{:>9}: {}", "actual2", str_from_mask(actual2_mask));
            println!("{:>9}: {}", "actual3", str_from_mask(actual3_mask));
            println!("{:>9}: {}", "expected", str_from_mask(expected_mask));
            println!("{:>9}: {}", "expected2", str_from_mask(expected2_mask));
            println!("{:>9}: {}", "expected3", str_from_mask(expected3_mask));
            assert_eq!(actual_mask, expected_mask, "\n{:>9}: {}\n{:>9}: {}", "actual", str_from_mask(actual_mask), "expected", str_from_mask(expected_mask));
            assert_eq!(actual2_mask, expected2_mask, "\n{:>9}: {}\n{:>9}: {}", "actual2", str_from_mask(actual2_mask), "expected2", str_from_mask(expected2_mask));
            assert_eq!(actual3_mask, expected3_mask, "\n{:>9}: {}\n{:>9}: {}", "actual3", str_from_mask(actual3_mask), "expected3", str_from_mask(expected3_mask));
            Some(actual_mask)
        }).for_each(|_|());
    }

    #[test]
    fn any() {
        let input = chunks64("                                                                                                                                ".as_bytes());
        assert_eq!(input.map(|input| input.where_eq(b'X').any()).collect::<Vec<bool>>(), vec![false,false]);
        let input = chunks64("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX".as_bytes());
        assert_eq!(input.map(|input| input.where_eq(b'X').any()).collect::<Vec<bool>>(), vec![true,true]);
        let input = chunks64("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                                                                ".as_bytes());
        assert_eq!(input.map(|input| input.where_eq(b'X').any()).collect::<Vec<bool>>(), vec![true,false]);
        let input = chunks64("                                                                XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX".as_bytes());
        assert_eq!(input.map(|input| input.where_eq(b'X').any()).collect::<Vec<bool>>(), vec![false,true]);
        let input = chunks64("X                                                                   X                                                           ".as_bytes());
        assert_eq!(input.map(|input| input.where_eq(b'X').any()).collect::<Vec<bool>>(), vec![true,true]);
        let input = chunks64("X                                                                                                                               ".as_bytes());
        assert_eq!(input.map(|input| input.where_eq(b'X').any()).collect::<Vec<bool>>(), vec![true,false]);
        let input = chunks64("                                                                    X                                                           ".as_bytes());
        assert_eq!(input.map(|input| input.where_eq(b'X').any()).collect::<Vec<bool>>(), vec![false,true]);
    }

    #[test]
    fn all() {
        let input = chunks64("                                                                                                                                ".as_bytes());
        assert_eq!(input.map(|input| input.where_eq(b'X').all()).collect::<Vec<bool>>(), vec![false,false]);
        let input = chunks64("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX".as_bytes());
        assert_eq!(input.map(|input| input.where_eq(b'X').all()).collect::<Vec<bool>>(), vec![true,true]);
        let input = chunks64("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                                                                ".as_bytes());
        assert_eq!(input.map(|input| input.where_eq(b'X').all()).collect::<Vec<bool>>(), vec![true,false]);
        let input = chunks64("                                                                XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX".as_bytes());
        assert_eq!(input.map(|input| input.where_eq(b'X').all()).collect::<Vec<bool>>(), vec![false,true]);
        let input = chunks64("X                                                                   X                                                           ".as_bytes());
        assert_eq!(input.map(|input| input.where_eq(b'X').all()).collect::<Vec<bool>>(), vec![false,false]);
        let input = chunks64("X                                                                                                                               ".as_bytes());
        assert_eq!(input.map(|input| input.where_eq(b'X').all()).collect::<Vec<bool>>(), vec![false,false]);
        let input = chunks64("                                                                    X                                                           ".as_bytes());
        assert_eq!(input.map(|input| input.where_eq(b'X').all()).collect::<Vec<bool>>(), vec![false,false]);
    }

    #[test]
    fn prev() {
        test_prev(b"     X   X X    X X X  X X X X X    X X  X X    X X   X   X X X   X    X  X  X    X    X     X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X ");
        test_prev(b"     X   X X    X X X  X X X X X    X X  X X    X X   X   X X X   X    X  X  X    X    X    X X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X X");
        test_prev(b" X    X   X X    X X X  X X X X X    X X  X X    X X   X   X X X   X    X  X  X    X    X     X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X X");
        test_prev(b" X    X   X X    X X X  X X X X X    X X  X X    X X   X   X X X   X    X  X  X    X    X    X X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X X");
    }

    #[test]
    fn back() {
        test_back(b" X X     X   X X    X X X  X X X X X    X X  X X    X X   X   X X X   X    X  X  X    X    X   X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X ", 2);
        test_back(b"   X    X   X X    X X X  X X X X X    X X  X X    X X   X   X X X   X    X  X  X    X    X    X X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X X", 2);
        test_back(b" X X X    X   X X    X X X  X X X X X    X X  X X    X X   X   X X X   X    X  X  X    X    X  X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X X", 2);
        test_back(b"       X   X X    X X X  X X X X X    X X  X X    X X   X   X X X   X    X  X  X    X    X    X_ X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X X", 2);
        test_back(b" X X     X   X X    X X X  X X X X X    X X  X X    X X   X   X X X   X    X  X  X    X    X   X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X ", 3);
        test_back(b"   X    X   X X    X X X  X X X X X    X X  X X    X X   X   X X X   X    X  X  X    X    X    X X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X X", 3);
        test_back(b" X X X    X   X X    X X X  X X X X X    X X  X X    X X   X   X X X   X    X  X  X    X    X  X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X X", 3);
        test_back(b"       X   X X    X X X  X X X X X    X X  X X    X X   X   X X X   X    X  X  X    X    X    X_ X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X X", 3);
    }
    #[test]
    fn prev_3() {
        test_prev_3(b" X X     X   X X    X X X  X X X X X    X X  X X    X X   X   X X X   X    X  X  X    X    X   X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X ");
        test_prev_3(b"   X    X   X X    X X X  X X X X X    X X  X X    X X   X   X X X   X    X  X  X    X    X    X X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X X");
        test_prev_3(b" X X X    X   X X    X X X  X X X X X    X X  X X    X X   X   X X X   X    X  X  X    X    X  X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X X");
        test_prev_3(b"       X   X X    X X X  X X X X X    X X  X X    X X   X   X X X   X    X  X  X    X    X    X_ X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X  X X X X");
    }
    #[test]
    fn is_series_start() {
        let input    = b"XXXX   X X  XXX X XXXXX XXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX XXXXXXXXXX          XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX XXXXX X";
        let expected = b"X      X X  X   X X     X         X                                                                                         X                   X                                                                                                                                                                                                                              X     X";
        let actual: Vec<u64> = chunks64(input).scan(Default::default(), |overflow, input| Some(input.where_eq(b'X').is_series_start(overflow))).collect();
        let expected: Vec<u64> = chunks64(expected).map(|input| input.where_eq(b'X')).collect();
        assert_eq!(actual, expected);
        let input    = b"                                                                XXXX";
        let expected = b"                                                                X   ";
        let actual: Vec<u64> = chunks64(input).scan(Default::default(), |overflow, input| Some(input.where_eq(b'X').is_series_start(overflow))).collect();
        let expected: Vec<u64> = chunks64(expected).map(|input| input.where_eq(b'X')).collect();
        assert_eq!(actual, expected);
    }
    #[test]
    fn after_series_end() {
        let input    = b"X ";
        let expected = b" X";
        let actual: Vec<u64> = chunks64(input).scan(Default::default(), |overflow, input| Some(input.where_eq(b'X').after_series_end(overflow))).collect();
        let expected: Vec<u64> = chunks64(expected).map(|input| input.where_eq(b'X')).collect();
        assert_eq!(actual, expected);
        let input    = b"XXXX   X X  XXX X XXXXX XXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX XXXXXXXXXX          XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX XXXXX X";
        let expected = b"    X   X X    X X     X         X                                                                                         X          X                                                                                                                                                                                                                                       X     X X";
        let actual: Vec<u64> = chunks64(input).scan(Default::default(), |overflow, input| Some(input.where_eq(b'X').after_series_end(overflow))).collect();
        let expected: Vec<u64> = chunks64(expected).map(|input| input.where_eq(b'X')).collect();
        assert_eq!(actual, expected);
        let input    = b"                                                                XXXX";
        let expected = b"                                                                    X";
        let actual: Vec<u64> = chunks64(input).scan(Default::default(), |overflow, input| Some(input.where_eq(b'X').after_series_end(overflow))).collect();
        let expected: Vec<u64> = chunks64(expected).map(|input| input.where_eq(b'X')).collect();
        assert_eq!(actual, expected);
    }
    #[test]
    fn after_odd_series_end() {
        let input    = b"X ";
        let expected = b" X";
        let actual: Vec<u64> = chunks64(input).scan(Default::default(), |overflow, input| Some(input.where_eq(b'X').after_odd_series_end(overflow))).collect();
        let expected: Vec<u64> = chunks64(expected).map(|input| input.where_eq(b'X')).collect();
        assert_eq!(actual, expected);
        // let input    = b"XX ";
        // let expected = b"   ";
        // let actual: Vec<u64> = chunks64(input).scan(Default::default(), |overflow, input| Some(input.where_eq(b'X').after_odd_series_end(overflow))).collect();
        // let expected: Vec<u64> = chunks64(expected).map(|input| input.where_eq(b'X')).collect();
        // assert_eq!(actual, expected);
        // let input    = b"XXXX   X X  XXX X XXXXX XXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX XXXXXXXXXX          XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX XXXXX X";
        // let expected = b"        X X    X X     X                                                                                                   X          X                                                                                                                                                                                                                                             X X";
        // let actual: Vec<u64> = chunks64(input).scan(Default::default(), |overflow, input| Some(input.where_eq(b'X').after_odd_series_end(overflow))).collect();
        // let expected: Vec<u64> = chunks64(expected).map(|input| input.where_eq(b'X')).collect();
        // assert_eq!(actual, expected);
        // let input    = b"                                                                XXXX";
        // let expected = b"                                                                    ";
        // let actual: Vec<u64> = chunks64(input).scan(Default::default(), |overflow, input| Some(input.where_eq(b'X').after_odd_series_end(overflow))).collect();
        // let expected: Vec<u64> = chunks64(expected).map(|input| input.where_eq(b'X')).collect();
        // assert_eq!(actual, expected);
    }
}
