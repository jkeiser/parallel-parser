use crate::int_traits::*;
use crate::simd_traits::*;
use crate::carryless_mul::*;
use packed_simd::*;
use std::ops::{BitAnd,BitOr,BitXor,Not,BitAndAssign,BitOrAssign,BitXorAssign};
use std::fmt::Debug;

pub trait StreamableBitmask: Sized
    +Copy+Clone+Debug
    +Not<Output=Self>
    +BitAnd<Self,Output=Self>+BitOr<Self,Output=Self>+BitXor<Self,Output=Self>
    +BitAndAssign<Self>+BitOrAssign<Self>+BitXorAssign<Self> {
    const ALL_BITS: Self;
    const NO_BITS: Self;
    const EVEN_BITS: Self;
    const ODD_BITS: Self;
    const TOP_BIT: Self;

    type Overflow: PrimitiveInteger;

    fn streaming_shift_forward(self, count: u32, prev_overflow: Self::Overflow) -> (Self, Self::Overflow);
    fn streaming_add(self, other: Self, prev_overflow: bool) -> (Self, bool);
    fn streaming_clmul(self, other: Self::Overflow, prev_overflow: Self::Overflow) -> (Self, Self::Overflow);
    fn from_overflow(prev_overflow: Self::Overflow) -> Self;
    fn from_bool_overflow(prev_overflow: bool) -> Self;

    ///
    /// The *previous* value (shifted over 1).
    /// 
    /// Zeroes are shifted in at the very beginning.
    /// 
    /// e.g. prev(01111010) = 00111101...
    /// 
    fn prev(self, prev_overflow: Self::Overflow) -> (Self, Self::Overflow) {
        self.streaming_shift_forward(1, prev_overflow)
    }

    ///
    /// The value multiple places back (shifted over by count).
    /// 
    /// Zeroes are shifted in at the very beginning.
    /// 
    /// e.g. back(01111010, 3) = 00001111...
    ///
    fn back(self, count: u32, prev_overflow: Self::Overflow) -> (Self, Self::Overflow) {
        self.streaming_shift_forward(count, prev_overflow)
    }

    ///
    /// The starts of any series of 1's: flips on the bit *at* the beginning of a run of 1's.
    ///
    /// e.g. starts(01111010) = 01000010
    /// 
    fn starts(self, prev_overflow: Self::Overflow) -> (Self, Self::Overflow) {
        let (prev, overflow) = self.streaming_shift_forward(1, prev_overflow);
        (self & !prev, overflow)
    }

    ///
    /// The ends of any series of 1's: flips on the bit *after* any run of 1's.
    ///
    /// e.g. after_series(01111010) = 00000101
    /// 
    fn after_series(self, prev_overflow: Self::Overflow) -> (Self, Self::Overflow) {
        let (prev, overflow) = self.streaming_shift_forward(1, prev_overflow);
        (!self & prev, overflow)
    }
    ///
    /// The ends of any *odd-length* series of 1's.
    /// 
    /// Flips on the bit *after* such series.
    /// 
    /// e.g. after_odd_series(01111010) = 00000001
    /// 
    fn after_odd_series(self, prev_overflow: (Self::Overflow, bool, bool)) -> (Self, (Self::Overflow, bool, bool)) {
        // NOTE: the overflow could be done more efficiently with an Option<bool> where None indicates no overflow, false indicates even overflow and true indicates odd overflow.
        // It could be done even MORE efficiently with a single bool ("was there odd overflow") as long as we're willing to sometimes mark the middle of the series as "the end of an odd run." (Could mask it back off, too.)
        let (starts, start_overflow) = self.starts(prev_overflow.0);
        let (even_ends, even_overflow) = (starts & Self::EVEN_BITS).streaming_add(self, prev_overflow.1);
        let (odd_ends, odd_overflow) = (starts & Self::ODD_BITS).streaming_add(self, prev_overflow.2);
        let after_odd_series = (even_ends & Self::ODD_BITS) | (odd_ends & Self::EVEN_BITS);
        (after_odd_series, (start_overflow, even_overflow, odd_overflow))
    }

    ///
    /// Fills in 1's between every pair of 1's.
    /// 
    /// Includes the *starting* 1 of each pair, but does not include the *trailing* 1.
    /// 
    /// e.g. between_pairs(0100100110) = (0111000100)
    /// 
    fn between_pairs(self, prev_overflow: Self::Overflow) -> (Self, Self::Overflow) {
        let zero: Self::Overflow = false.into();
        self.streaming_clmul(!zero, prev_overflow)
    }
}

impl StreamableBitmask for u64x8 {
    const NO_BITS: Self = Self::splat(<<Self as SimdBase>::LaneType>::NO_BITS);
    const ALL_BITS: Self = Self::splat(<<Self as SimdBase>::LaneType>::ALL_BITS);
    const EVEN_BITS: Self = Self::splat(<<Self as SimdBase>::LaneType>::EVEN_BITS);
    const ODD_BITS: Self = Self::splat(<<Self as SimdBase>::LaneType>::ODD_BITS);
    const TOP_BIT: Self = u64x8::new(u64::TOP_BIT, 0, 0, 0, 0, 0, 0, 0);
    type Overflow = <Self as SimdBase>::LaneType;

    fn streaming_shift_forward(self, count: u32, prev_overflow: Self::Overflow) -> (Self, Self::Overflow) {
        assert!(count < Self::BIT_WIDTH);

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
        let carried = self << (64 - count);
        // Move the carried words to the right, and keep the last one.
        let overflow = carried.extract(0);
        let overflow_zeroed = carried.replace(0, 0);
        // (we're actually moving the words to the left here, because >> means "make
        // each bit less significant," and little-endian has the least significant stuff on the left.)
        let carried = shuffle!(overflow_zeroed, [1,2,3,4,5,6,7,0]);
        (prev_overflow | shifted_in_place | carried, overflow)
    }
    fn streaming_add(self, other: Self, prev_overflow: bool) -> (Self, bool) {
        // Instructions: 3 (+ > select)
        let half_add = self + other;
        // Collect bits that actually carried, and put them into an 8 byte mask, with 1 in each byte that carried.
        let carries = self.gt(half_add).bitmask() | prev_overflow as u8;

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
    fn streaming_clmul(self, other: Self::Overflow, prev_overflow: Self::Overflow) -> (Self, Self::Overflow) {
        let mut result = u64x8::splat(0);
        let mut overflow = prev_overflow;
        for i in 0..8 {
            let cmul = result.extract(i).streaming_clmul(other, overflow);
            result = result.replace(i, cmul.0);
            overflow = cmul.1;
        }
        (result, overflow)
    }
    fn from_overflow(prev_overflow: Self::Overflow) -> Self {
        Self::NO_BITS.replace(Self::BYTE_WIDTH-1, prev_overflow)
    }
    fn from_bool_overflow(prev_overflow: bool) -> Self {
        Self::from_overflow(Self::Overflow::from_bool_overflow(prev_overflow))
    }
}

impl StreamableBitmask for u64 {
    const NO_BITS: Self = 0;
    const ALL_BITS: Self = !Self::NO_BITS;
    const EVEN_BITS: Self = Self::ODD_BITS << 1;
    const ODD_BITS: Self = Self::ALL_BITS / 3; // odd + even = all. even = odd * 2. Therefore, odd * 3 = all, and all/3 = odd.
    const TOP_BIT: Self = 1 << (Self::BIT_WIDTH - 1);

    type Overflow = Self;

    fn streaming_shift_forward(mut self, count: u32, prev_overflow: Self::Overflow) -> (Self, Self::Overflow) {
        assert!(count <= Self::Overflow::BIT_WIDTH);
        self |= prev_overflow;
        (self >> count, self << count)
    }
    fn streaming_add(self, other: Self, prev_overflow: bool) -> (Self, bool) {
        self.overflowing_add(other + Self::from(prev_overflow))
    }
    fn streaming_clmul(self, other: Self::Overflow, prev_overflow: Self::Overflow) -> (Self, Self::Overflow) {
        let result = self.clmul(other) ^ prev_overflow;
        (result.extract(0), result.extract(1))
    }
    fn from_overflow(prev_overflow: Self::Overflow) -> Self {
        prev_overflow
    }
    fn from_bool_overflow(prev_overflow: bool) -> Self {
        prev_overflow.into()
    }
}
