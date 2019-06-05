#![feature(reverse_bits)]

use std::ops::BitXor;
#[cfg(target_arch = "x86")]
use std::arch::x86::*;
#[cfg(target_arch = "x86_64")]
use std::arch::x86_64::*;

use packed_simd::*;
use std::mem;

trait Bitmask: Sized {
    const SIZE: usize;
    const ALL_BITS: Self;
    const NO_BITS: Self;
    const EVEN_BITS: Self;
    const ODD_BITS: Self;
}

trait CarrylessMul: Sized {
    type ExpandedOutput;
    fn clmul(self, other: Self) -> Self;
    fn clmul_expand(self, other: Self) -> Self::ExpandedOutput;
}

impl CarrylessMul for u64 {
    type ExpandedOutput = u64x2;
    fn clmul(self, other: Self) -> Self {
        self.clmul_expand(other).extract(0)
    }
    fn clmul_expand(self, other: Self) -> Self::ExpandedOutput {
        let a: __m128i = unsafe { mem::transmute(u64x2::new(self, 0)) };
        let b: __m128i = unsafe { mem::transmute(u64x2::new(other, 0)) };
        let result: u64x2 = unsafe { mem::transmute(_mm_clmulepi64_si128(a, b, 0)) };
        result
    }
}

impl Bitmask for u64 {
    const SIZE: usize = 64;
    const ALL_BITS: Self  = 0b11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111;
    const NO_BITS: Self   = 0b00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000;
    const EVEN_BITS: Self = 0b01010101_01010101_01010101_01010101_01010101_01010101_01010101_01010101;
    const ODD_BITS: Self  = 0b10101010_10101010_10101010_10101010_10101010_10101010_10101010_10101010;
}

// Parses exactly 64 bits, with possible information about the prior 64 bits.
pub mod parser64 {
    use packed_simd::*;
    use super::*;

    const BACKSLASHES: u8x64 = u8x64::splat(b'\\');
    const QUOTES: u8x64 = u8x64::splat(b'"');

    pub struct BlockData {
        pub escape_mask: u64,
        pub string_mask: u64,
        pub first_character_is_escaped: bool,
    }
    pub struct NextData {
        pub still_in_string: u64,
        pub next_character_is_escaped: bool,
    }

    pub fn parse(input: u8x64, next_data: NextData) -> (BlockData, NextData) {
        let (escape_mask, first_character_is_escaped, next_character_is_escaped) = find_backslashes(input, &next_data);
        let (string_mask, still_in_string) = find_strings(input, escape_mask, &next_data);
        (BlockData { escape_mask, string_mask, first_character_is_escaped }, NextData { next_character_is_escaped, still_in_string })
    }

    fn find_backslashes(input: u8x64, next_data: &NextData) -> (u64, bool, bool) {
        let first_character_is_escaped = next_data.next_character_is_escaped;
        // If the first character is escaped, pretend it's not a backslash
        let backslashes = input.eq(BACKSLASHES).bitmask() & !(first_character_is_escaped as u64);
        // Find the position of escaped characters (the character after any odd-length series of backslashes)
        let (escape_mask, next_character_is_escaped) = escape_mask(backslashes);
        // Denote that the first character is escaped (if it is supposed to be)
        let escape_mask = escape_mask | first_character_is_escaped as u64;

        // println!("{:30}: {:064b}", "backslashes", backslashes.reverse_bits());
        // println!("{:30}: {:064b}", "backslash_run_starts", backslash_run_starts.reverse_bits());
        // println!("{:30}: {:064b}, next_character_is_escaped: {}", "escape_mask", escape_mask.reverse_bits(), next_character_is_escaped);
        (escape_mask, first_character_is_escaped, next_character_is_escaped)
    }

    fn find_strings(input: u8x64, escape_mask: u64, next_data: &NextData) -> (u64, u64) {
        let quotes = (input.eq(QUOTES).bitmask() & !escape_mask) | next_data.still_in_string;
        let string_mask = runs_from_pairs(quotes);
        let still_in_string = string_mask >> 63;
        (string_mask, still_in_string)
    }

    ///
    /// Given a mask with alternating run starts and ends, show the values between them (includes the
    /// start bit and excludes the end bit).
    /// 
    /// runs(0011010001001010)
    ///   -> 0010011110001100
    ///
    /// This works by multiplying the mask with u64::MAX with a carryless multiply.
    /// 
    /// Consider this (single-byte) example.
    ///   10010000
    /// x 11111111
    /// --------------
    ///   11111111
    /// + 00011111111
    /// ==============
    ///   111000001111
    ///   11100000     (truncated)
    /// 
    /// Which is exactly what we want. Essentially, the start and end bit shift the 1's, and they
    /// 1's cancel out everywhere except where they were shifted.
    /// 
    fn runs_from_pairs(pairs: u64) -> u64 {
        pairs.clmul(u64::ALL_BITS)
    }

    ///
    /// This is a parallel algorithm and runs in 9 iops.
    ///
    fn escape_mask(backslashes: u64) -> (u64, bool) {
        // Find the first backslash in each series of backslashes
        let escape_starts = backslashes & !(backslashes << 1);

        // Find out where runs of bits end by adding the starts to the bits, causing addition to overflow.
        // e.g.
        //      00011100011110010 (bits)
        //      00010000010000010 (run starts)
        //      00000010000001001 (run ends)
        //
        // We actually do this separately for runs that start on even indices, and runs that start
        // on odd indices. (We'll use this to check for odd vs. even length later). In this case
        // only the runs we want actually get a carry, so we have to mask out the original bits
        // to get rid of the runs we didn't check. i.e.:
        //      00011100011110010 (bits)
        //      00000000000000010 (even starts)
        //      00011100011110001 (even sum)
        //      00000000000000001 (even ends)
        let (even_sum, _) = backslashes.overflowing_add(escape_starts & u64::EVEN_BITS);
        let (odd_sum, odd_overflow) = backslashes.overflowing_add(escape_starts & u64::ODD_BITS);

        // To find runs with odd length, we find even runs that ended on odd bits (and vice versa): 
        //      00011100011110010 (bits)
        //      00000000000000001 (even ends)
        //      00000000000000001 (even ends on odd bits)
        //      00000010000001000 (odd ends)
        //      00000010000000000 (odd ends on even bits)
        //      00000010000000001 (odd length ends)
        let escaped_characters = ((even_sum & u64::ODD_BITS) | (odd_sum & u64::EVEN_BITS)) & !backslashes;

        // Overflow from odd_carries means an odd backslash run goes all the way to the end of the
        // input.
        let next_character_is_escaped = odd_overflow;

        println!("{:30}: {:064b}", "backslashes", backslashes.reverse_bits());
        println!("{:30}: {:064b}", "escaped_characters", escaped_characters.reverse_bits());
        let escape_mask = backslashes | escaped_characters;
        (escape_mask, next_character_is_escaped)
    }
}

#[inline]
fn calc_region_prefix_xor_64_pair(mut xor: u64, mut all_ones: u64, group_size: u64) -> (u64, u64) {
    println!("Level {:2}: xor = {:064b}, all_ones = {:064b}", group_size, xor.reverse_bits(), all_ones.reverse_bits());
    println!("   << {:2}        {:064b}", group_size, (xor << group_size).reverse_bits());
    println!(  "    & all_ones   {:064b}", ((xor << group_size) & all_ones).reverse_bits());
    println!(  "    ^ xor       {:064b}", (xor.bitxor((xor << group_size) & all_ones)).reverse_bits());
    xor ^= (xor << group_size) & all_ones;
    all_ones &= all_ones << group_size;
    println!("After {:2}: xor = {:064b}, all_ones = {:064b}", group_size, xor.reverse_bits(), all_ones.reverse_bits());
    (xor, all_ones)
}

pub fn region_prefix_xor_64(bits: u64) -> (u64, bool) {
    // xor the previous bit into each bit, unless the current bit is 0. This works out to NAND.
    // let xor_2 = bits & !(bits << 1);
    // let all_ones_2 = bits & (bits << 1);
    let (xor, all_ones) = calc_region_prefix_xor_64_pair(bits, bits, 1);
    let (xor, all_ones) = calc_region_prefix_xor_64_pair(xor, all_ones, 2);
    let (xor, all_ones) = calc_region_prefix_xor_64_pair(xor, all_ones, 4);
    let (xor, all_ones) = calc_region_prefix_xor_64_pair(xor, all_ones, 8);
    let (xor, all_ones) = calc_region_prefix_xor_64_pair(xor, all_ones, 16);
    let (xor, all_ones) = calc_region_prefix_xor_64_pair(xor, all_ones, 32);
    println!("Result:   xor = {:064b}, all_ones = {:064b}", xor.reverse_bits(), all_ones.reverse_bits());
    (xor, (all_ones & 0x8000_0000_0000_0000) > 0)
}

pub fn simple_region_prefix_xor_64(bits: u64) -> (u64, bool) {
    println!("Input:  {:064b}", bits.reverse_bits());
    let mut result = 0;
    let mut prev = 0;
    for i in 0..64 {
        let mut masked = bits & (1 << i);
        if masked > 0 && prev > 0 {
            masked = 0;
        }
        result |= masked;
        prev = masked;
    }
    println!("Result: {:064b}", result.reverse_bits());
    (result, bits == 0xffff_ffff_ffff_ffff)
}

#[cfg(test)]
mod tests {
    use super::*;

    mod parser64_tests {
        use super::*;
        use parser64::*;

        fn parse64_test(input: &[u8;64], first_character_is_escaped: bool, escape_mask_reversed: u64, next_character_is_escaped: bool) {
            println!("{:30}: {:64}", "input", String::from_utf8_lossy(input));
            println!("{:30}: {}", "first_character_is_escaped", first_character_is_escaped);
            let input = u8x64::from(*input);
            let (result, next) = parser64::parse(input, NextData { next_character_is_escaped: first_character_is_escaped, still_in_string: false as u64 });
            println!("{:30}: {:064b}", "escape_mask", result.escape_mask.reverse_bits());
            println!("{:30}: {:064b}", "escape_mask?", escape_mask_reversed);
            assert_eq!(escape_mask_reversed.reverse_bits(), result.escape_mask);
            println!("{:30}: {}", "next_character_is_escaped", next.next_character_is_escaped);
            println!("{:30}: {}", "next_character_is_escaped?", next_character_is_escaped);
            assert_eq!(next_character_is_escaped, next.next_character_is_escaped);
        }

        #[test]
        fn no_backslashes() {
            parse64_test(
                br"abcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcd", false,
                 0b0000000000000000000000000000000000000000000000000000000000000000, false
            )
        }
        #[test]
        fn no_backslashes_first_backslashed() {
            parse64_test(
                br"abcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcd", true,
                 0b1000000000000000000000000000000000000000000000000000000000000000, false
            )
        }
        #[test]
        fn all_backslashes() {
            parse64_test(
                br"\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\", false,
                 0b1111111111111111111111111111111111111111111111111111111111111111, false
            )
        }
        #[test]
        fn all_backslashes_first_backslashed() {
            parse64_test(
                br"\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\", true,
                 0b1111111111111111111111111111111111111111111111111111111111111111, true
            )
        }
        #[test]
        fn four_backslashes_first_backslashed() {
            parse64_test(
                br"\\\\                                                            ", true,
                 0b1111100000000000000000000000000000000000000000000000000000000000, false
            )
        }
        #[test]
        fn five_backslashes_first_backslashed() {
            parse64_test(
                br"\\\\\                                                           ", true,
                 0b1111100000000000000000000000000000000000000000000000000000000000, false
            )
        }
        #[test]
        fn almost_all_backslashes() {
            parse64_test(
                br"\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ ", false,
                 0b1111111111111111111111111111111111111111111111111111111111111111, false
            )
        }
        #[test]
        fn almost_all_backslashes_first_backslashed() {
            parse64_test(
                br"\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ ", true,
                 0b1111111111111111111111111111111111111111111111111111111111111110, false
            )
        }
        #[test]
        fn almost_all_backslashes_alternate() {
            parse64_test(
                br" \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\", false,
                 0b0111111111111111111111111111111111111111111111111111111111111111, true
            )
        }
        #[test]
        fn almost_all_backslashes_alternate_first_backslashed() {
            parse64_test(
                br" \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\", true,
                 0b1111111111111111111111111111111111111111111111111111111111111111, true
            )
        }
        #[test]
        fn many_different_backslashes() {
            parse64_test(
                br"\ \\ \\\ \\\\ \\\\\ \\\\\\ \\\\\\\ \\\\\\\\ \\\\\\\\\ \\\\\\\\\\", false,
                 0b1111011111111011111111111101111111111111111011111111111111111111, false
            )
        }
        #[test]
        fn many_different_backslashes_first_backslashed() {
            parse64_test(
                br"\ \\ \\\ \\\\ \\\\\ \\\\\\ \\\\\\\ \\\\\\\\ \\\\\\\\\ \\\\\\\\\\", true,
                 0b1011011111111011111111111101111111111111111011111111111111111111, false
            )
        }
        #[test]
        fn every_other_backslash() {
            parse64_test(
                br"\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ ", false,
                 0b1111111111111111111111111111111111111111111111111111111111111111, false
            )
        }
        #[test]
        fn every_other_backslash_first_backslashed() {
            parse64_test(
                br"\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ ", true,
                 0b1011111111111111111111111111111111111111111111111111111111111111, false
            )
        }
        #[test]
        fn every_other_backslash_alternate() {
            parse64_test(
                br" \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \", false,
                 0b0111111111111111111111111111111111111111111111111111111111111111, true
            )
        }
        #[test]
        fn every_other_backslash_alternate_first_backslashed() {
            parse64_test(
                br" \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \", true,
                 0b1111111111111111111111111111111111111111111111111111111111111111, true
            )
        }
        #[test]
        fn every_other_2_backslash() {
            parse64_test(
                br"\\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \", false,
                 0b1101101101101101101101101101101101101101101101101101101101101101, true
            )
        }
        #[test]
        fn every_other_2_backslash_alternate() {
            parse64_test(
                br" \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ ", false,
                 0b0110110110110110110110110110110110110110110110110110110110110110, false
            )
        }
        #[test]
        fn every_other_2_backslash_alternate2() {
            parse64_test(
                br"  \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\", false,
                 0b0011011011011011011011011011011011011011011011011011011011011011, false
            )
        }
        #[test]
        fn every_other_3_backslash() {
            parse64_test(
                br"\\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ ", false,
                 0b1111111111111111111111111111111111111111111111111111111111111111, false
            )
        }
        #[test]
        fn every_other_3_backslash_first_backslashed() {
            parse64_test(
                br"\\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ ", true,
                 0b1110111111111111111111111111111111111111111111111111111111111111, false
            )
        }
        #[test]
        fn every_other_3_backslash_alternate() {
            parse64_test(
                br" \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\", false,
                 0b0111111111111111111111111111111111111111111111111111111111111111, true
            )
        }
        #[test]
        fn every_other_3_backslash_alternate2() {
            parse64_test(
                br"\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\", false,
                 0b1111111111111111111111111111111111111111111111111111111111111111, false
            )
        }
        #[test]
        fn every_other_3_backslash_alternate3() {
            parse64_test(
                br"\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \", false,
                 0b1101111111111111111111111111111111111111111111111111111111111111, true
            )
        }
    }
    #[test]
    fn all_one() {
        assert_eq!(region_prefix_xor_64(0xffff_ffff_ffff_ffff), (0x5555_5555_5555_5555, true))
    }
    #[test]
    fn all_zero() {
        assert_eq!(region_prefix_xor_64(0x0000_0000_0000_0000), (0x0000_0000_0000_0000, false))
    }
    #[test]
    fn every_other() {
        assert_eq!(region_prefix_xor_64(0b10101010_10101010_10101010_10101010_10101010_10101010_10101010_10101010), (0b10101010_10101010_10101010_10101010_10101010_10101010_10101010_10101010, false))
    }
    #[test]
    fn every_other_reversed() {
        assert_eq!(region_prefix_xor_64(0b01010101_01010101_01010101_01010101_01010101_01010101_01010101_01010101), (0b01010101_01010101_01010101_01010101_01010101_01010101_01010101_01010101, false))
    }
    #[test]
    fn every_other_2() {
        assert_eq!(region_prefix_xor_64(0b11001100_11001100_11001100_11001100_11001100_11001100_11001100_11001100), (0b01000100_01000100_01000100_01000100_01000100_01000100_01000100_01000100, false))
    }
    #[test]
    fn every_other_2_reversed() {
        assert_eq!(region_prefix_xor_64(0b00110011_00110011_00110011_00110011_00110011_00110011_00110011_00110011), (0b00010001_00010001_00010001_00010001_00010001_00010001_00010001_00010001, false))
    }
    #[test]
    fn every_other_3() {
        assert_eq!(region_prefix_xor_64(0b11100011_10001110_00111000_11100011_10001110_00111000_11100011_10001110), (0b10100010_10001010_00101000_10100010_10001010_00101000_10100010_10001010, false))
    }
    #[test]
    fn every_other_3_reversed() {
        assert_eq!(region_prefix_xor_64(0b01110001_11000111_00011100_01110001_11000111_00011100_01110001_11000111), (0b01010001_01000101_00010100_01010001_01000101_00010100_01010001_01000101, false))
    }

    #[test]
    fn all_one_simple() {
        assert_eq!(simple_region_prefix_xor_64(0xffff_ffff_ffff_ffff), (0x5555_5555_5555_5555, true))
    }
    #[test]
    fn all_zero_simple() {
        assert_eq!(simple_region_prefix_xor_64(0x0000_0000_0000_0000), (0x0000_0000_0000_0000, false))
    }
    // #[test]
    // fn it_works() {
    //     let s = u8x64::from(*b"1111111111111111222222222222222233333333333333334444444444444444");
    //     assert_eq!(eq_mask(s, b'2'),
    //         m8x64::new(
    //             false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,
    //             true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,
    //             false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,
    //             false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,
    //         )
    //     );
    // }
}