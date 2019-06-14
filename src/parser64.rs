use packed_simd::*;
use crate::streamable_bitmask::*;
use crate::carryless_mul::CarrylessMul;



type Mask = u64x8;
type Bitmask = u64;
type Bytes = [u8;Chunks::BYTE_WIDTH];
pub const BYTES_PER_CHUNK: usize = 1;
pub const NUM_CHUNKS: usize = 1;
pub const BYTE_WIDTH: usize = NUM_CHUNKS*BYTES_PER_CHUNK;


pub struct BlockData {
    pub escape_mask: Chunks::Bitmask,
    pub string_mask: Chunks::Bitmask,
    pub first_character_is_escaped: bool,
}
pub struct NextData {
    pub still_in_string: bool,
    pub next_character_is_escaped: bool,
}

pub fn parse(input: Chunks, next_data: NextData) -> (BlockData, NextData) {
    let (escape_mask, first_character_is_escaped, next_character_is_escaped) = find_backslashes(input, &next_data);
    let (string_mask, still_in_string) = find_strings(input, escape_mask, &next_data);
    (BlockData { escape_mask, string_mask, first_character_is_escaped }, NextData { next_character_is_escaped, still_in_string })
}

pub fn backslash_mask(input: u8x64) -> u64 {
    input.eq(BACKSLASHES).bitmask()
}


const BACKSLASHES: Chunks = u8x64::splat(b'\\');
const QUOTES: Chunks = u8x64::splat(b'"');

fn find_backslashes<T: Maskable<u8>>(input: T, prev_overflow: bool) -> (u64, bool, bool) {
    let backslashes = input.where_eq('\\');
    let first_character_is_escaped = next_data.next_character_is_escaped;
    // If the first character is escaped, pretend it's not a backslash
    let backslashes = backslash_mask(input) & !(first_character_is_escaped as u64);
    // Find the position of escaped characters (the character after any odd-length series of backslashes)
    let (escape_mask, next_character_is_escaped) = escape_mask(backslashes);
    // Denote that the first character is escaped (if it is supposed to be)
    let escape_mask = escape_mask | first_character_is_escaped as u64;

    // println!("{:30}: {:064b}", "backslashes", backslashes.reverse_bits());
    // println!("{:30}: {:064b}", "backslash_run_starts", backslash_run_starts.reverse_bits());
    // println!("{:30}: {:064b}, next_character_is_escaped: {}", "escape_mask", escape_mask.reverse_bits(), next_character_is_escaped);
    (escape_mask, first_character_is_escaped, next_character_is_escaped)
}

fn find_strings(input: Chunks, escape_mask: Bitmask, next_data: &NextData) -> (Bitmask, bool) {
    let quotes = (input.eq(QUOTES).bitmask() & !escape_mask) | next_data.still_in_string as u64;
    let string_mask = runs_from_pairs(quotes);
    let still_in_string = (string_mask & u64::TOP_BIT) != 0;
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

// fn escape_mask_stream(bytes: impl SimdIterator<u8>) -> impl SimdBitmask {
//     let even_mask = simd::repeat(&[true,false]);
//     let odd_mask = simd::repeat(&[false,true]);

//     let backslashes = bytes.simd_map_to_mask(|b| b == b'\\');
//     let escape_starts = backslashes & !(backslashes >> 1);
//     let even_ends = (escape_starts & even_mask) + backslashes;
//     let odd_ends = (escape_starts & odd_mask) + backslashes;
//     let escaped_characters = (even_ends & odd_mask) | (odd_ends & even_mask);
//     let escape_mask = backslashes | escaped_characters;
//     escape_mask;
// }

// fn string_mask_stream(bytes: impl SimdIterator<u8>, escape_mask: impl SimdBitmask) -> impl SimdBitmask {
//     let all_ones = simd::repeat(true);
//     let quotes = bytes.simd_map_to_mask(|b| b == b'"') & !escape_mask;
//     let string_mask_without_end_quotes = quotes.cmul(all_ones);
//     let end_quotes = quotes & !(quotes << 1);
//     string_mask_without_end_quotes & end_quotes;
// }

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

#[cfg(test)]
mod tests {
    use super::*;

    fn parse64_test(input: &[u8;64], first_character_is_escaped: bool, escape_mask_reversed: u64, next_character_is_escaped: bool) {
        println!("{:30}: {:64}", "input", String::from_utf8_lossy(input));
        println!("{:30}: {}", "first_character_is_escaped", first_character_is_escaped);
        let input = u8x64::from(*input);
        let (result, next) = parse(input, NextData { next_character_is_escaped: first_character_is_escaped, still_in_string: false });
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
