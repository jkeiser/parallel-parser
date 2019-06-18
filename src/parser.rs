use packed_simd::*;
use crate::maskable::*;
use crate::streamable_bitmask::*;

#[derive(Clone,Debug,Default)]
pub struct ParsedChunk {
    ///
    /// The location of all backslashes
    /// 
    pub backslashes: u64x8,
    ///
    /// Escaped characters (so you can mask out non-backslashes)
    /// 
    pub escaped: u64x8,
    ///
    /// Regions with strings
    ///
    pub strings: u64x8,
    ///
    /// Invalid UTF-8 bytes
    /// 
    pub invalid_utf8: u64x8,
    ///
    /// Invalid bytes inside strings
    /// 
    pub invalid_string_bytes: u64x8,
}

#[derive(Clone,Debug,Default)]
pub struct ParseChunkOverflow {
    validate_utf8_overflow: ValidateUtf8Overflow,
    find_escapes_overflow: FindEscapesOverflow,
    find_strings_overflow: FindStringsOverflow,
}

pub fn parse_chunk(input: &[u8;512], overflow: ParseChunkOverflow) -> (ParsedChunk, ParseChunkOverflow) {
    let (ValidateUtf8 { invalid_utf8 }, validate_utf8_overflow) = validate_utf8(input, overflow.validate_utf8_overflow);
    let (FindEscapes { backslashes, escaped }, find_escapes_overflow) = find_escapes(input, overflow.find_escapes_overflow);
    let (FindStrings { strings, invalid_string_bytes }, find_strings_overflow) = find_strings(input, escaped, overflow.find_strings_overflow);

    (
        ParsedChunk { backslashes, escaped, strings, invalid_utf8, invalid_string_bytes },
        ParseChunkOverflow { validate_utf8_overflow, find_escapes_overflow, find_strings_overflow }
    )
}


#[derive(Clone,Debug,Default)]
pub struct FindEscapes {
    pub backslashes: u64x8,
    pub escaped: u64x8,
}

#[derive(Clone,Debug,Default)]
pub struct FindEscapesOverflow {
    after_odd_series_overflow: AfterOddSeriesOverflow<u64>,
    // last_4_unicode_hex_starts: u64,
}

pub fn find_escapes(input: &[u8;512], overflow: FindEscapesOverflow) -> (FindEscapes, FindEscapesOverflow) {
    let backslashes = each_64(input, |i| i.where_eq(b'\\'));
    let (escaped, after_odd_series_overflow) = backslashes.after_odd_series(overflow.after_odd_series_overflow);
    // Mark off the 4 hexes after \u as being escaped
    // let unicode_hex_starts = input.where_eq(b'u') & escaped;
    // let (last_4_unicode_hex_starts, last_4_unicode_hex_starts_overflow) = input.prev_range(1..4, overflow.last_4_unicode_hex_starts);
    // escaped |= last_4_unicode_hex_starts;
    (FindEscapes { backslashes, escaped }, FindEscapesOverflow { after_odd_series_overflow })
}


#[derive(Clone,Debug,Default)]
pub struct FindStrings {
    pub strings: u64x8,
    pub invalid_string_bytes: u64x8,
}

#[derive(Clone,Debug,Default)]
pub struct FindStringsOverflow {
    strings_overflow: u64
}

pub fn find_strings(input: &[u8;512], escaped: u64x8, overflow: FindStringsOverflow) -> (FindStrings, FindStringsOverflow) {
    let quotes = each_64(input, |i| i.where_eq(b'"')) & !escaped;
    let (strings, strings_overflow) = quotes.between_pairs(overflow.strings_overflow);
    // 00-1F are invalid characters within a string.
    let mut invalid_string_bytes = each_64(input, |i| i.where_lt(0x20)) & strings;
    // escape characters are invalid outside a string.
    invalid_string_bytes |= !strings & escaped;
    (FindStrings { strings, invalid_string_bytes }, FindStringsOverflow { strings_overflow })
}

#[derive(Clone,Debug,Default)]
pub struct ValidateUtf8 {
    pub invalid_utf8: u64x8
}

#[derive(Clone,Debug,Default)]
pub struct ValidateUtf8Overflow {
    after_second_bit_overflow: u64,
    after_third_bit_overflow: u64,
    after_fourth_bit_overflow: u64,
    preceded_by_e0_overflow: u64,
    preceded_by_f0_overflow: u64,
    preceded_by_f4_overflow: u64,
}

///
/// Validate UTF-8.
/// 
/// Invalid UTF-8 multibyte sequences will not have *all* bytes in the sequence invalidated, but in
/// all cases at least *one* of them will be.
/// 
pub fn validate_utf8(input: &[u8;512], overflow: ValidateUtf8Overflow) -> (ValidateUtf8, ValidateUtf8Overflow) {
    let first_bit = each_64(input, |i| i.where_ge(0b1000000));
    // ASCII only is so common we skip validation when it's off.
    if !first_bit.any() {
        return (Default::default(), Default::default());
    }

    // Valid Bytes
    //
    // Notes          | Bytes | Bits     | Invalid After | Invalid After (2) | Invalid After (3) |
    // ---------------|-------|----------|---------------|-------------------|-------------------|
    // ASCII          | 00-0F | 0xxxxxxx | 11xxxxxx      | 111xxxxx          | 1111xxxx          |
    // Continuation   | 80-BF | 10xxxxxx | x0xxxxxx      | xx0xxxxx          | xxx0xxxx          |
    //                | 80-8F | 1000xxxx | 111x0000 E0,F0|                   |                   |
    //                | 90-9F | 1001xxxx | 11110x00 F0,F4|                   |                   |
    //                | A0-BF | 101xxxxx | 11110100 F4
    // 2 Byte Head    | C0-DF | 110xxxxx | 11xxxxxx      | 111xxxxx          | 1111xxxx          |
    //                | C0-C1 | 1100000x | xxxxxxxx      |                   |                   |
    // 3 Byte Head    | E0-EF | 1110xxxx | 11xxxxxx      | 111xxxxx          | 1111xxxx          |
    // 4 Byte Head    | F0-F7 | 11110xxx | 11xxxxxx      | 111xxxxx          | 1111xxxx          |
    //                | F5,F7 | 1111011x | xxxxxxxx      |                   |                   |
    //                | F6,F7 | 111101x1 | xxxxxxxx      |                   |                   |
    // 5+ Byte Head   | F8-FF | 11111xxx | xxxxxxxx      | 111xxxxx          | 1111xxxx          |

    // C0 (11000000), C1 (11000001) and F5-FF (11110101-11111111) are all invalid in UTF-8 no matter what.
    let mut invalid_utf8 = each_64(input, |i| i.where_eq(0xC0) | i.where_eq(0xC1) | i.where_ge(0xF5));
    // Continuations that are *not* after a second, third or fourth bit are always invalid.
    // *Non*-continuations after a second, third or fourth bit are always invalid.
    // This means it's an exclusive or :)
    // NOTE: this means even if we parse everything, we need to go 3 bytes further to fully detect invalid UTF-8.
    // TODO look into whether we get real savings by getting separate masks for each bit and comparing down the line.
    // Each where_xxx() does *8* comparisons because it has to hit the whole 512 bytes.

    let second_bit  = each_64(input, |i| i.where_bits_set(0b01000000));
    let third_bit   = each_64(input, |i| i.where_bits_set(0b00100000));
    let fourth_bit  = each_64(input, |i| i.where_bits_set(0b00010000));
    let continuation        = first_bit & !second_bit;
    let (after_second_bit, after_second_bit_overflow) = second_bit.prev(overflow.after_second_bit_overflow);
    let (after_third_bit, after_third_bit_overflow) = third_bit.back(2, overflow.after_third_bit_overflow);
    let (after_fourth_bit, after_fourth_bit_overflow) = fourth_bit.back(3, overflow.after_fourth_bit_overflow);
    invalid_utf8 |= continuation ^ !(after_second_bit | after_third_bit | after_fourth_bit);

    // Overlong UTF-8 encodings (http://www.unicode.org/versions/corrigendum1.html): 1001xxxx, 1010xxxx, 1011xxxx
    // - E0 followed by 80-9F (00-9F,C0-FF really) = 11100000 followed by x < 10100000 or x >= 11000000. MUST start with 101.                            invalidate xx1xxxxx or < A0.
    // - F0 followed by 80-8F (00-8F,C0-FF really) = 11110000 followed by x < 10010000 or x >= 11000000. MUST NOT start with 1000 (1001, 1010, 1011 ok.) invalidate xx00xxxx or < 90.
    // - F4 followed by 90-BF (00-7F,90-FF really) = 11110100 followed by x < 10000000 or x >= 10010000. MUST start with 1000.                           invalidate xx1xxxxx or xxx1xxxx or >= 90.
    let (preceded_by_e0, preceded_by_e0_overflow) = each_64(input, |i| i.where_eq(0xE0)).prev(overflow.preceded_by_e0_overflow);
    let (preceded_by_f0, preceded_by_f0_overflow) = each_64(input, |i| i.where_eq(0xF0)).prev(overflow.preceded_by_f0_overflow);
    let (preceded_by_f4, preceded_by_f4_overflow) = each_64(input, |i| i.where_eq(0xF4)).prev(overflow.preceded_by_f4_overflow);
    // At this point, we're ONLY deciding whether valid continuation bytes form overlong sequences, so we know all these bytes start with 10xxxxxx.
    let ge_a0 = third_bit; // To be >= A0 (10100000), it has to start with 10 AND have its third bit set.
    invalid_utf8 |= preceded_by_e0 & !ge_a0; // & each_64(input, |i| i.where_lt(0xA0));
    let ge_90 = third_bit | fourth_bit; // To be >= 90 (10010000), it has to start with 10 and have its third OR fourth bits set.
    invalid_utf8 |= preceded_by_f0 & !ge_90; // each_64(input, |i| i.where_lt(0x90));
    invalid_utf8 |= preceded_by_f4 & ge_90;

    (ValidateUtf8 { invalid_utf8 }, ValidateUtf8Overflow { after_second_bit_overflow, after_third_bit_overflow, after_fourth_bit_overflow, preceded_by_e0_overflow, preceded_by_f0_overflow, preceded_by_f4_overflow })

    // In all, in thie procedure we check:
    // bits set: 1, 1, 1, 1
    // bit         (1       )
    // bit         ( 1      )
    // bit         (  1     )
    // bit         (   1    )
    // invalid C0/C1/F5-FF:
    // == 11000000 (11000000)
    // == 11000001 (11000001)
    // >= 11110101 (11110101)
    // missing or extra continuations:
    //             (10      )
    //             ( 1      )
    //             (  1     )
    //             (   1    )
    // Overlong:
    // E0          (11100000)
    // F0          (11110000)
    // F4          (11110100)
    // < A0        (  1     )
    // >= 90       (  1     )
    //             (   1    )
    //             (1111011 )
    //             (11111   )
    // == 11100000 (11100000)
    // == 11110000 (11110000)
    // == 11110100 (11110100)

    // TODO this was a bit of "bit twiddling," see if it helps or hurts :)
    // Could hurt because we'd need to keep more stuff in cache ... but then again, it really might
    // not, because the masks aren't going to take any more than the buffer itself did, and this lets
    // us throw away the buffer.
    // // Handle overlong 
    // let (after_e0_or_f0_overflow, after_f0_or_f4_overflow, after_f4_overflow) = 
    // {
    //     // Figure out whether the preceding byte was e0, f0 or f4. Those can have overlong bytes.
    //     // e0 11100000
    //     // f0 11110000
    //     // f4 11110100
    //     let e0_f0_or_f4 = mask_111xxxxx & !mask_xxxx1xxxx & mask_xxxxxx00;
        
    //     let (after_e0_or_f0, after_e0_or_f0_overflow) = {
    //         let e0_or_f0 = e0_f0_or_f4 & !mask_xxxxx1xx;
    //         e0_or_f0.prev(overflow.after_e0_or_f0_overflow)
    //     };
    //     let (after_f0_or_f4, after_f0_or_f4_overflow) = {
    //         let f0_or_f4 = e0_f0_or_f4 & mask_xxx1xxxx;
    //         f0_or_f4.prev(overflow.after_f0_or_f4_overflow)
    //     };
    //     let (after_f4, after_f4_overflow) = {
    //         let f4 = f0_or_f4 & mask_xxxxx1xx;
    //         f4.prev(overflow.after_f4_overflow)
    //     }
    //     // 11x and 00x will be marked invalid after F4 anyway, so we only concern ourselves with the
    //     // later bits.
    //     // 1000xxxx (80-8F) is always overlong after E0 or F0.
    //     // 1001xxxx (90-9F) is always overlong after F0 or F4.
    //     // 101xxxxx (A0-BF) is always overlong after F4.
    //     invalid_utf8 |= after_e0_or_f0 & mask_
    //     invalid_utf8 |= after_f0_or_f4 & mask_xx1xxxxx;
    //     invalid_utf8 |= after_f4 & mask_xx1xxxxx;
    //     (after_e0_or_f0_overflow, after_f0_or_f4_overflow, after_f4_overflow)
    // }
    // invalid

}

fn each_64<F: Fn(u8x64) -> u64>(input: &[u8;512], f: F) -> u64x8{
    let mut result = u64x8::splat(0);
    for i in 0..8 {
        let start = i*64;
        let input64 = u8x64::from_le(unsafe { u8x64::from_slice_unaligned_unchecked(&input[start..start+64]) });
        result = result.replace(i, f(input64));
    }
    result
}



// type Mask = u64x8;
// type Bitmask = u64;
// type Bytes = [u8;Chunks::BYTE_WIDTH];
// pub const BYTES_PER_CHUNK: usize = 1;
// pub const NUM_CHUNKS: usize = 1;
// pub const BYTE_WIDTH: usize = NUM_CHUNKS*BYTES_PER_CHUNK;


// pub struct BlockData {
//     pub escape_mask: Chunks::Bitmask,
//     pub string_mask: Chunks::Bitmask,
//     pub first_character_is_escaped: bool,
// }
// pub struct NextData {a
//     pub still_in_string: bool,
//     pub next_character_is_escaped: bool,
// }

// pub fn parse(input: Chunks, next_data: NextData) -> (BlockData, NextData) {
//     let (escape_mask, first_character_is_escaped, next_character_is_escaped) = find_backslashes(input, &next_data);
//     let (string_mask, still_in_string) = find_strings(input, escape_mask, &next_data);
//     (BlockData { escape_mask, string_mask, first_character_is_escaped }, NextData { next_character_is_escaped, still_in_string })
// }

// pub fn backslash_mask(input: u8x64) -> u64 {
//     input.eq(BACKSLASHES).bitmask()
// }


// const BACKSLASHES: Chunks = u8x64::splat(b'\\');
// const QUOTES: Chunks = u8x64::splat(b'"');

// fn find_backslashes<T: Maskable<u8>>(input: T, prev_overflow: bool) -> (u64, bool, bool) {
//     let backslashes = input.where_eq('\\');
//     let first_character_is_escaped = next_data.next_character_is_escaped;
//     // If the first character is escaped, pretend it's not a backslash
//     let backslashes = backslash_mask(input) & !(first_character_is_escaped as u64);
//     // Find the position of escaped characters (the character after any odd-length series of backslashes)
//     let (escape_mask, next_character_is_escaped) = escape_mask(backslashes);
//     // Denote that the first character is escaped (if it is supposed to be)
//     let escape_mask = escape_mask | first_character_is_escaped as u64;

//     // println!("{:30}: {:064b}", "backslashes", backslashes.reverse_bits());
//     // println!("{:30}: {:064b}", "backslash_run_starts", backslash_run_starts.reverse_bits());
//     // println!("{:30}: {:064b}, next_character_is_escaped: {}", "escape_mask", escape_mask.reverse_bits(), next_character_is_escaped);
//     (escape_mask, first_character_is_escaped, next_character_is_escaped)
// }

// fn find_strings(input: Chunks, escape_mask: Bitmask, next_data: &NextData) -> (Bitmask, bool) {
//     let quotes = (input.eq(QUOTES).bitmask() & !escape_mask) | next_data.still_in_string as u64;
//     let string_mask = runs_from_pairs(quotes);
//     let still_in_string = (string_mask & u64::TOP_BIT) != 0;
//     (string_mask, still_in_string)
// }

// ///
// /// Given a mask with alternating run starts and ends, show the values between them (includes the
// /// start bit and excludes the end bit).
// /// 
// /// runs(0011010001001010)
// ///   -> 0010011110001100
// ///
// /// This works by multiplying the mask with u64::MAX with a carryless multiply.
// /// 
// /// Consider this (single-byte) example.
// ///   10010000
// /// x 11111111
// /// --------------
// ///   11111111
// /// + 00011111111
// /// ==============
// ///   111000001111
// ///   11100000     (truncated)
// /// 
// /// Which is exactly what we want. Essentially, the start and end bit shift the 1's, and they
// /// 1's cancel out everywhere except where they were shifted.
// /// 
// fn runs_from_pairs(pairs: u64) -> u64 {
//     pairs.clmul(u64::ALL_BITS)
// }

// // fn escape_mask_stream(bytes: impl SimdIterator<u8>) -> impl SimdBitmask {
// //     let even_mask = simd::repeat(&[true,false]);
// //     let odd_mask = simd::repeat(&[false,true]);

// //     let backslashes = bytes.simd_map_to_mask(|b| b == b'\\');
// //     let escape_starts = backslashes & !(backslashes >> 1);
// //     let even_ends = (escape_starts & even_mask) + backslashes;
// //     let odd_ends = (escape_starts & odd_mask) + backslashes;
// //     let escaped_characters = (even_ends & odd_mask) | (odd_ends & even_mask);
// //     let escape_mask = backslashes | escaped_characters;
// //     escape_mask;
// // }

// // fn string_mask_stream(bytes: impl SimdIterator<u8>, escape_mask: impl SimdBitmask) -> impl SimdBitmask {
// //     let all_ones = simd::repeat(true);
// //     let quotes = bytes.simd_map_to_mask(|b| b == b'"') & !escape_mask;
// //     let string_mask_without_end_quotes = quotes.cmul(all_ones);
// //     let end_quotes = quotes & !(quotes << 1);
// //     string_mask_without_end_quotes & end_quotes;
// // }

// ///
// /// This is a parallel algorithm and runs in 9 iops.
// ///
// fn escape_mask(backslashes: u64) -> (u64, bool) {
//     // Find the first backslash in each series of backslashes
//     let escape_starts = backslashes & !(backslashes << 1);


//     // Find out where runs of bits end by adding the starts to the bits, causing addition to overflow.
//     // e.g.
//     //      00011100011110010 (bits)
//     //      00010000010000010 (run starts)
//     //      00000010000001001 (run ends)
//     //
//     // We actually do this separately for runs that start on even indices, and runs that start
//     // on odd indices. (We'll use this to check for odd vs. even length later). In this case
//     // only the runs we want actually get a carry, so we have to mask out the original bits
//     // to get rid of the runs we didn't check. i.e.:
//     //      00011100011110010 (bits)
//     //      00000000000000010 (even starts)
//     //      00011100011110001 (even sum)
//     //      00000000000000001 (even ends)
//     let (even_sum, _) = backslashes.overflowing_add(escape_starts & u64::EVEN_BITS);
//     let (odd_sum, odd_overflow) = backslashes.overflowing_add(escape_starts & u64::ODD_BITS);

//     // To find runs with odd length, we find even runs that ended on odd bits (and vice versa): 
//     //      00011100011110010 (bits)
//     //      00000000000000001 (even ends)
//     //      00000000000000001 (even ends on odd bits)
//     //      00000010000001000 (odd ends)
//     //      00000010000000000 (odd ends on even bits)
//     //      00000010000000001 (odd length ends)
//     let escaped_characters = ((even_sum & u64::ODD_BITS) | (odd_sum & u64::EVEN_BITS)) & !backslashes;

//     // Overflow from odd_carries means an odd backslash run goes all the way to the end of the
//     // input.
//     let next_character_is_escaped = odd_overflow;

//     println!("{:30}: {:064b}", "backslashes", backslashes.reverse_bits());
//     println!("{:30}: {:064b}", "escaped_characters", escaped_characters.reverse_bits());
//     let escape_mask = backslashes | escaped_characters;
//     (escape_mask, next_character_is_escaped)
// }

#[cfg(test)]
mod tests {
    mod find_escapes {
        use super::super::*;
        fn escape_test(input64: &[u8;64], first_character_is_escaped: bool, escape_mask_reversed: u64, next_character_is_escaped: bool) {
            let mut input: [u8;512] = [0;512];
            input[0..64].copy_from_slice(input64);
            println!("{:30}: {:64}", "input", String::from_utf8_lossy(&input[0..64]));
            println!("{:30}: {}", "first_character_is_escaped", first_character_is_escaped);
            let (result, next) = find_escapes(&input, FindEscapesOverflow { after_odd_series_overflow: AfterOddSeriesOverflow { start_overflow: next_character_is_escaped as u64, even_overflow: false, odd_overflow: next_character_is_escaped } });
            let escape_mask = result.backslashes.extract(0) | result.escaped.extract(0);
            println!("{:30}: {:064b}", "escape_mask", escape_mask.reverse_bits());
            println!("{:30}: {:064b}", "escape_mask?", escape_mask_reversed);
            assert_eq!(escape_mask_reversed.reverse_bits(), escape_mask);
            println!("{:30}: {}", "next_character_is_escaped", next.after_odd_series_overflow.odd_overflow);
            println!("{:30}: {}", "next_character_is_escaped?", next_character_is_escaped);
            assert_eq!(next_character_is_escaped, next.after_odd_series_overflow.odd_overflow);
        }

        #[test]
        fn no_backslashes() {
            escape_test(
                br"abcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcd", false,
                 0b0000000000000000000000000000000000000000000000000000000000000000, false
            )
        }
        #[test]
        fn no_backslashes_first_backslashed() {
            escape_test(
                br"abcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcd", true,
                 0b1000000000000000000000000000000000000000000000000000000000000000, false
            )
        }
        #[test]
        fn all_backslashes() {
            escape_test(
                br"\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\", false,
                 0b1111111111111111111111111111111111111111111111111111111111111111, false
            )
        }
        #[test]
        fn all_backslashes_first_backslashed() {
            escape_test(
                br"\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\", true,
                 0b1111111111111111111111111111111111111111111111111111111111111111, true
            )
        }
        #[test]
        fn four_backslashes_first_backslashed() {
            escape_test(
                br"\\\\                                                            ", true,
                 0b1111100000000000000000000000000000000000000000000000000000000000, false
            )
        }
        #[test]
        fn five_backslashes_first_backslashed() {
            escape_test(
                br"\\\\\                                                           ", true,
                 0b1111100000000000000000000000000000000000000000000000000000000000, false
            )
        }
        #[test]
        fn almost_all_backslashes() {
            escape_test(
                br"\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ ", false,
                 0b1111111111111111111111111111111111111111111111111111111111111111, false
            )
        }
        #[test]
        fn almost_all_backslashes_first_backslashed() {
            escape_test(
                br"\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ ", true,
                 0b1111111111111111111111111111111111111111111111111111111111111110, false
            )
        }
        #[test]
        fn almost_all_backslashes_alternate() {
            escape_test(
                br" \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\", false,
                 0b0111111111111111111111111111111111111111111111111111111111111111, true
            )
        }
        #[test]
        fn almost_all_backslashes_alternate_first_backslashed() {
            escape_test(
                br" \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\", true,
                 0b1111111111111111111111111111111111111111111111111111111111111111, true
            )
        }
        #[test]
        fn many_different_backslashes() {
            escape_test(
                br"\ \\ \\\ \\\\ \\\\\ \\\\\\ \\\\\\\ \\\\\\\\ \\\\\\\\\ \\\\\\\\\\", false,
                 0b1111011111111011111111111101111111111111111011111111111111111111, false
            )
        }
        #[test]
        fn many_different_backslashes_first_backslashed() {
            escape_test(
                br"\ \\ \\\ \\\\ \\\\\ \\\\\\ \\\\\\\ \\\\\\\\ \\\\\\\\\ \\\\\\\\\\", true,
                 0b1011011111111011111111111101111111111111111011111111111111111111, false
            )
        }
        #[test]
        fn every_other_backslash() {
            escape_test(
                br"\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ ", false,
                 0b1111111111111111111111111111111111111111111111111111111111111111, false
            )
        }
        #[test]
        fn every_other_backslash_first_backslashed() {
            escape_test(
                br"\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ ", true,
                 0b1011111111111111111111111111111111111111111111111111111111111111, false
            )
        }
        #[test]
        fn every_other_backslash_alternate() {
            escape_test(
                br" \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \", false,
                 0b0111111111111111111111111111111111111111111111111111111111111111, true
            )
        }
        #[test]
        fn every_other_backslash_alternate_first_backslashed() {
            escape_test(
                br" \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \", true,
                 0b1111111111111111111111111111111111111111111111111111111111111111, true
            )
        }
        #[test]
        fn every_other_2_backslash() {
            escape_test(
                br"\\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \", false,
                 0b1101101101101101101101101101101101101101101101101101101101101101, true
            )
        }
        #[test]
        fn every_other_2_backslash_alternate() {
            escape_test(
                br" \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ ", false,
                 0b0110110110110110110110110110110110110110110110110110110110110110, false
            )
        }
        #[test]
        fn every_other_2_backslash_alternate2() {
            escape_test(
                br"  \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\", false,
                 0b0011011011011011011011011011011011011011011011011011011011011011, false
            )
        }
        #[test]
        fn every_other_3_backslash() {
            escape_test(
                br"\\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ ", false,
                 0b1111111111111111111111111111111111111111111111111111111111111111, false
            )
        }
        #[test]
        fn every_other_3_backslash_first_backslashed() {
            escape_test(
                br"\\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ ", true,
                 0b1110111111111111111111111111111111111111111111111111111111111111, false
            )
        }
        #[test]
        fn every_other_3_backslash_alternate() {
            escape_test(
                br" \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\", false,
                 0b0111111111111111111111111111111111111111111111111111111111111111, true
            )
        }
        #[test]
        fn every_other_3_backslash_alternate2() {
            escape_test(
                br"\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\", false,
                 0b1111111111111111111111111111111111111111111111111111111111111111, false
            )
        }
        #[test]
        fn every_other_3_backslash_alternate3() {
            escape_test(
                br"\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \\\ \", false,
                 0b1101111111111111111111111111111111111111111111111111111111111111, true
            )
        }
    }
}
