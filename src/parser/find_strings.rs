use packed_simd::*;
use crate::maskable::*;
use crate::streamable_bitmask::*;
use crate::separated_bits::*;
use super::*;

#[derive(Clone,Debug,Default)]
pub struct FindStrings {
    ///
    /// Location of all string characters.
    /// 
    /// Includes the open quote of each string, but not the close quote.
    /// 
    pub strings: u64x8,
    ///
    /// Locations of all escaping backslashes.
    /// 
    pub escapes: u64x8,
    ///
    /// The locations of escaped characters (the n \n or \\\n).
    /// 
    /// Correctly detects even #'s of escapes, so the n in \\n will not be escaped.
    /// 
    pub escaped: u64x8,
    ///
    /// Location of invalid bytes inside a string.
    /// 
    pub invalid_string_bytes: u64x8,
    // ///
    // /// Location of escaped unicode hex (\u12BE)
    // /// 
    // pub escaped_unicode_hex: u64
}

#[derive(Clone,Debug,Default)]
pub struct FindStringsOverflow {
    starts_backslash_series: u8,
    escaped: u8,
    backslash_series_starting_on_odd_boundary: bool,
    strings_overflow: bool,
    // escaped_unicode_hex_1: u8,
    // escaped_unicode_hex_2: u8,
    // escaped_unicode_hex_3: u8,
    // escaped_unicode_hex_4: u8,
}

pub fn find_escapes(overflow: &mut FindStringsOverflow, input: &SeparatedBits) -> (u64x8, u64x8) {
    //
    // Find backslashed characters (including quotes).
    //
    // First, find series of backslashes that start on an even boundaries.
    //
    let backslash = input.where_eq(b'\\');
    println!("{:10}: {}", "backslash", into_x_str(backslash));
    let starts_backslash_series = backslash.starts_series(&mut overflow.starts_backslash_series);
    let backslash_series_starting_on_odd_boundary = backslash.series_not_starting_with(starts_backslash_series & u64x8::EVEN_BITS, &mut overflow.backslash_series_starting_on_odd_boundary);

    // Next, turn series of backslashes on even boundaries into 10101010 using XOR,
    // and series on odd boundaries the same way, to get the backslashes that actually escape
    // something.
    let escapes = (u64x8::ODD_BITS & backslash_series_starting_on_odd_boundary) |
                  (u64x8::EVEN_BITS & backslash & !backslash_series_starting_on_odd_boundary);
    // The escaped characters are the characters right after a "real" escaping backslash.
    let escaped = escapes.prev(&mut overflow.escaped);
    println!("{:10}: {}", "escapes", into_x_str(escapes));
    println!("{:10}: {}", "escaped", into_x_str(escaped));
    (escapes, escaped)
}

///
/// Find strings and string parts.
/// 
pub fn find_strings(overflow: &mut FindStringsOverflow, input: &SeparatedBits) -> FindStrings {
    println!("{:10}: {}", "bit0", into_x_str(input[0]));
    println!("{:10}: {}", "bit1", into_x_str(input[1]));
    println!("{:10}: {}", "bit2", into_x_str(input[2]));
    println!("{:10}: {}", "bit3", into_x_str(input[3]));
    println!("{:10}: {}", "bit4", into_x_str(input[4]));
    println!("{:10}: {}", "bit5", into_x_str(input[5]));
    println!("{:10}: {}", "bit6", into_x_str(input[6]));
    println!("{:10}: {}", "bit7", into_x_str(input[7]));

    //
    // Exit early if there are no quotes.
    //
    // BRANCH NOTE: Strings are so frequent that almost all chunks in all files will have them; the
    // only files that won't, pretty much, are tiny empty JSON. In those few cases, avoiding the
    // CMUL is worth the misprediction, because in use cases where they *do* happen they are
    // likely to happen with enough frequency to matter. TODO find the % where matters.
    //
    // Invalid backslashes can still occur, but those will be detected in the "unexpected character"
    // catchall that looks for non-whitespace outside of strings, numbers, literals and structure.
    //
    let quote = input.where_eq(b'"');
    println!("{:10}: {}", "quotes", into_x_str(quote));
    if !quote.any() && !overflow.strings_overflow {
        return Default::default();
    }

    // Figure out which characters are escaped
    let (escapes, escaped) = find_escapes(overflow, input);

    // Find characters in strings (every other ", not counting \").
    let strings = (quote & !escaped).between_pairs(&mut overflow.strings_overflow);

    // Check for invalid string characters (00-1F).
    let invalid_string_bytes = input.where_lt(0x20) & strings; // 00-1F

    // TODO ensure we don't *end* the stream in a string.

    // // Find Unicode escapes (\uDDDD ranges)
    // let escaped_u = each_64(input, |i| i.where_eq(b'u'));
    // // BRANCH NOTE: this is rare enough to be worth skipping when we can
    // let (escaped_unicode_hex, invalid_escaped_unicode_hex) = if escaped_u.any() {
    //     // Best time to validate the digits is during copy.
    //     // TODO make sure that "\u","D" doesn't break the world
    //     let invalid_escaped_unicode_hex = escaped_unicode_hex & !hex_digit;
    //     (escaped_unicode_hex, invalid_escaped_unicode_hex)
    // }

    println!("{:10}: {}", "strings", into_x_str(strings));

    FindStrings { strings, escapes, escaped, invalid_string_bytes }
}
