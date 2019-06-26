use packed_simd::*;
use crate::maskable::*;
use crate::streamable_bitmask::*;
use crate::separated_bits::*;

#[derive(Clone,Debug,Default)]
pub struct ParsedChunk {
    ///
    /// Invalid UTF-8 bytes
    /// 
    pub invalid_utf8: u64x8,

    ///
    /// The location of all escaping backslashes
    /// 
    pub escapes: u64x8,
    ///
    /// Escaped characters (characters with an escaping backslash in front of them, including \\)
    /// 
    pub escaped: u64x8,
    ///
    /// Regions with strings
    ///
    pub strings: u64x8,
    ///
    /// Invalid bytes inside strings
    /// 
    pub invalid_string_bytes: u64x8,

    ///
    /// The start of valid true literals
    /// 
    pub true_literal: u64x8,
    ///
    /// The start of valid false literals
    /// 
    pub false_literal: u64x8,
    ///
    /// The "n" in null
    /// 
    pub null_literal: u64x8,
    ///
    /// Mask of literal characters (true/false/null).
    /// 
    /// Superceded by "number" mask since this includes the "e" in valid floats (such as 1.2e8).
    ///
    pub literal_name: u64x8,
    ///
    /// Mask where previous character is *not* literal.
    /// 
    /// literal_name & (prev_not_literal_name << 1) tells you what literals are invalid.
    /// 
    pub prev_not_literal_name: u64x8,
}

#[derive(Clone,Debug,Default)]
pub struct ParseChunkOverflow {
    validate_utf8_overflow: ValidateUtf8Overflow,
    find_strings_overflow: FindStringsOverflow,
    find_literal_names_overflow: FindLiteralNamesOverflow,
}

pub fn parse_chunk(input: &[u8;512], overflow: &mut ParseChunkOverflow) -> ParsedChunk {
    let bits = separate_bits(input);
    let invalid_utf8 = validate_utf8(&bits, &mut overflow.validate_utf8_overflow);
    let FindStrings { strings, escapes, escaped, invalid_string_bytes } = find_strings(&bits, &mut overflow.find_strings_overflow);
    let FindLiteralNames { true_literal, false_literal, null_literal, literal_name, prev_not_literal_name } = find_literal_names(&bits, &mut overflow.find_literal_names_overflow);

    ParsedChunk { escapes, escaped, strings, invalid_utf8, invalid_string_bytes, true_literal, false_literal, null_literal, literal_name, prev_not_literal_name }
}


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

///
/// Find strings and string parts.
/// 
pub fn find_strings(input: &SeparatedBits, overflow: &mut FindStringsOverflow) -> FindStrings {
    //
    // Exit early if there are no quotes.
    //
    //
    // BRANCH NOTE: Strings are so frequent that almost all chunks in all files will have them; the
    // only files that won't, pretty much, are tiny empty JSON. In those few cases, avoiding the
    // CMUL is worth the misprediction, because in use cases where they *do* happen they are
    // likely to happen with enough frequency to matter. TODO find the % where matters.
    //
    // Invalid backslashes can still occur, but those will be detected in the "unexpected character"
    // catchall that looks for non-whitespace outside of strings, numbers, literals and structure.
    //
    println!("{:10}: {}", "bit0", into_x_str(input[0]));
    println!("{:10}: {}", "bit1", into_x_str(input[1]));
    println!("{:10}: {}", "bit2", into_x_str(input[2]));
    println!("{:10}: {}", "bit3", into_x_str(input[3]));
    println!("{:10}: {}", "bit4", into_x_str(input[4]));
    println!("{:10}: {}", "bit5", into_x_str(input[5]));
    println!("{:10}: {}", "bit6", into_x_str(input[6]));
    println!("{:10}: {}", "bit7", into_x_str(input[7]));

    // Find quotes. Interestingly, quote and backslash are near mirrors. It's more efficient to test
    // quote     = 00100010
    // backslash = 01011100
    use std::iter::once;
    let backslash_or_quote = !input[0] & !input[7] & input.where_bits_same(once(1).chain(once(5))) & input.where_bits_same((2..=4).chain(once(6))) & input.where_bits_different(1..=2);
    let quote = backslash_or_quote & input[1];
    println!("{:10}: {}", "quotes", into_x_str(quote));
    // If there are no quotes, AND we're not in a string, don't bother with backslashes, escapes, or
    // anything else.
    // BRANCH NOTE: Nearly all JSON files have strings. This is unlikely to trigger in the negative
    // (and therefore won't mispredict), but when it does we'll save some significant instructions
    // (CMUL particularly).
    if !quote.any() && !overflow.strings_overflow {
        return Default::default();
    }

    //
    // Find backslashed characters (including quotes).
    //
    // First, find series of backslashes that start on an even boundaries.
    //
    let backslash = backslash_or_quote & !input[1];
    println!("{:10}: {}", "backslash", into_x_str(backslash));
    let starts_backslash_series = backslash.starts_series(&mut overflow.starts_backslash_series);
    println!("starts_backslash_series:");
    println!("{:10}: {}", "", into_x_str(starts_backslash_series));
    let backslash_series_starting_on_odd_boundary = backslash.series_not_starting_with(starts_backslash_series & u64x8::EVEN_BITS, &mut overflow.backslash_series_starting_on_odd_boundary);
    println!("backslash_series_starting_on_odd_boundary:");
    println!("{:10}: {}", "", into_x_str(backslash_series_starting_on_odd_boundary));

    // Next, turn series of backslashes on even boundaries into 10101010 using XOR,
    // and series on odd boundaries the same way, to get the backslashes that actually escape
    // something.
    let escapes = (u64x8::ODD_BITS & backslash_series_starting_on_odd_boundary) |
                  (u64x8::EVEN_BITS & backslash & !backslash_series_starting_on_odd_boundary);
    println!("{:10}: {}", "escapes", into_x_str(escapes));
    // The escaped characters are the characters right after a "real" escaping backslash.
    let escaped = escapes.prev(&mut overflow.escaped);
    println!("{:10}: {}", "escaped", into_x_str(escaped));

    //
    // Find characters in strings (every other ", not counting \").
    //
    println!("{:10}: {}", "realquotes", into_x_str(quote & !escaped));
    let strings = (quote & !escaped).between_pairs(&mut overflow.strings_overflow);
    println!("{:10}: {}", "strings", into_x_str(strings));

    //
    // Check for invalid string characters (00-1F).
    //
    let invalid_string_bytes = input.where_lt(0x20) & strings; // 00-1F

    //
    // TODO ensure we don't *end* the stream in a string.
    //

    // //
    // // Find Unicode escapes (\uDDDD ranges)
    // //
    // let escaped_u = each_64(input, |i| i.where_eq(b'u'));
    // // BRANCH NOTE: this is rare enough to be worth skipping when we can
    // let (escaped_unicode_hex, invalid_escaped_unicode_hex) = if escaped_u.any() {
    //     // Best time to validate the digits is during copy.
    //     // TODO make sure that "\u","D" doesn't break the world
    //     let invalid_escaped_unicode_hex = escaped_unicode_hex & !hex_digit;
    //     (escaped_unicode_hex, invalid_escaped_unicode_hex)
    // }
    FindStrings { strings, escapes, escaped, invalid_string_bytes }
}

#[derive(Clone,Debug,Default)]
pub struct FindLiteralNames {
    ///
    /// The start of valid true literals
    /// 
    pub true_literal: u64x8,
    ///
    /// The start of valid false literals
    /// 
    pub false_literal: u64x8,
    ///
    /// The "n" in null
    /// 
    pub null_literal: u64x8,
    ///
    /// Mask of literal characters (true/false/null).
    /// 
    /// Superceded by "number" mask since this includes the "e" in valid floats (such as 1.2e8).
    ///
    pub literal_name: u64x8,
    ///
    /// Mask where previous character is *not* literal.
    /// 
    /// literal_name & (prev_not_literal_name << 1) tells you what literals are invalid.
    /// 
    pub prev_not_literal_name: u64x8,
}

#[derive(Clone,Debug,Default)]
pub struct FindLiteralNamesOverflow {
    t1: u8,
    r1: u8,
    u1: u8,
    f1: u8,
    a1: u8,
    l1: u8,
    s1: u8,
    n1: u8,
    r2: u8,
    u2: u8,
    n2: u8,
}

pub fn find_literal_names(input: &SeparatedBits, overflow: &mut FindLiteralNamesOverflow) -> FindLiteralNames {
// a         | 61    | 01100001
// e         | 65    | 01100101
// f         | 66    | 01100110
//                     011011
//                     ------
// l         | 6C    | 01101100
// n         | 6E    | 01101110
//                     01110
//                     -----
// r         | 72    | 01110010
// s         | 73    | 01110011
// t         | 74    | 01110100
// u         | 75    | 01110101
    // Grab all the characters we want to match, along with their predecessors
    let mask_a_z = input.where_bits_eq(b'a', 5..=7);
        let mask_a_n = mask_a_z & !input[4];
            let mask_a_f = mask_a_n & !input[3] & input.where_bits_different(0..=1);
                let a = mask_a_f & !input[2] & input[0];
                let mask_e_f = mask_a_f &  input[2];
                    let e = mask_e_f & input[0];
                    let f = mask_e_f & !input[0];
            let mask_l_n = mask_a_n & input[3] & input[2] & input[0];
                let l = mask_l_n &  input[1];
                let n = mask_l_n & !input[1];

        let mask_r_u = mask_a_z & input[4] & !input[3] & input.where_bits_different(1..=2);
            let mask_r_s = mask_r_u & !input[2];
                let r = mask_r_s & !input[0];
                let s = mask_r_s &  input[0];
            let mask_t_u = mask_r_u & input[2];
                let t = mask_t_u & !input[0];
                let u = mask_t_u &  input[0];

    let (t1, r1, u1, f1, a1, l1, s1, n1) = (
        t.prev(&mut overflow.t1),
        r.prev(&mut overflow.r1),
        u.prev(&mut overflow.u1),
        // e.prev(&mut overflow.e1), // not needed, only at the end of words

        f.prev(&mut overflow.f1),
        a.prev(&mut overflow.a1),
        l.prev(&mut overflow.l1),
        s.prev(&mut overflow.s1),

        n.prev(&mut overflow.n1),
    );
    let (r2, u2, n2) = (
        r.back(2, &mut overflow.r2),
        u.back(2, &mut overflow.u2),
        n.back(2, &mut overflow.n2),
    );

    // Literal characters are all the above characters.
    let literal_name = t | r | u | e |
                       f | a | l | s |
                       n;

    // Mask out places where previous literal characters *aren't* followed by the correct next character.
    // This is how we weed out partial and otherwise jumbled literals like nul, ull and ulnl
    let prev_not_literal_name = !(
        (t1 & r) |      (r1 & u) | (r2 & u1 & e) |            // t(r),  r(u), ru(e)
        (f1 & a) |      (a1 & l) |      (l1 & s) | (s1 & e) | // f(a),  a(l),  l(s), s(e)
        (n1 & u) | (n2 & u1 & l) | (u2 & l1 & l)              // n(u), nu(l), ul(l)
    );

    // nullnull and truefalse are treated as valid here, but but invalidated later because there
    // is no comma between the two values.
    FindLiteralNames { true_literal: t, false_literal: f, null_literal: n, literal_name, prev_not_literal_name }
}


#[derive(Clone,Debug,Default)]
pub struct ValidateUtf8Overflow {
    cont1: u8,
    cont2: u8,
    cont3: u8,
    prev_e0: u8,
    prev_ed: u8,
    prev_f0: u8,
    prev_f4: u8
}

///
/// Validate UTF-8.
/// 
/// Invalid UTF-8 multibyte sequences will not have *all* bytes in the sequence invalidated, but in
/// all cases at least *one* of them will be.
/// 
/// See [Wikipedia](https://en.wikipedia.org/wiki/UTF-8#Overlong_encodings) and
/// [UTF-8 Corrigendum #1](http://www.unicode.org/versions/corrigendum1.html) for details.
///
/// Returns a mask of all invalid UTF-8
pub fn validate_utf8(input: &SeparatedBits, overflow: &mut ValidateUtf8Overflow) -> u64x8 {
    //
    // Short-circuit ASCII-only blocks.
    //
    // ASCII only (0xxxxxxx) is always valid, and so common we skip validation when it's off.
    //
    if !input[7].any() {
        *overflow = Default::default();
        return Default::default();
    }

    //
    // Get masks for leading bits
    //
    let continuation = input[7] & !input[6];      // 10xxxxxx
    let lead2plus    = input[7] &  input[6];      // 11xxxxxx
    let lead2        = lead2plus & !input[5]; // 110xxxxx
    let lead3plus    = lead2plus &  input[5]; // 111xxxxx
    let lead3        = lead3plus & !input[4]; // 1110xxxx
    let lead4plus    = lead3plus &  input[4]; // 1111xxxx
    let lead4        = lead4plus & !input[3]; // 11110xxx

    //
    // Check for missing or extra continuations.
    //
    //                   110xxxxx cont1    must be leading byte
    //          1110xxxx cont1    cont2    must be leading byte
    // 11110xxx cont1    cont2    cont3    must be leading byte
    //
    // If current byte is cont1, cont2 or cont3, it MUST be a continuation (10xxxxxx).
    // If it is not cont1, cont2 *or* cont3, it must NOT be a continuation (0xxxxxxx or 11xxxxxx).
    //
    // Everything off the end of the buffer is considered == 0, and thus is a leading byte, so if
    // there is a missing continuation off to the right, it will be noted.
    //
    let cont1 = lead2plus.back(1, &mut overflow.cont1); //                   11xxxxxx <cont1>
    let cont2 = lead3plus.back(2, &mut overflow.cont2); //          111xxxxx          <cont2>
    let cont3 = lead4plus.back(3, &mut overflow.cont3); // 1111xxxx                   <cont3>
    let mut invalid = continuation ^ (cont1 | cont2 | cont3);

    //
    // Check for UTF-8 that leads to too-large Unicode codepoints (> U+10FFFF).
    //
    let prev_f4 = (        lead4 &  input[2] & !input[1] & !input[0]).prev(&mut overflow.prev_f4); // F4 = 11110100
    invalid |= prev_f4 & (cont1 & (input[6] & input[5]));   // F4 90-BF: 11110100 1001xxxx/101xxxxx
    invalid |= lead4 & input[2] & !input[0];                // F5:       11110101
    invalid |= lead4 & input[2] & input[1];                 // F6-F7:    1111011x
    invalid |= lead4plus & input[3];                    // F8-FF:    11111xxx

    //
    // Check for Unicode surrogates (U+D800-U+DFFF).
    //
    let prev_ed = (lead3 &  input[3] &  input[2] & !input[1] &  input[0]).prev(&mut overflow.prev_ed); // ED = 11101101
    invalid |= prev_ed & cont1 & input[6];                // ED A0-BF: 11101101 101xxxxx

    //
    // Check for overlong encodings (using more bytes than needed for one Unicode codepoint).
    //
    let prev_e0 = (lead3 & !input[3] & !input[2] & !input[1] & !input[0]).prev(&mut overflow.prev_e0); // ED = 11100000
    let prev_f0 = (        lead4 & !input[2] & !input[1] & !input[0]).prev(&mut overflow.prev_f0); // F0 = 11110000
    invalid |= lead2 & !input[4] & !input[3] & !input[2] & !input[1]; //    C0-C1: 1100000x
    invalid |= prev_e0 & cont1 & !input[6];               // E0 80-9F: 11100000 100xxxxx
    invalid |= prev_f0 & cont1 & !input[6] & !input[7];        // F0 80-8F: 11110000 1000xxxx

    invalid
}


// fn each_64<F: Fn(u8x64) -> u64>(input: &[u8;512], f: F) -> u64x8{
//     let mut result = u64x8::splat(0);
//     for i in 0..8 {
//         let start = i*64;
//         let input64 = u8x64::from_le(unsafe { u8x64::from_slice_unaligned_unchecked(&input[start..start+64]) });
//         result = result.replace(i, f(input64));
//     }
//     result
// }

fn iter_bits<'a, T: StreamableBitmask+'a>(mask: T) -> impl Iterator<Item=bool>+'a {
    (0..T::NUM_BITS).map(move |n| mask.get_bit(n))
}
fn into_x_str<T: StreamableBitmask>(mask: T) -> String {
    iter_bits(mask).map(|bit| if bit { 'X' } else { ' ' }).fold(String::with_capacity(T::NUM_BITS as usize), |mut s,bit| { s.push(bit); s })
}

#[cfg(test)]
mod tests {
    pub use super::*;
    use std::fmt::Debug;

    const SPACES: [u8;512] = [b' ';512];

    fn from_x_str(input: &[u8;512]) -> u64x8 {
        separate_bits(input).where_eq(b'X')
    }
    fn into_x_str<T: StreamableBitmask>(mask: T) -> String {
        iter_bits(mask).map(|bit| if bit { 'X' } else { ' ' }).fold(String::with_capacity(T::NUM_BITS as usize), |mut s,bit| { s.push(bit); s })
    }
    fn assert_bitmasks_eq(actual: u64x8, expected: u64x8, name: &str, input: &[u8;512], chunk_num: usize) {
        assert_eq!(actual, expected, "{} in chunk {} didn't match!\n{:10}: {}\n{:10}: {}\n{:10}: {}", name, chunk_num, "actual", into_x_str(actual), "expected", into_x_str(expected), "input", String::from_utf8_lossy(input));
    }
    fn all(bytes: impl AsRef<[u8]>) -> [u8;512] {
        let bytes = bytes.as_ref();
        assert!(512 % bytes.len() == 0);
        let mut result = [b' '; 512];
        let mut i = result.len();
        while i > 0 {
            result[i-bytes.len()..i].copy_from_slice(bytes);
            i -= bytes.len();
        }
        result
    }
    fn chunk(head: impl AsRef<[u8]>, tail: impl AsRef<[u8]>) -> [u8;512] {
        let head = head.as_ref();
        let tail = tail.as_ref();
        let mut result = [b' '; 512];
        result[..head.len()].copy_from_slice(head);
        result[512-tail.len()..].copy_from_slice(tail);
        result
    }
    fn head(head: impl AsRef<[u8]>) -> [u8;512] {
        let head = head.as_ref();
        let mut result = [b' '; 512];
        result[..head.len()].copy_from_slice(head);
        result
    }
    fn tail(tail: impl AsRef<[u8]>) -> [u8;512] {
        let tail = tail.as_ref();
        let mut result = [b' '; 512];
        result[512-tail.len()..].copy_from_slice(tail);
        result
    }

    trait VecManip<T> {
        fn concat<A: AsRef<[T]>>(self, other: A) -> Vec<T>;
        fn overwrite<A: AsRef<[T]>>(self, other: A) -> Vec<T>;
        fn repeat(self, n: usize) -> Vec<T>;
    }
    impl<T: Clone+Debug, S: AsRef<[T]>> VecManip<T> for S {
        fn concat<A: AsRef<[T]>>(self, other: A) -> Vec<T> {
            let a = self.as_ref();
            let b = other.as_ref();
            let mut result = Vec::with_capacity(a.len() + b.len());
            result.extend_from_slice(a);
            result.extend_from_slice(b);
            result
        }
        fn overwrite<A: AsRef<[T]>>(self, other: A) -> Vec<T> {
            let a = self.as_ref();
            let b = other.as_ref();
            assert!(b.len() < a.len());
            let mut result = Vec::with_capacity(a.len());
            result[0..b.len()].clone_from_slice(b);
            result
        }
        fn repeat(self, n: usize) -> Vec<T> {
            let a = self.as_ref();
            let mut result = Vec::with_capacity(n * a.len());
            for _ in 0..n {
                result.extend_from_slice(a);
            }
            result
        }
    }

    mod find_strings {
        use super::*;

        struct TestFindStrings {
            input: Vec<[u8;512]>,
            strings: Vec<[u8;512]>,
            escapes: Vec<[u8;512]>,
            escaped: Vec<[u8;512]>,
            invalid_string_bytes: Vec<[u8;512]>,
        }
        impl TestFindStrings {
            fn test(&self) {
                let num_chunks = self.input.len().max(self.strings.len()).max(self.escapes.len()).max(self.escaped.len()).max(self.invalid_string_bytes.len());
                let expected = (0..num_chunks).map(|i| {
                    let strings = from_x_str(self.strings.get(i).unwrap_or(&SPACES));
                    let escapes = from_x_str(self.escapes.get(i).unwrap_or(&SPACES));
                    let escaped = from_x_str(self.escaped.get(i).unwrap_or(&SPACES));
                    let invalid_string_bytes = from_x_str(self.invalid_string_bytes.get(i).unwrap_or(&SPACES));
                    FindStrings { strings, escapes, escaped, invalid_string_bytes }
                });
                let iter_input = || (0..num_chunks).map(|i| self.input.get(i).unwrap_or(&SPACES));
                let actual = iter_input().scan(Default::default(), |overflow,input| {
                    println!("{:10}: {}", "input", String::from_utf8_lossy(input));
                    Some(find_strings(&separate_bits(input), overflow))
                });
                for (i, ((input, actual), expected)) in iter_input().zip(actual).zip(expected).enumerate() {
                    assert_bitmasks_eq(actual.strings, expected.strings, "strings", &input, i);
                    assert_bitmasks_eq(actual.escapes, expected.escapes, "escapes", &input, i);
                    assert_bitmasks_eq(actual.escaped, expected.escaped, "escaped", &input, i);
                    assert_bitmasks_eq(actual.invalid_string_bytes, expected.invalid_string_bytes, "invalid_string_bytes", &input, i);
                }
            }
        }

        #[test]
        fn no_strings() {
            TestFindStrings {
                input:                vec![ all(br#" "#), all(br#" "#) ],
                strings:              vec![ all(br#" "#), all(br#" "#) ],
                escapes:              vec![],
                escaped:              vec![],
                invalid_string_bytes: vec![],
            }.test()
        }
        #[test]
        fn all_empty_strings() {
            TestFindStrings {
                input:                vec![ all(br#""""#), all(br#""""#) ],
                strings:              vec![ all(br#"X "#), all(br#"X "#) ],
                escapes:              vec![],
                escaped:              vec![],
                invalid_string_bytes: vec![],
            }.test()
        }
        #[test]
        fn many_strings() {
            TestFindStrings {
                input:                vec![ head((0..100).fold(Vec::<u8>::new(), |mut v, n| { if v.len() + n + 2 < 512 { v.push(b'"'); v.append(&mut b" ".repeat(n)); v.push(b'"'); }; v })) ],
                strings:              vec![ head((0..100).fold(Vec::<u8>::new(), |mut v, n| { if v.len() + n + 2 < 512 { v.push(b'X'); v.append(&mut b"X".repeat(n)); v.push(b' '); }; v })) ],
                escapes:              vec![],
                escaped:              vec![],
                invalid_string_bytes: vec![],
            }.test()
        }
        #[test]
        fn many_strings_with_commas() {
            TestFindStrings {
                input:                vec![ head((0..100).fold(Vec::<u8>::new(), |mut v, n| { if v.len() + n + 3 < 512 { v.push(b'"'); v.append(&mut b" ".repeat(n)); v.push(b'"'); v.push(b',')}; v })) ],
                strings:              vec![ head((0..100).fold(Vec::<u8>::new(), |mut v, n| { if v.len() + n + 3 < 512 { v.push(b'X'); v.append(&mut b"X".repeat(n)); v.push(b' '); v.push(b' ')}; v })) ],
                escapes:              vec![],
                escaped:              vec![],
                invalid_string_bytes: vec![],
            }.test()
        }
        #[test]
        fn one_string_510() {
            TestFindStrings {
                input:                vec![ head(br#"""#.concat(br#"X"#.repeat(510)).concat(br#"""#)) ],
                strings:              vec![ head(br#"X"#.concat(br#"X"#.repeat(510)).concat(br#" "#)) ],
                escapes:              vec![],
                escaped:              vec![],
                invalid_string_bytes: vec![],
            }.test()
        }
        #[test]
        fn one_string_509() {
            TestFindStrings {
                input:                vec![ head(br#"""#.concat(br#"X"#.repeat(509)).concat(br#"""#)) ],
                strings:              vec![ head(br#"X"#.concat(br#"X"#.repeat(509)).concat(br#" "#)) ],
                escapes:              vec![],
                escaped:              vec![],
                invalid_string_bytes: vec![],
            }.test()
        }
        #[test]
        fn one_string_509_alternate() {
            TestFindStrings {
                input:                vec![ tail(br#"""#.concat(br#"X"#.repeat(509)).concat(br#"""#)) ],
                strings:              vec![ tail(br#"X"#.concat(br#"X"#.repeat(509)).concat(br#" "#)) ],
                escapes:              vec![],
                escaped:              vec![],
                invalid_string_bytes: vec![],
            }.test()
        }

        #[test]
        fn string_across_boundaries() {
            TestFindStrings {
                input:                vec![ tail(br#"""#), head(br#"""#) ],
                strings:              vec![ tail(br#"X"#), head(br#" "#) ],
                escapes:              vec![],
                escaped:              vec![],
                invalid_string_bytes: vec![],
            }.test()
        }
        #[test]
        fn string_across_boundaries_multiple_chunks() {
            TestFindStrings {
                input:                vec![ tail(br#"""#), all(br#" "#), all(br#" "#), head(br#"""#) ],
                strings:              vec![ tail(br#"X"#), all(br#"X"#), all(br#"X"#), head(br#" "#) ],
                escapes:              vec![],
                escaped:              vec![],
                invalid_string_bytes: vec![],
            }.test()
        }
        #[test]
        fn string_across_boundaries_long() {
            TestFindStrings {
                input:                vec![ head(br#"""#), chunk(br#" "#.repeat(511),br#"""#) ],
                strings:              vec![  all(br#"X"#), chunk(br#"X"#.repeat(511),br#" "#) ],
                escapes:              vec![],
                escaped:              vec![],
                invalid_string_bytes: vec![],
            }.test()
        }
        #[test]
        fn string_across_boundaries_multiple_chunks_long() {
            TestFindStrings {
                input:                vec![ head(br#"""#), all(br#" "#), all(br#" "#), chunk(br#" "#.repeat(511),br#"""#) ],
                strings:              vec![  all(br#"X"#), all(br#"X"#), all(br#"X"#), chunk(br#"X"#.repeat(511),br#" "#) ],
                escapes:              vec![],
                escaped:              vec![],
                invalid_string_bytes: vec![],
            }.test()
        }
        #[test]
        fn escaped_quote() {
            TestFindStrings {
                input:                vec![ head(br#""\"""#) ],
                strings:              vec![ head(br#"XXX "#) ],
                escapes:              vec![ head(br#" X  "#) ],
                escaped:              vec![ head(br#"  X "#) ],
                invalid_string_bytes: vec![],
            }.test()
        }


        #[test]
        fn no_backslashes() {
            TestFindStrings {
                input:       vec![ tail(br#"""#), all(br#" "#), head(br#"""#) ],
                strings:     vec![ tail(br#"X"#), all(br#"X"#), head(br#" "#) ],
                escapes:     vec![],
                escaped:     vec![],
                invalid_string_bytes: vec![],
            }.test()
        }
        #[test]
        fn no_backslashes_first_backslashed() {
            TestFindStrings {
                input:       vec![ tail(br#""\"#), chunk(br#" "#,br#"""#) ],
                escapes:     vec![ tail(br#" X"#), chunk(br#" "#,br#" "#) ],
                escaped:     vec![ tail(br#"  "#), chunk(br#"X"#,br#" "#) ],
                strings:     vec![ tail(br#"XX"#), chunk(br#"X"#.repeat(511),br#" "#) ],
                invalid_string_bytes: vec![],
            }.test()
        }
        #[test]
        fn all_backslashes() {
            TestFindStrings {
                input:       vec![ tail(br#"""#), all(br#"\\"#) ],
                escapes:     vec![ tail(br#" "#), all(br#"X "#) ],
                escaped:     vec![ tail(br#" "#), all(br#" X"#) ],
                strings:     vec![ tail(br#"X"#), all(br#"XX"#) ],
                invalid_string_bytes: vec![],
            }.test()
        }
        #[test]
        fn all_backslashes_first_backslashed() {
            TestFindStrings {
                input:       vec![ tail(br#""\"#), all(br#"\\"#), head(br#""""#) ],
                escapes:     vec![ tail(br#" X"#), all(br#" X"#), head(br#"  "#) ],
                escaped:     vec![ tail(br#"  "#), all(br#"X "#), head(br#"X "#) ],
                strings:     vec![ tail(br#"XX"#), all(br#"XX"#), head(br#"X "#) ],
                invalid_string_bytes: vec![],
            }.test()
        }
        #[test]
        fn all_backslashes_first_backslashed_multiple() {
            TestFindStrings {
                input:       vec![ tail(br#""\"#), all(br#"\\"#), all(br#"\\"#), head(br#""""#) ],
                escapes:     vec![ tail(br#" X"#), all(br#" X"#), all(br#" X"#), head(br#"  "#) ],
                escaped:     vec![ tail(br#"  "#), all(br#"X "#), all(br#"X "#), head(br#"X "#) ],
                strings:     vec![ tail(br#"XX"#), all(br#"XX"#), all(br#"XX"#), head(br#"X "#) ],
                invalid_string_bytes: vec![],
            }.test()
        }
        #[test]
        fn all_backslashes_first_backslashed_multiple_multiple() {
            TestFindStrings  {
                input:       vec![ tail(br#""\"#), all(br#"\\"#), all(br#"\\"#), chunk(br#" "#,br#"\\\"#), all(br#"\\"#), all(br#"\\"#), all(br#"\\"#), head(br#""""#) ],
                escapes:     vec![ tail(br#" X"#), all(br#" X"#), all(br#" X"#), chunk(br#" "#,br#"X X"#), all(br#" X"#), all(br#" X"#), all(br#" X"#), head(br#"  "#) ],
                escaped:     vec![ tail(br#"  "#), all(br#"X "#), all(br#"X "#), chunk(br#"X"#,br#" X "#), all(br#"X "#), all(br#"X "#), all(br#"X "#), head(br#"X "#) ],
                strings:     vec![ tail(br#"XX"#), all(br#"XX"#), all(br#"XX"#),  all(br#"XX"#),  all(br#"XX"#), all(br#"XX"#), all(br#"XX"#), head(br#"X "#) ],
                invalid_string_bytes: vec![],
            }.test()
        }
        #[test]
        fn four_backslashes() {
            TestFindStrings {
                input:       vec![ tail(br#"""#), head(br#"\\\\""#) ],
                escapes:     vec![ tail(br#" "#), head(br#"X X  "#) ],
                escaped:     vec![ tail(br#" "#), head(br#" X X "#) ],
                strings:     vec![ tail(br#"X"#), head(br#"XXXX "#) ],
                invalid_string_bytes: vec![],
            }.test()
        }
        #[test]
        fn four_backslashes_first_backslashed() {
            TestFindStrings {
                input:       vec![ tail(br#""\"#), head(br#"\\\\"""#) ],
                escapes:     vec![ tail(br#" X"#), head(br#" X X  "#) ],
                escaped:     vec![ tail(br#"  "#), head(br#"X X X "#) ],
                strings:     vec![ tail(br#"XX"#), head(br#"XXXXX "#) ],
                invalid_string_bytes: vec![],
            }.test()
        }
        #[test]
        fn five_backslashes() {
            TestFindStrings {
                input:       vec![ tail(br#"""#), head(br#"\\\\\"""#) ],
                escapes:     vec![ tail(br#" "#), head(br#"X X X  "#) ],
                escaped:     vec![ tail(br#" "#), head(br#" X X X "#) ],
                strings:     vec![ tail(br#"X"#), head(br#"XXXXXX "#) ],
                invalid_string_bytes: vec![],
            }.test()
        }
        #[test]
        fn five_backslashes_first_backslashed() {
            TestFindStrings {
                input:       vec![ tail(br#""\"#), head(br#"\\\\\""#) ],
                escapes:     vec![ tail(br#" X"#), head(br#" X X  "#) ],
                escaped:     vec![ tail(br#"  "#), head(br#"X X X "#) ],
                strings:     vec![ tail(br#"XX"#), head(br#"XXXXX "#) ],
                invalid_string_bytes: vec![],
            }.test()
        }
        #[test]
        fn almost_all_backslashes() {
            TestFindStrings {
                input:       vec![ tail(br#"""#), chunk(br#"\\"#.repeat((512-2)/2),br#"\""#), head(br#"""#) ],
                escapes:     vec![ tail(br#" "#), chunk(br#"X "#.repeat((512-2)/2),br#"X "#), head(br#" "#) ],
                escaped:     vec![ tail(br#" "#), chunk(br#" X"#.repeat((512-2)/2),br#" X"#), head(br#" "#) ],
                strings:     vec![ tail(br#"X"#), chunk(br#"XX"#.repeat((512-2)/2),br#"XX"#), head(br#" "#) ],
                invalid_string_bytes: vec![],
            }.test()
        }
        #[test]
        fn almost_all_backslashes_first_backslashed() {
            TestFindStrings {
                input:       vec![ tail(br#""\"#), chunk(br#"\\"#.repeat((512-2)/2),br#"\""#) ],
                escapes:     vec![ tail(br#" X"#), chunk(br#" X"#.repeat((512-2)/2),br#"  "#) ],
                escaped:     vec![ tail(br#"  "#), chunk(br#"X "#.repeat((512-2)/2),br#"X "#) ],
                strings:     vec![ tail(br#"XX"#), chunk(br#"XX"#.repeat((512-2)/2),br#"X "#) ],
                invalid_string_bytes: vec![],
            }.test()
        }
        #[test]
        fn almost_all_backslashes_alternate() {
            TestFindStrings {
                input:       vec![ tail(br#"""#), chunk(br#" "#,br#"\\"#.repeat((512-2)/2).concat(br#"\"#)), head(br#""""#) ],
                escapes:     vec![ tail(br#" "#), chunk(br#" "#,br#"X "#.repeat((512-2)/2).concat(br#"X"#)), head(br#"  "#) ],
                escaped:     vec![ tail(br#" "#), chunk(br#" "#,br#" X"#.repeat((512-2)/2).concat(br#" "#)), head(br#"X "#) ],
                strings:     vec![ tail(br#"X"#), chunk(br#"X"#,br#"XX"#.repeat((512-2)/2).concat(br#"X"#)), head(br#"X "#) ],
                invalid_string_bytes: vec![],
            }.test()
        }
        #[test]
        fn almost_all_backslashes_alternate_first_backslashed() {
            TestFindStrings {
                input:       vec![ tail(br#""\"#), chunk(br#" "#,br#"\\"#.repeat((512-2)/2).concat(br#"\"#)), head(br#""""#) ],
                escapes:     vec![ tail(br#" X"#), chunk(br#" "#,br#"X "#.repeat((512-2)/2).concat(br#"X"#)), head(br#"  "#) ],
                escaped:     vec![ tail(br#"  "#), chunk(br#"X"#,br#" X"#.repeat((512-2)/2).concat(br#" "#)), head(br#"X "#) ],
                strings:     vec![ tail(br#"XX"#), chunk(br#"X"#,br#"XX"#.repeat((512-2)/2).concat(br#"X"#)), head(br#"X "#) ],
                invalid_string_bytes: vec![],
            }.test()
        }
        #[test]
        fn many_different_backslashes() {
            TestFindStrings {
                input:       vec![ tail(br#"""#), head(br#"\ \\ \\\ \\\\ \\\\\ \\\\\\ \\\\\\\ \\\\\\\\ \\\\\\\\\ \\\\\\\\\\""#.repeat(1)) ],
                escapes:     vec![ tail(br#" "#), head(br#"X X  X X X X  X X X X X X  X X X X X X X X  X X X X X X X X X X  "#.repeat(1)) ],
                escaped:     vec![ tail(br#" "#), head(br#" X X  X X X X  X X X X X X  X X X X X X X X  X X X X X X X X X X "#.repeat(1)) ],
                strings:     vec![ tail(br#"X"#), head(br#"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX "#.repeat(1)) ],
                invalid_string_bytes: vec![],
            }.test()
        }
        #[test]
        fn many_different_backslashes_first_backslashed() {
            TestFindStrings {
                input:       vec![ tail(br#""\"#), head(br#"\ \\ \\\ \\\\ \\\\\ \\\\\\ \\\\\\\ \\\\\\\\ \\\\\\\\\ \\\\\\\\\\""#.repeat(1)) ],
                escapes:     vec![ tail(br#" X"#), head(br#"  X  X X X X  X X X X X X  X X X X X X X X  X X X X X X X X X X  "#.repeat(1)) ],
                escaped:     vec![ tail(br#"  "#), head(br#"X  X  X X X X  X X X X X X  X X X X X X X X  X X X X X X X X X X "#.repeat(1)) ],
                strings:     vec![ tail(br#"XX"#), head(br#"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX "#.repeat(1)) ],
                invalid_string_bytes: vec![],
            }.test()
        }

        #[test]
        fn every_other_backslash() {
            TestFindStrings {
                input:       vec![ tail(br#"""#),  all(br#"\ "#), head(br#"""#) ],
                escapes:     vec![ tail(br#" "#),  all(br#"X "#), head(br#" "#) ],
                escaped:     vec![ tail(br#" "#),  all(br#" X"#), head(br#" "#) ],
                strings:     vec![ tail(br#"X"#),  all(br#"XX"#), head(br#" "#) ],
                invalid_string_bytes: vec![],
            }.test()
        }
        #[test]
        fn every_other_backslash_first_backslashed() {
            TestFindStrings {
                input:       vec![ tail(br#""\"#), chunk(br#"\ "#,br#"\ "#.repeat((512-2)/2)), head(br#"""#) ],
                escapes:     vec![ tail(br#" X"#), chunk(br#"  "#,br#"X "#.repeat((512-2)/2)), head(br#" "#) ],
                escaped:     vec![ tail(br#"  "#), chunk(br#"X "#,br#" X"#.repeat((512-2)/2)), head(br#" "#) ],
                strings:     vec![ tail(br#"XX"#), chunk(br#"XX"#,br#"XX"#.repeat((512-2)/2)), head(br#" "#) ],
                invalid_string_bytes: vec![],
            }.test()
        }

        #[test]
        fn every_other_backslash_alternate() {
            TestFindStrings {
                input:       vec![ tail(br#"""#), chunk(br#" \"#,br#" \"#.repeat((512-2)/2)), head(br#""""#) ],
                escapes:     vec![ tail(br#" "#), chunk(br#" X"#,br#" X"#.repeat((512-2)/2)), head(br#"  "#) ],
                escaped:     vec![ tail(br#" "#), chunk(br#"  "#,br#"X "#.repeat((512-2)/2)), head(br#"X "#) ],
                strings:     vec![ tail(br#"X"#), chunk(br#"XX"#,br#"XX"#.repeat((512-2)/2)), head(br#"X "#) ],
                invalid_string_bytes: vec![],
            }.test()
        }
        #[test]
        fn every_other_backslash_alternate_first_backslashed() {
            TestFindStrings {
                input:       vec![ tail(br#""\"#), chunk(br#" \"#,br#" \"#.repeat((512-2)/2)), head(br#""""#) ],
                escapes:     vec![ tail(br#" X"#), chunk(br#" X"#,br#" X"#.repeat((512-2)/2)), head(br#"  "#) ],
                escaped:     vec![ tail(br#"  "#), chunk(br#"X "#,br#"X "#.repeat((512-2)/2)), head(br#"X "#) ],
                strings:     vec![ tail(br#"XX"#), chunk(br#"XX"#,br#"XX"#.repeat((512-2)/2)), head(br#"X "#) ],
                invalid_string_bytes: vec![],
            }.test()
        }
        #[test]
        fn every_other_2_backslash() {
            TestFindStrings {
                input:       vec![ tail(br#"""#),  head(br#"\\ "#.concat(br#"\\ "#.repeat((512-4)/3)).concat(br#"""#)) ],
                escapes:     vec![ tail(br#" "#),  head(br#"X  "#.concat(br#"X  "#.repeat((512-4)/3)).concat(br#" "#)) ],
                escaped:     vec![ tail(br#" "#),  head(br#" X "#.concat(br#" X "#.repeat((512-4)/3)).concat(br#" "#)) ],
                strings:     vec![ tail(br#"X"#),  head(br#"XXX"#.concat(br#"XXX"#.repeat((512-4)/3)).concat(br#" "#)) ],
                invalid_string_bytes: vec![],
            }.test()
        }
        #[test]
        fn every_other_2_backslash_first_backslashed() {
            TestFindStrings {
                input:       vec![ tail(br#""\"#),  head(br#"\\ "#.concat(br#"\\ "#.repeat((512-4)/3)).concat(br#"""#)) ],
                escapes:     vec![ tail(br#" X"#),  head(br#" X "#.concat(br#"X  "#.repeat((512-4)/3)).concat(br#" "#)) ],
                escaped:     vec![ tail(br#"  "#),  head(br#"X X"#.concat(br#" X "#.repeat((512-4)/3)).concat(br#" "#)) ],
                strings:     vec![ tail(br#"XX"#),  head(br#"XXX"#.concat(br#"XXX"#.repeat((512-4)/3)).concat(br#" "#)) ],
                invalid_string_bytes: vec![],
            }.test()
        }
        #[test]
        fn every_other_2_backslash_alternate() {
            TestFindStrings {
                input:       vec![ tail(br#"""#),  head(br#" \\"#.concat(br#" \\"#.repeat((512-4)/3)).concat(br#"""#)) ],
                escapes:     vec![ tail(br#" "#),  head(br#" X "#.concat(br#" X "#.repeat((512-4)/3)).concat(br#" "#)) ],
                escaped:     vec![ tail(br#" "#),  head(br#"  X"#.concat(br#"  X"#.repeat((512-4)/3)).concat(br#" "#)) ],
                strings:     vec![ tail(br#"X"#),  head(br#"XXX"#.concat(br#"XXX"#.repeat((512-4)/3)).concat(br#" "#)) ],
                invalid_string_bytes: vec![],
            }.test()
        }
        #[test]
        fn every_other_2_backslash_alternate_first_backslashed() {
            TestFindStrings {
                input:       vec![ tail(br#""\"#),  head(br#" \\"#.concat(br#" \\"#.repeat((512-4)/3)).concat(br#"""#)) ],
                escapes:     vec![ tail(br#" X"#),  head(br#" X "#.concat(br#" X "#.repeat((512-4)/3)).concat(br#" "#)) ],
                escaped:     vec![ tail(br#"  "#),  head(br#"X X"#.concat(br#"  X"#.repeat((512-4)/3)).concat(br#" "#)) ],
                strings:     vec![ tail(br#"XX"#),  head(br#"XXX"#.concat(br#"XXX"#.repeat((512-4)/3)).concat(br#" "#)) ],
                invalid_string_bytes: vec![],
            }.test()
        }
    }
}
