use packed_simd::*;
use crate::streamable_bitmask::*;
use crate::separated_bits::*;
use crate::maskable::*;

pub struct JsonParser {
    result: JsonResult,
    validate_utf8_overflow: ValidateUtf8Overflow,
    find_strings_overflow: FindStringsOverflow,
    find_literal_names_overflow: FindLiteralNamesOverflow,
}

pub struct JsonResult {
    /// The buffers (we modify strings and numbers in place). This can be stripped away
    modified_json: Vec<[u8;512]>,
    // /// Errors that occurred during parsing
    // errors: Vec<JsonParseError>,
    // /// The index of every line start, for error reporting
    // line_starts: Vec<usize>,
    // /// Strings, numbers, booleans, and nulls, in order
    // atomic_values: Vec<AtomicValueType>,
    // /// For strings and numbers, indices to their content within the buffer
    // atomic_value_indices: Vec<usize>,
    // /// The length of strings and numbers in the buffer
    // atomic_value_lengths: Vec<usize>,
}

#[derive(Debug)]
pub enum AtomicValueType {
    String,
    Number,
    True,
    False,
    Null,
}

// pub fn parse(read: Read) -> JsonResult {

// }
pub fn parse_chunks(input: Vec<[u8;512]>) -> JsonResult {
    JsonParser::from_chunks(input).parse_existing_chunks()
}

impl JsonParser {
    pub(crate) fn from_chunks(input: Vec<[u8;512]>) -> Self {
        JsonParser {
            result: JsonResult {
                modified_json: input,
                // line_starts: Default::default(),
                // atomic_values: Default::default(),
                // atomic_value_indices: Default::default(),
                // atomic_value_lengths: Default::default(),
            },
            validate_utf8_overflow: Default::default(),
            find_strings_overflow: Default::default(),
            find_literal_names_overflow: Default::default(),
        }
    }
    fn parse_existing_chunks(mut self) -> JsonResult {
        for i in 0..self.result.modified_json.len() {
            self.parse_existing_chunk(i);
        }
        self.result
    }
    ///
    /// Parse a chunk that's already in the JsonResult.
    /// 
    /// Used by methods like parse_buffer or parse_string that take a whole buffer in before
    /// parsing.
    /// 
    fn parse_existing_chunk(&mut self, chunk: usize) -> ParsedChunk {
        let bits = separate_bits(&self.result.modified_json[chunk]);
        println!("{:10}: {}", "bit0", into_x_str(bits[0]));
        println!("{:10}: {}", "bit1", into_x_str(bits[1]));
        println!("{:10}: {}", "bit2", into_x_str(bits[2]));
        println!("{:10}: {}", "bit3", into_x_str(bits[3]));
        println!("{:10}: {}", "bit4", into_x_str(bits[4]));
        println!("{:10}: {}", "bit5", into_x_str(bits[5]));
        println!("{:10}: {}", "bit6", into_x_str(bits[6]));
        println!("{:10}: {}", "bit7", into_x_str(bits[7]));
        let invalid_utf8 = validate_utf8(&mut self.validate_utf8_overflow, &bits);
        let FindStrings { strings, escapes, escaped, invalid_string_bytes } = self.find_strings(&bits);
        let FindLiteralNames { true_literal, false_literal, null_literal, literal_name, prev_not_literal_name } = find_literal_names(&mut self.find_literal_names_overflow, &bits);

        ParsedChunk { escapes, escaped, strings, invalid_utf8, invalid_string_bytes, true_literal, false_literal, null_literal, literal_name, prev_not_literal_name }
    }

    fn find_escapes(&mut self, input: &SeparatedBits) -> (u64x8, u64x8) {
        //
        // Find backslashed characters (including quotes).
        //
        // First, find series of backslashes that start on an even boundaries.
        //
        let backslash = input.where_eq(b'\\');
        println!("{:10}: {}", "backslash", into_x_str(backslash));
        let starts_backslash_series = backslash.starts_series(&mut self.find_strings_overflow.starts_backslash_series);
        let backslash_series_starting_on_odd_boundary = backslash.series_not_starting_with(starts_backslash_series & u64x8::EVEN_BITS, &mut self.find_strings_overflow.backslash_series_starting_on_odd_boundary);

        // Next, turn series of backslashes on even boundaries into 10101010 using XOR,
        // and series on odd boundaries the same way, to get the backslashes that actually escape
        // something.
        let escapes = (u64x8::ODD_BITS & backslash_series_starting_on_odd_boundary) |
                    (u64x8::EVEN_BITS & backslash & !backslash_series_starting_on_odd_boundary);
        // The escaped characters are the characters right after a "real" escaping backslash.
        let escaped = escapes.prev(&mut self.find_strings_overflow.escaped);
        println!("{:10}: {}", "escapes", into_x_str(escapes));
        println!("{:10}: {}", "escaped", into_x_str(escaped));
        (escapes, escaped)
    }

    ///
    /// Find strings and string parts.
    /// 
    pub(crate) fn find_strings(&mut self, input: &SeparatedBits) -> FindStrings {
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
        if !quote.any() && !self.find_strings_overflow.strings {
            return Default::default();
        }

        // Figure out which characters are escaped
        let (escapes, escaped) = self.find_escapes(input);

        // Find characters in strings (every other ", not counting \").
        let strings = (quote & !escaped).between_pairs(&mut self.find_strings_overflow.strings);

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
    strings: bool,
    // escaped_unicode_hex_1: u8,
    // escaped_unicode_hex_2: u8,
    // escaped_unicode_hex_3: u8,
    // escaped_unicode_hex_4: u8,
}

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

//fn find_hashes_and_arrays(input: &SeparatedBits, overflow: &mut FindStringsOverflow) -> FindStrings {
    // Validate structure
    // Ensure : is preceded by space and then "
    // Ensure "": is preceded by space and then { or ,
    // Ensure values, [ and { are preceded by space and then { [ : or ,
    // Ensure ] and } are preceded by a value, ], }, [ or {
//}

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

pub fn find_literal_names(overflow: &mut FindLiteralNamesOverflow, input: &SeparatedBits) -> FindLiteralNames {
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
pub fn validate_utf8(overflow: &mut ValidateUtf8Overflow, input: &SeparatedBits) -> u64x8 {
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

// Stuff to help with debug prints and tests
pub(crate) fn iter_bits<'a, T: StreamableBitmask+'a>(mask: T) -> impl Iterator<Item=bool>+'a {
    (0..T::NUM_BITS).map(move |n| mask.get_bit(n))
}
pub(crate) fn into_x_str<T: StreamableBitmask>(mask: T) -> String {
    iter_bits(mask).map(|bit| if bit { 'X' } else { ' ' }).fold(String::with_capacity(T::NUM_BITS as usize), |mut s,bit| { s.push(bit); s })
}
