use packed_simd::*;
use super::result::*;
use crate::bitmask::streamable_bitmask::*;
use crate::bitmask::separated_bits::*;
use crate::bitmask::maskable::*;
use JsonErrorKind::*;

#[derive(Debug,Default)]
pub struct JsonParser {
    utf8_validator: Utf8Validator,
    string_parser: JsonStringParser,
    literal_names_parser: JsonLiteralNamesParser,
    number_parser: JsonNumberParser,
}

///
/// UTF-8 validator.
/// 
/// All state is there to hold overflow between chunks.
///
#[derive(Debug,Default)]
pub struct Utf8Validator {
    cont1: u8,
    cont2: u8,
    cont3: u8,
    prev_e0: bool,
    prev_ed: bool,
    prev_f0: bool,
    prev_f4: bool
}

///
/// String parser.
/// 
/// All state is there to hold overflow between chunks.
/// 
#[derive(Debug,Default)]
pub struct JsonStringParser {
    ///
    /// True if the next byte has a backslash in front of it (might not be escaped, though).
    /// 
    backslashed: bool,
    ///
    /// True if the last byte was in a backslash series starting on an odd boundary.
    /// 
    backslash_series_starting_on_odd_boundary: bool,
    ///
    /// True if the next byte will be escaped.
    /// 
    escaped: bool,
    ///
    /// True if we are still in a unclosed string.
    ///
    in_string: bool,
    ///
    /// State for escape parsing. Mostly used to parse \uXXXX
    ///
    json_string_escape_parser: JsonStringEscapeParser,
}

///
/// String escape parser.
/// 
/// Used by `JsonStringParser`.
/// 
/// All state is there to hold overflow between chunks, and all of that is for \uXXXX.
/// 
#[derive(Debug,Default)]
struct JsonStringEscapeParser {
    ///
    /// True if the previous ascii byte was 8-F (1xxx).
    /// 
    prev_hex3: bool,
    ///
    /// True if the previous ascii byte was 4-7 or B-F (x1xx).
    /// 
    prev_hex2: bool,
    ///
    /// True if the previous ascii byte was 2-3, 6-7, A-B or E-F (xx1x).
    /// 
    prev_hex1: bool,
    ///
    /// True if the previous ascii byte was even (xxx1).
    /// 
    prev_hex0: bool,
    ///
    /// True if the next byte is just after \u (\uXxxx).
    /// 
    u1: bool,
    ///
    /// True if the next byte is the second byte after \u (\uxXxx).
    /// 
    u2: bool,
    ///
    /// True if the next byte is the third byte after \u (\uxxXx).
    /// 
    u3: bool,
    ///
    /// True if the next byte is the fourth byte after \u (\uxxxX).
    /// 
    u4: bool,
    ///
    /// True if the previous byte was u2 and we are in a three-byte Unicode character.
    /// 
    u3_cont: bool,
    ///
    /// True if the previous byte was u3 and we are in a one-byte Unicode character.
    /// 
    u4_lead: bool,
}

#[derive(Debug,Default)]
pub struct JsonLiteralNamesParser {
    ///
    /// True if the last character was t.
    /// 
    t1: bool,
    ///
    /// True if the last character was r.
    /// 
    r1: bool,
    ///
    /// True if the last character was u.
    /// 
    u1: bool,
    ///
    /// True if the last character was f.
    /// 
    f1: bool,
    ///
    /// True if the last character was a.
    /// 
    a1: bool,
    ///
    /// True if the last character was l.
    /// 
    l1: bool,
    ///
    /// True if the last character was s.
    /// 
    s1: bool,
    ///
    /// True if the last character was n.
    /// 
    n1: bool,
    ///
    /// True if there was an "r" 2 characters ago.
    /// 
    r2: u8,
    ///
    /// True if there was a "u" 2 characters ago.
    /// 
    u2: u8,
    ///
    /// True if there was an "n" 2 characters ago.
    /// 
    n2: u8,
}

#[derive(Debug,Default)]
pub struct JsonNumberParser {

}

pub struct JsonChunk<'a> {
    pub(crate) result: JsonResult,
    pub(crate) input: &'a mut [u8;512],
    bits: SeparatedBits,
    chunk_index: usize,
}

impl JsonParser {
    pub fn parse_chunks(input: &mut Vec<[u8;512]>) -> JsonResult {
        let mut parser = JsonParser::default();
        let mut result = JsonResult::default();
        for chunk_index in 0..input.len() {
            result = parser.parse(result, &mut input[chunk_index], chunk_index);
        }
        parser.finish(result)
    }
    pub fn parse(&mut self, result: JsonResult, input: &mut [u8;512], chunk_index: usize) -> JsonResult {
        let mut chunk = JsonChunk::new(result, input, chunk_index);
        self.utf8_validator.validate(&mut chunk);
        self.string_parser.parse(&mut chunk);
        self.literal_names_parser.parse(&mut chunk);
        self.number_parser.parse(&mut chunk);
        chunk.finish()
    }
    pub fn finish(mut self, mut result: JsonResult) -> JsonResult {
        self.string_parser.finish(&mut result);
        result
    }
}

impl<'a> JsonChunk<'a> {
    pub(crate) fn new(mut result: JsonResult, input: &'a mut [u8;512], chunk_index: usize) -> Self {
        println!("{:10}: {}", "input", String::from_utf8_lossy(input));
        let bits = separate_bits(input);
        println!("{:10}: {}", "bit7", into_x_str(bits[7]));
        println!("{:10}: {}", "bit6", into_x_str(bits[6]));
        println!("{:10}: {}", "bit5", into_x_str(bits[5]));
        println!("{:10}: {}", "bit4", into_x_str(bits[4]));
        println!("{:10}: {}", "bit3", into_x_str(bits[3]));
        println!("{:10}: {}", "bit2", into_x_str(bits[2]));
        println!("{:10}: {}", "bit1", into_x_str(bits[1]));
        println!("{:10}: {}", "bit0", into_x_str(bits[0]));
        result.len += 512;
        JsonChunk { input, result, bits, chunk_index }
    }

    pub(crate) fn finish(self) -> JsonResult {
        let JsonChunk { input, result, bits, .. } = self;
        bits.reconstruct_bytes(input);
        println!("{:10}: {}", "input", String::from_utf8_lossy(input));
        result
    }

    fn report_errors(&mut self, kind: JsonErrorKind, error_mask: u64x8) -> u64x8 {
        // BRANCH NOTE: JSON errors happen rarely in normal parsing situations, making this a noop branch.
        // #[cold] on the report_error() function is how we tag that; putting it in the else clause
        // is meant to further reinforce it, since many compilers (I dunno if this one does) treat the
        // if as more likely than the else, by default.
        if !error_mask.any() {
        } else {
            report_error(&mut self.result, JsonError { kind, chunk_index: self.chunk_index, error_mask });
        }
        error_mask
    }
}

fn report_errors_after(result: &mut JsonResult, kind: JsonErrorKind, error_mask: u64x8) -> u64x8 {
    // BRANCH NOTE: JSON errors happen rarely in normal parsing situations, making this a noop branch.
    // #[cold] on the report_error() function is how we tag that; putting it in the else clause
    // is meant to further reinforce it, since many compilers (I dunno if this one does) treat the
    // if as more likely than the else, by default.
    if !error_mask.any() {
    } else {
        println!("ERROR AFTER: {}", kind);
        println!("{:10}: {}", "error_mask", into_x_str(error_mask));
        report_error_after(result, kind, error_mask);
    }
    error_mask
}

#[cold]
fn report_error_after(result: &mut JsonResult, kind: JsonErrorKind, error_mask: u64x8) -> u64x8 {
    let chunk_index = result.len / 512;
    let start_index = result.len % 512;
    let first_error_mask = error_mask >> start_index as u32;
    if first_error_mask.any() {
        result.errors.push(JsonError { kind, chunk_index, error_mask: first_error_mask });
    }
    let next_error_mask = error_mask << (512 - start_index) as u32;
    if next_error_mask.any() {
        result.errors.push(JsonError { kind, chunk_index: chunk_index + 1, error_mask: next_error_mask });
    }
    error_mask
}

#[cold]
fn report_error(result: &mut JsonResult, error: JsonError) {
    println!("ERROR: {}", error.kind);
    println!("{:10}: {}", "error_mask", into_x_str(error.error_mask));
    result.errors.push(error)
}


#[derive(Debug,Default)]
pub struct ValueMask {
    pub handled: u64x8,
    pub keep: u64x8,
}

impl JsonStringParser {
    ///
    /// Find strings and string parts.
    /// 
    pub fn parse(&mut self, chunk: &mut JsonChunk) -> ValueMask {
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
        let quote = chunk.bits.where_eq(b'"');
        println!("{:10}: {}", "quote", into_x_str(quote));
        if self.in_string || quote.any() {
            // Figure out which characters are escaped
            let (escapes, escaped, backslash) = self.find_escapes(chunk);

            // Find string characters (every other quote)
            let real_quote = quote & !escaped;
            let in_string = real_quote.between_pairs(&mut self.in_string);
            println!("{:10}: {}", "in_string", into_x_str(in_string));

            // Check for invalid string characters (00-1F)
            let mut keep = in_string & !real_quote;
            let handled = in_string | real_quote;
            keep &= !chunk.report_errors(InvalidByteInString, chunk.bits.where_lt(0x20) & in_string);

            // Replace and validate string escape characters (\n, \\, etc.)
            keep &= !self.json_string_escape_parser.parse(chunk, quote, backslash, escapes, escaped);

            ValueMask { handled, keep }
        } else {
            *self = Self::default();
            ValueMask::default()
        }
    }

    ///
    /// Finish string parsing at the end of the stream.
    /// 
    /// (handle unterminated strings).
    /// 
    pub fn finish(&mut self, result: &mut JsonResult) {
        let error_if_in_string = m64x8::splat(self.in_string).select(u64x8::FIRST_BIT, u64x8::NO_BITS);
        report_errors_after(result, UnterminatedString, error_if_in_string);
    }

    ///
    /// Find escaped characters, so that we can figure out which things are and are not in strings
    /// 
    fn find_escapes(&mut self, chunk: &JsonChunk) -> (u64x8, u64x8, u64x8) {
        //
        // Find backslashed characters (including quotes).
        //
        // First, find series of backslashes that start on an even boundaries.
        //
        let backslash = chunk.bits.where_eq(b'\\');
        println!("{:10}: {}", "backslash", into_x_str(backslash));
        let starts_backslash_series = backslash.starts_series(&mut self.backslashed);
        let backslash_series_starting_on_odd_boundary = backslash.series_not_starting_with(starts_backslash_series & u64x8::EVEN_BITS, &mut self.backslash_series_starting_on_odd_boundary);

        // Next, turn series of backslashes on even boundaries into 10101010 using XOR,
        // and series on odd boundaries the same way, to get the backslashes that actually escape
        // something.
        let escapes = (u64x8::ODD_BITS & backslash_series_starting_on_odd_boundary) |
                    (u64x8::EVEN_BITS & backslash & !backslash_series_starting_on_odd_boundary);
        // The escaped characters are the characters right after a "real" escaping backslash.
        let escaped = escapes.prev(&mut self.escaped);
        println!("{:10}: {}", "escapes", into_x_str(escapes));
        println!("{:10}: {}", "escaped", into_x_str(escaped));
        (escapes, escaped, backslash)
    }
}

impl JsonStringEscapeParser {
    fn parse(&mut self, chunk: &mut JsonChunk, quote: u64x8, backslash: u64x8, escapes: u64x8, escaped: u64x8) -> u64x8 {
        // Escapes are generally rare enough that we want to skip processing if we can
        if !escaped.any() {
            *self = Self::default();
            escapes
        } else {
            // Replace single-character escapes (\b \f \n \r \t)
            // b (62 01100010) -> 08 00001000
            // f (66 01100110) -> 0C 00001100
            // n (6E 01101110) -> 0A 00001010
            // r (72 01110010) -> 0D 00001101
            // t (74 01110100) -> 09 00001001
            const ALL_BITS: u64x8 = u64x8::ALL_BITS;
            const NO_BITS: u64x8 = u64x8::NO_BITS;
            let b     = chunk.bits.where_eq(b'b'); // b 62 01100010
            let f     = chunk.bits.where_eq(b'f'); // f 66 01100110
            let n     = chunk.bits.where_eq(b'n'); // n 6E 01101110
            let r     = chunk.bits.where_eq(b'r'); // r 72 01110010
            let t     = chunk.bits.where_eq(b't'); // t 74 01110100
            let replace = escaped & (b | f | n | r | t);
            chunk.bits.replace_where(replace, [
                NO_BITS,  NO_BITS, NO_BITS, NO_BITS,
                ALL_BITS,   f | r,       n,   r | t,
            ]);

            // Replace Unicode escapes
            let u     = chunk.bits.where_eq(b'u'); // u 75 01111001
            let mut discard = escapes;
            discard |= self.replace_escaped_unicode(chunk, escaped & u);

            // Discover invalid escapes (above plus the non-replaced escapes \" \\ and \/)
            let slash = chunk.bits.where_eq(b'/'); // / 2F 00101111
            discard |= chunk.report_errors(InvalidEscapeCode, escaped & !(replace | u | quote | backslash | slash));

            discard
        }
    }

    fn parse_hex_digit(&mut self, chunk: &mut JsonChunk) -> (u64x8, u64x8, u64x8, u64x8, u64x8) {
        // 0-9 (30-39 00110000-00111001)
        // a-f (61-66 01100001-01100110)
        // A-F (41-46 01000001-01000110)
        // 0-7        00110xxx = 0xxx
        // 8-9        0011100x = 100x
        // aA         01x00001 = 1010
        // bB         01x00010 = 1011
        // cC         01x00011 = 1100
        // dD         01x00100 = 1101
        // eE         01x00101 = 1110
        // fF         01x00110 = 1111
        let is_alpha = chunk.bits[6];
        let is_a_f = (chunk.bits[3] ^ chunk.bits[2]) | (chunk.bits[2] ^ chunk.bits[1]);
        let is_0_9 = chunk.bits[5] & (
                !chunk.bits[3] | !(chunk.bits[2] & chunk.bits[1])
            );
        let is_hex = !chunk.bits[7] & (is_alpha ^ chunk.bits[4]) & (
            ( is_alpha & is_a_f) | (!is_alpha & is_0_9)
        );
        let hex0 = chunk.bits[0] ^ is_alpha;
        let hex1 = chunk.bits[1] ^ (is_alpha & chunk.bits[0]);
        let hex2 = chunk.bits[2] ^ (is_alpha & chunk.bits[0] & chunk.bits[1]);
        let hex3 = chunk.bits[3] | is_alpha;
        (is_hex, hex3, hex2, hex1, hex0)
    }

    ///
    /// Replace Unicode escapes (\uXXXX) with equivalent UTF-8 (where XXXX is hex).
    /// 
    /// This will shove the UTF-8 over to the right of the XXXX, and return a mask saying which
    /// bytes it *didn't* use.
    ///
    /// UTF-8 ranges supported in JSON:
    /// - U+0000-007F    ->                            0xxxxxxx
    /// - U+0080-07FF    ->                   110xxxxx 10xxxxxx
    /// - U+0800-FFFF    ->          1110xxxx 10xxxxxx 10xxxxxx
    /// 
    /// Also, WTF JSON doesn't support all Unicode?
    /// - U+10000-10FFFF -> 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
    /// 
    fn replace_escaped_unicode(&mut self, chunk: &mut JsonChunk, escaped_u: u64x8) -> u64x8 {
        // BRANCH NOTE: \u escapes are way rarer than normal escapes, skip this painful, expensive
        // process where we can.
        if !escaped_u.any() {
            *self = Self::default();
            u64x8::NO_BITS
        } else {
            // Figure out the hex digits (and the previous hex digits).
            // 8 = 1000
            //     ^^^^
            //  hex3..hex0
            let (is_hex, hex3, hex2, hex1, hex0) = self.parse_hex_digit(chunk);
            let prev_hex3 = hex3.prev(&mut self.prev_hex3);
            let prev_hex2 = hex2.prev(&mut self.prev_hex2);
            let prev_hex1 = hex1.prev(&mut self.prev_hex1);
            let prev_hex0 = hex0.prev(&mut self.prev_hex0);

            // Figure out which bits are 1, 2, 3 and 4 bytes after the u1.
            let u1 = escaped_u.prev(&mut self.u1);
            let u2 = u1.prev(&mut self.u2);
            let u3 = u2.prev(&mut self.u3);
            let u4 = u3.prev(&mut self.u4);
            // If we find a non-hex byte, stop--otherwise we could end up overwriting something we
            // shouldn't. This means if something is wrong we may end up partly transforming the XXXX,
            // but at least we won't overwrite later bytes (and these bytes will be marked invalid).
            let mut do_not_copy = escaped_u | chunk.report_errors(UnicodeEscapeTooShort, (u1 | u2 | u3 | u4) & !is_hex);
            let u1 = u1 & is_hex;
            let u2 = u2 & is_hex;
            let u3 = u3 & is_hex;
            let u4 = u4 & is_hex;

            // Now figure out which ones are leading, and replace the appropriate bits if so
            // \u u1 u2 u3 u4
            let u2_lead = prev_hex3 | prev_hex2 | prev_hex1 | prev_hex0 | hex3;
            let u3_cont = u2_lead.prev(&mut self.u3_cont);
            let u3_none = !u3_cont & !(prev_hex2 | prev_hex1 | prev_hex0 | hex3);
            let u4_lead = u3_none.prev(&mut self.u4_lead);

            //
            // Replace the target bits for everything.
            //
            //                   u4_lead
            // ________ ________ 0CCCDDDD
            //          u3_lead  u4_cont
            // ________ 110BBBCC 10CCDDDD
            // u2_lead  u3_cont  u4_cont
            // 1110AAAA 10BBBBCC 10CCDDDD
            //
            //  Bit | u2_lead | u3_lead | u3_cont | u4_lead | u4_cont |
            // -----|---------|---------|---------|---------|---------|
            //    7 |       1 |       1 |       1 |       0 |       1 |
            //    6 |       1 |       1 |       0 |  prev 2 |       0 |
            //    5 |       1 |       0 |  prev 3 |  prev 1 |  prev 1 |
            //    4 |       0 |  prev 2 |  prev 2 |  prev 0 |  prev 0 |
            //    3 |  prev 3 |  prev 1 |  prev 1 |   hex 3 |   hex 3 |
            //    2 |  prev 2 |  prev 0 |  prev 0 |   hex 2 |   hex 2 |
            //    1 |  prev 1 |   hex 3 |   hex 3 |   hex 1 |   hex 1 |
            //    0 |  prev 0 |   hex 2 |   hex 2 |   hex 0 |   hex 0 |

            do_not_copy |= u1 | (u2 & !u2_lead) | (u3 & u3_none);

            const ALL_BITS: u64x8 = u64x8::ALL_BITS;
            const NO_BITS: u64x8 = u64x8::NO_BITS;

            // Replace u2 (leading 3 byte)
            chunk.bits.replace_where(u2, [
                ALL_BITS,  ALL_BITS,  ALL_BITS,  NO_BITS,
                prev_hex3, prev_hex2, prev_hex1, prev_hex0
            ]);

            // Replace u3 (leading 2 byte or 3 byte cont)
            chunk.bits.replace_where(u3, [
                ALL_BITS,   !u3_cont,   u3_cont & prev_hex3, prev_hex2,
                prev_hex1, prev_hex0,                  hex3,      hex2
            ]);

            // Replace u4 (ASCII byte, or 2/3 byte cont)
            chunk.bits.replace_where(u4, [
                !u4_lead, u4_lead & prev_hex2, prev_hex1, prev_hex0,
                    hex3,                hex2,      hex1,      hex0
            ]);

            do_not_copy
        }
    }
}

impl Utf8Validator {
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
    pub fn validate(&mut self, chunk: &mut JsonChunk) -> u64x8 {
        //
        // Short-circuit ASCII-only blocks.
        //
        // ASCII only (0xxxxxxx) is always valid, and so common we skip validation when it's off.
        //
        if !chunk.bits[7].any() {
            *self = Self::default();
            u64x8::NO_BITS
        } else {
            self.really_validate_utf8(chunk)
        }
    }

    #[cold]
    fn really_validate_utf8(&mut self, chunk: &mut JsonChunk) -> u64x8 {
        //
        // Get masks for leading bits
        //
        let continuation = chunk.bits[7]  & !chunk.bits[6]; // 10xxxxxx
        let lead2plus    = chunk.bits[7]  &  chunk.bits[6]; // 11xxxxxx
        let lead2        = lead2plus & !chunk.bits[5]; // 110xxxxx
        let lead3plus    = lead2plus &  chunk.bits[5]; // 111xxxxx
        let lead3        = lead3plus & !chunk.bits[4]; // 1110xxxx
        let lead4plus    = lead3plus &  chunk.bits[4]; // 1111xxxx
        let lead4        = lead4plus & !chunk.bits[3]; // 11110xxx

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
        let cont1 = lead2plus.back(1, &mut self.cont1); //                   11xxxxxx <cont1>
        let cont2 = lead3plus.back(2, &mut self.cont2); //          111xxxxx          <cont2>
        let cont3 = lead4plus.back(3, &mut self.cont3); // 1111xxxx                   <cont3>
        let mut invalid = chunk.report_errors(Utf8CharacterWrongLength, continuation ^ (cont1 | cont2 | cont3));

        //
        // Check for UTF-8 that leads to too-large Unicode codepoints (> U+10FFFF).
        //
        let prev_f4 = (        lead4 &  chunk.bits[2] & !chunk.bits[1] & !chunk.bits[0]).prev(&mut self.prev_f4); // F4 = 11110100
        invalid |= chunk.report_errors(CharacterOutOfUnicodeRange,
            (prev_f4 & (cont1 & (chunk.bits[6] & chunk.bits[5]))) // F4 90-BF: 11110100 1001xxxx/101xxxxx
            | (lead4 & chunk.bits[2] & !chunk.bits[0])           // F5:       11110101
            | (lead4 & chunk.bits[2] & chunk.bits[1])             // F6-F7:    1111011x
            | (lead4plus & chunk.bits[3])                         // F8-FF:    11111xxx
        );

        //
        // Check for Unicode surrogates (U+D800-U+DFFF).
        //
        let prev_ed = (lead3 &  chunk.bits[3] &  chunk.bits[2] & !chunk.bits[1] &  chunk.bits[0]).prev(&mut self.prev_ed); // ED = 11101101
        invalid |= chunk.report_errors(UnicodeSurrogate, prev_ed & cont1 & chunk.bits[6]); // ED A0-BF: 11101101 101xxxxx

        //
        // Check for overlong encodings (using more bytes than needed for one Unicode codepoint).
        //
        let prev_e0 = (lead3 & !chunk.bits[3] & !chunk.bits[2] & !chunk.bits[1] & !chunk.bits[0]).prev(&mut self.prev_e0); // ED = 11100000
        let prev_f0 = (        lead4 & !chunk.bits[2] & !chunk.bits[1] & !chunk.bits[0]).prev(&mut self.prev_f0); // F0 = 11110000
        invalid |= chunk.report_errors(Utf8OverlongEncoding,
            (lead2 & !chunk.bits[4] & !chunk.bits[3] & !chunk.bits[2] & !chunk.bits[1]) //    C0-C1: 1100000x
            | (prev_e0 & cont1 & !chunk.bits[6])                                        // E0 80-9F: 11100000 100xxxxx
            | (prev_f0 & cont1 & !chunk.bits[6] & !chunk.bits[7])                       // F0 80-8F: 11110000 1000xxxx
        );
        invalid
    }
}

impl JsonLiteralNamesParser {
    pub fn parse(&mut self, chunk: &mut JsonChunk) -> (u64x8, u64x8) {
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
        let mask_a_z = chunk.bits.where_bits_eq(b'a', 5..=7);
            let mask_a_n = mask_a_z & !chunk.bits[4];
                let mask_a_f = mask_a_n & !chunk.bits[3] & chunk.bits.where_bits_different(0..=1);
                    let a = mask_a_f & !chunk.bits[2] & chunk.bits[0];
                    let mask_e_f = mask_a_f &  chunk.bits[2];
                        let e = mask_e_f & chunk.bits[0];
                        let f = mask_e_f & !chunk.bits[0];
                let mask_l_n = mask_a_n & chunk.bits[3] & chunk.bits[2] & chunk.bits[0];
                    let l = mask_l_n &  chunk.bits[1];
                    let n = mask_l_n & !chunk.bits[1];

            let mask_r_u = mask_a_z & chunk.bits[4] & !chunk.bits[3] & chunk.bits.where_bits_different(1..=2);
                let mask_r_s = mask_r_u & !chunk.bits[2];
                    let r = mask_r_s & !chunk.bits[0];
                    let s = mask_r_s &  chunk.bits[0];
                let mask_t_u = mask_r_u & chunk.bits[2];
                    let t = mask_t_u & !chunk.bits[0];
                    let u = mask_t_u &  chunk.bits[0];

        let (t1, r1, u1, f1, a1, l1, s1, n1) = (
            t.prev(&mut self.t1),
            r.prev(&mut self.r1),
            u.prev(&mut self.u1),
            // e.prev(&mut self.parser.find_literal_names_overflow.e1), // not needed, only at the end of words

            f.prev(&mut self.f1),
            a.prev(&mut self.a1),
            l.prev(&mut self.l1),
            s.prev(&mut self.s1),

            n.prev(&mut self.n1),
        );
        let (r2, u2, n2) = (
            r.back(2, &mut self.r2),
            u.back(2, &mut self.u2),
            n.back(2, &mut self.n2),
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
        (literal_name, prev_not_literal_name)
    }
}

impl JsonNumberParser {
    fn parse(&mut self, _chunk: &mut JsonChunk) {

    }
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

//fn find_hashes_and_arrays(chunk.bits: &SeparatedBits, overflow: &mut FindStringsOverflow) -> FindStrings {
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
