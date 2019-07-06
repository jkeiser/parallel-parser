use packed_simd::*;
use strum_macros::*;
use std::str;

#[derive(Default,Debug)]
pub struct JsonResult {
    /// The buffers (we modify strings and numbers in place). This can be stripped away
    // pub modified_json: Vec<[u8;512]>,
    /// Errors that occurred during parsing
    pub(crate) errors: Vec<JsonError>,
    pub(crate) len: usize,
    pub(crate) string_bytes: Vec<u8>,
    pub(crate) string_indices: Vec<usize>,
    // /// The index of every line start, for error reporting
    // line_starts: Vec<usize>,
    // /// Strings, numbers, booleans, and nulls, in order
    // atomic_values: Vec<AtomicValueType>,
    // /// For strings and numbers, indices to their content within the buffer
    // atomic_value_indices: Vec<usize>,
    // /// The length of strings and numbers in the buffer
    // atomic_value_lengths: Vec<usize>,
}

// #[derive(Debug,Copy,Clone)]
// pub enum AtomicValueType {
//     String,
//     Number,
//     True,
//     False,
//     Null,
// }

#[derive(Debug,Clone)]
pub struct JsonError {
    pub kind: JsonErrorKind,
    pub chunk_index: usize,
    pub error_mask: u64x8,
}

///
/// Types of errors that can occur parsing JSON.
///
#[derive(Debug,Copy,Clone,EnumCount,EnumIter,Display,PartialEq,Eq,Hash)]
pub enum JsonErrorKind {
    /// Bytes < 0x20 (control characters) in string
    InvalidByteInString,
    /// Escape other than \" \\ \/ \n \r \t \b \f \u
    InvalidEscapeCode,
    /// Less than 4 hex digits after \uXXXX
    UnicodeEscapeTooShort,
    /// String doesn't have terminating " at EOF
    UnterminatedString,
    /// UTF-8 character isn't the right number of bytes (e.g. 3-byte character with 2 or 4 bytes)
    Utf8CharacterWrongLength,
    /// Character is too large for Unicode (e.g. > U+10FFFF)
    CharacterOutOfUnicodeRange,
    /// Unicode surrogate character found (U+D800-U+DFFF).
    UnicodeSurrogate,
    /// Overlong UTF-8 encoding (e.g. 3 byte character found when 2 byte would do)
    Utf8OverlongEncoding,
}

struct JsonResultStringsIterator<'a> {
    result: &'a JsonResult,
    next: usize,
}

impl<'a> Iterator for JsonResultStringsIterator<'a> {
    type Item=&'a str;
    fn next(&mut self) -> Option<Self::Item> {
        if self.next < self.result.string_indices.len() {
            let start = self.result.string_indices[self.next];
            let end = match self.result.string_indices.get(self.next+1) {
                Some(&end) => end,
                None => self.result.string_bytes.len(),
            };
            let result = unsafe { str::from_utf8_unchecked(&self.result.string_bytes[start..end]) };
            self.next += 1;
            Some(result)
        } else {
            None
        }
    }
}

impl JsonResult {
    pub fn errors(&self) -> &Vec<JsonError> {
        &self.errors
    }
    pub fn strings(&self) -> impl Iterator<Item=&str> {
        JsonResultStringsIterator { result: self, next: 0 }
    }
}