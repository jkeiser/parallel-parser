pub use packed_simd::*;
pub use crate::bitmask::separated_bits::*;
pub use crate::bitmask::streamable_bitmask::*;
pub use crate::json_parser::*;
pub use crate::bitmask::maskable::*;
pub use JsonErrorKind::*;
use pretty_assertions::assert_eq;
use std::fmt::Debug;
use std::str;

const SPACES: [u8;512] = [b' ';512];

fn from_x_str(input: &[u8;512]) -> u64x8 {
    separate_bits(input).where_eq(b'X')
}
fn assert_bitmasks_eq(actual: u64x8, expected: u64x8, name: impl AsRef<str>, input: &[u8;512], chunk_num: usize) {
    assert_eq!(actual, expected, "{} in chunk {} didn't match!\n{:10}: {}\n{:10}: {}\n{:10}: {}", name.as_ref(), chunk_num, "actual", into_x_str(actual), "expected", into_x_str(expected), "input", String::from_utf8_lossy(input));
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
    use std::collections::HashMap;
    use pretty_assertions::assert_eq;
    use super::*;

    struct TestJsonStrings<T: PartialEq<String>+Debug> {
        input: Vec<[u8;512]>,
        strings: Vec<T>,
    }
    impl<T: PartialEq<String>+Debug> TestJsonStrings<T> {
        fn test(self) {
            TestJsonStringParser {
                input: self.input,
                strings: self.strings,
                errors: Default::default(),
            }.test()
        }
    }
    struct TestJsonStringParser<T: PartialEq<String>+Debug> {
        input: Vec<[u8;512]>,
        strings: Vec<T>,
        errors: HashMap<JsonErrorKind, Vec<[u8;512]>>,
    }
    impl<T: PartialEq<String>+Debug> TestJsonStringParser<T> {
        fn test(mut self) {
            // Extend all expected/actual values with spaces as necessary to be the same length
            let num_chunks = self.input.len().max(self.errors.values().map(|e| e.len()).max().unwrap_or(0));
            while self.input.len() < num_chunks { self.input.push(SPACES.clone()); }
            for masks in self.errors.values_mut() {
                while masks.len() < num_chunks { masks.push(SPACES.clone()); }
            }

            // Parse
            let (actual_strings, actual) = {
                let mut actual_strings: Vec<String> = vec![];
                let mut current_string = None;
                let mut parser = JsonStringParser::default();
                let mut chunk = JsonChunk::default();
                for (chunk_index,input) in self.input.iter().enumerate() {
                    chunk = chunk.next(input, chunk_index);
                    let ValueMask { handled, keep } = parser.parse(&mut chunk);
                    for i in 0..512 {
                        if handled.get_bit(i) {
                            let mut vec = current_string.unwrap_or_else(|| Vec::new());
                            if keep.get_bit(i) {
                                vec.push(input[i as usize]);
                            }
                            current_string = Some(vec);
                        } else if let Some(vec) = current_string {
                            actual_strings.push(String::from_utf8(vec).unwrap());
                            current_string = None;
                        }
                    }
                }
                if let Some(vec) = current_string {
                    actual_strings.push(String::from_utf8(vec).unwrap());
                }
                parser.finish(&mut chunk);
                (actual_strings, chunk.finish())
            };

            // Validate actual vs. expected strings
            println!("{:?}", actual_strings);
            assert_eq!(self.strings, actual_strings);

            // Validate actual vs. expected errors
            for (chunk_index, input) in self.input.iter().enumerate() {
                let actual_errors: Vec<JsonError> = actual.errors().iter().filter(|e| e.chunk_index == chunk_index).cloned().collect();

                // Test errors
                for actual_error in &actual_errors {
                    let expected_error = self.errors.get(&actual_error.kind);
                    assert!(expected_error.is_some(), format!("unexpected error {}", actual_error.kind));
                    let expected_error = expected_error.unwrap()[chunk_index];
                    assert_bitmasks_eq(actual_error.error_mask, from_x_str(&expected_error), format!("error {}", actual_error.kind), input, chunk_index)
                }
                for expected_error in self.errors.keys() {
                    assert!(actual_errors.iter().any(|e| e.kind == *expected_error));
                }
            }
        }
    }

    #[test]
    fn no_strings() {
        TestJsonStrings::<&str> {
            input:   vec![ all(br#" "#), all(br#" "#) ],
            strings: vec![ ],
        }.test()
    }
    #[test]
    fn empty_string() {
        TestJsonStrings {
            input:   vec![ head(br#""""#) ],
            strings: vec![ "" ],
        }.test()
    }
    // This yields a somewhat unexpected result, simply because it is invalid JSON (under normal circumstances,
    // two actual strings cannot be right next to each other without a comma or : between them).
    // #[test]
    // fn all_empty_strings() {
    //     TestJsonStrings {
    //         input:                vec![ all(br#""""#), all(br#""""#) ],
    //         strings:              [""].repeat(512*2/2),
    //     }.test()
    // }
    #[test]
    fn all_empty_strings_with_commas() {
        TestJsonStrings {
            input:   vec![ head(br#""","#.repeat(510/3).concat(br#""""#)), head(br#","#.concat(br#""","#.repeat(510/3))) ],
            strings: [""].repeat((510/3)+1+(510/3)),
        }.test()
    }
    // This yields a somewhat unexpected result, simply because it is invalid JSON (under normal circumstances,
    // two actual strings cannot be right next to each other without a comma or : between them).
    // #[test]
    // fn many_strings() {
    //     TestJsonStrings {
    //         input:   vec![ *br#"""" ""  ""   ""    ""     ""      ""       ""         ""          ""           ""            ""             ""              ""               ""                ""                 ""                  ""                   ""                    ""                     ""                       ""                       ""                        ""                         ""                          ""                           ""                            ""                             ""                        ""# ],
    //         strings: vec![      ""   ],
    //     }.test()
    // }
    #[test]
    fn many_strings_with_commas() {
        TestJsonStrings {
            input:   vec![ *br#"""," ","  ","   ","    ","     ","      ","       ","         ","          ","           ","            ","             ","              ","               ","                ","                 ","                  ","                   ","                    ","                     ","                       ","                       ","                        ","                         ","                          ","                           ","                            ","                           ""# ],
            strings: vec![      ""," ","  ","   ","    ","     ","      ","       ","         ","          ","           ","            ","             ","              ","               ","                ","                 ","                  ","                   ","                    ","                     ","                       ","                       ","                        ","                         ","                          ","                           ","                            ","                           "   ],
        }.test()
    }
    #[test]
    fn one_string_510() {
        TestJsonStrings {
            input:   vec![ head(br#"""#.concat(br#"X"#.repeat(510)).concat(br#"""#)) ],
            strings: vec![ "X".repeat(510) ],
        }.test()
    }
    #[test]
    fn one_string_509() {
        TestJsonStrings {
            input:   vec![ head(br#"""#.concat(br#"X"#.repeat(509)).concat(br#"""#)) ],
            strings: vec![ "X".repeat(509) ],
        }.test()
    }
    #[test]
    fn one_string_509_alternate() {
        TestJsonStrings {
            input:   vec![ tail(br#"""#.concat(br#"X"#.repeat(509)).concat(br#"""#)) ],
            strings: vec![ "X".repeat(509) ],
        }.test()
    }
    #[test]
    fn string_across_boundaries() {
        TestJsonStrings {
            input:   vec![ head(br#"""#), tail(br#"""#) ],
            strings: vec![ " ".repeat(511+511) ],
        }.test()
    }
    #[test]
    fn empty_string_across_boundaries() {
        TestJsonStrings {
            input:   vec![ tail(br#"""#), head(br#"""#) ],
            strings: vec![ "" ]
        }.test()
    }
    #[test]
    fn string_across_boundaries_multiple_chunks() {
        TestJsonStrings {
            input:   vec![ tail(br#"""#), all(br#" "#), all(br#" "#), head(br#"""#) ],
            strings: vec![ " ".repeat(512+512) ],
        }.test()
    }
    #[test]
    fn string_across_boundaries_multiple_chunks_long() {
        TestJsonStrings {
            input:   vec![ head(br#"""#), all(br#" "#), all(br#" "#), tail(br#"""#) ],
            strings: vec![ " ".repeat(511+512+512+511) ],
        }.test()
    }
    #[test]
    fn escaped_quote() {
        TestJsonStrings {
            input:   vec![ head(br#""\"""#) ],
            strings: vec![ r#"""# ]
        }.test()
    }
    #[test]
    fn escaped_backslash() {
        TestJsonStrings {
            input:   vec![ head(br#""\\""#) ],
            strings: vec![ r#"\"# ]
        }.test()
    }
    #[test]
    fn escaped_slash() {
        TestJsonStrings {
            input:   vec![ head(br#""\/""#) ],
            strings: vec![ r#"/"# ]
        }.test()
    }
    #[test]
    fn valid_string_bytes_ascii() {
        // 20-7F (escape codes below 20 not valid)
        TestJsonStrings {
            input:   vec![         head(b"\"\x20\x21\x23\x24\x25\x26\x27\x28\x29\x2A\x2B\x2C\x2D\x2E\x2F\x30\x31\x32\x33\x34\x35\x36\x37\x38\x39\x3A\x3B\x3C\x3D\x3E\x3F\x40\x41\x42\x43\x44\x45\x46\x47\x48\x49\x4A\x4B\x4C\x4D\x4E\x4F\x50\x51\x52\x53\x54\x55\x56\x57\x58\x59\x5A\x5B\x5D\x5E\x5F\x60\x61\x62\x63\x64\x65\x66\x67\x68\x69\x6A\x6B\x6C\x6D\x6E\x6F\x70\x71\x72\x73\x74\x75\x76\x77\x78\x79\x7A\x7B\x7C\x7D\x7E\x7F\"".repeat(1)) ],
            strings: vec![ str::from_utf8(b"\x20\x21\x23\x24\x25\x26\x27\x28\x29\x2A\x2B\x2C\x2D\x2E\x2F\x30\x31\x32\x33\x34\x35\x36\x37\x38\x39\x3A\x3B\x3C\x3D\x3E\x3F\x40\x41\x42\x43\x44\x45\x46\x47\x48\x49\x4A\x4B\x4C\x4D\x4E\x4F\x50\x51\x52\x53\x54\x55\x56\x57\x58\x59\x5A\x5B\x5D\x5E\x5F\x60\x61\x62\x63\x64\x65\x66\x67\x68\x69\x6A\x6B\x6C\x6D\x6E\x6F\x70\x71\x72\x73\x74\x75\x76\x77\x78\x79\x7A\x7B\x7C\x7D\x7E\x7F").unwrap() ]
        }.test()
    }
    #[test]
    fn valid_string_bytes_utf8_2byte() {
        // C0-DF lead2 (C0, C1 always lead to overlong encodings)
        // 80-BF cont
        TestJsonStrings {
            input:   vec![         head(b"\"\xC2\x80\xC3\x80\xC4\x80\xC5\x80\xC6\x80\xC7\x80\xC8\x80\xC9\x80\xCA\x80\xCB\x80\xCC\x80\xCD\x80\xCE\x80\xCF\x80\xD0\x80\xD1\x80\xD2\x80\xD3\x80\xD4\x80\xD5\x80\xD6\x80\xD7\x80\xD8\x80\xD9\x80\xDA\x80\xDB\x80\xDC\x80\xDD\x80\xDE\x80\xDF\x80\"".repeat(1)) ],
            strings: vec![ str::from_utf8(b"\xC2\x80\xC3\x80\xC4\x80\xC5\x80\xC6\x80\xC7\x80\xC8\x80\xC9\x80\xCA\x80\xCB\x80\xCC\x80\xCD\x80\xCE\x80\xCF\x80\xD0\x80\xD1\x80\xD2\x80\xD3\x80\xD4\x80\xD5\x80\xD6\x80\xD7\x80\xD8\x80\xD9\x80\xDA\x80\xDB\x80\xDC\x80\xDD\x80\xDE\x80\xDF\x80").unwrap() ]
        }.test();
        TestJsonStrings {
            // 80-BF cont
            // C0-DF lead2 (C0, C1 always lead to overlong encodings)
            input:   vec![         head(b"\"\xC2\xBF\xC3\xBF\xC4\xBF\xC5\xBF\xC6\xBF\xC7\xBF\xC8\xBF\xC9\xBF\xCA\xBF\xCB\xBF\xCC\xBF\xCD\xBF\xCE\xBF\xCF\xBF\xD0\xBF\xD1\xBF\xD2\xBF\xD3\xBF\xD4\xBF\xD5\xBF\xD6\xBF\xD7\xBF\xD8\xBF\xD9\xBF\xDA\xBF\xDB\xBF\xDC\xBF\xDD\xBF\xDE\xBF\xDF\xBF\"".repeat(1)) ],
            strings: vec![ str::from_utf8(b"\xC2\xBF\xC3\xBF\xC4\xBF\xC5\xBF\xC6\xBF\xC7\xBF\xC8\xBF\xC9\xBF\xCA\xBF\xCB\xBF\xCC\xBF\xCD\xBF\xCE\xBF\xCF\xBF\xD0\xBF\xD1\xBF\xD2\xBF\xD3\xBF\xD4\xBF\xD5\xBF\xD6\xBF\xD7\xBF\xD8\xBF\xD9\xBF\xDA\xBF\xDB\xBF\xDC\xBF\xDD\xBF\xDE\xBF\xDF\xBF").unwrap() ]
        }.test()
    }
    #[test]
    fn valid_string_bytes_utf8_3byte() {
        // E0-EF lead3 (E0 starts with E0 A0 since E0 80-9F lead to overlong encodings, and ED ends with 9F since ED A0-BF is the Unicode surrogate range)
        // 80-BF cont
        TestJsonStrings {
            input:   vec![         head(b"\"\xE0\xA0\x80\xE1\x80\x80\xE2\x80\x80\xE3\x80\x80\xE4\x80\x80\xE5\x80\x80\xE6\x80\x80\xE7\x80\x80\xE8\x80\x80\xE9\x80\x80\xEA\x80\x80\xEB\x80\x80\xEC\x80\x80\xED\x80\x80\xEE\x80\x80\xEF\x80\x80\"".repeat(1)) ],
            strings: vec![ str::from_utf8(b"\xE0\xA0\x80\xE1\x80\x80\xE2\x80\x80\xE3\x80\x80\xE4\x80\x80\xE5\x80\x80\xE6\x80\x80\xE7\x80\x80\xE8\x80\x80\xE9\x80\x80\xEA\x80\x80\xEB\x80\x80\xEC\x80\x80\xED\x80\x80\xEE\x80\x80\xEF\x80\x80").unwrap() ]
        }.test();
        TestJsonStrings {
            input:   vec![         head(b"\"\xE0\xBF\xBF\xE1\xBF\xBF\xE2\xBF\xBF\xE3\xBF\xBF\xE4\xBF\xBF\xE5\xBF\xBF\xE6\xBF\xBF\xE7\xBF\xBF\xE8\xBF\xBF\xE9\xBF\xBF\xEA\xBF\xBF\xEB\xBF\xBF\xEC\xBF\xBF\xED\x9F\xBF\xEE\xBF\xBF\xEF\xBF\xBF\"".repeat(1)) ],
            strings: vec![ str::from_utf8(b"\xE0\xBF\xBF\xE1\xBF\xBF\xE2\xBF\xBF\xE3\xBF\xBF\xE4\xBF\xBF\xE5\xBF\xBF\xE6\xBF\xBF\xE7\xBF\xBF\xE8\xBF\xBF\xE9\xBF\xBF\xEA\xBF\xBF\xEB\xBF\xBF\xEC\xBF\xBF\xED\x9F\xBF\xEE\xBF\xBF\xEF\xBF\xBF").unwrap() ]
        }.test();
    }
    #[test]
    fn valid_string_bytes_utf8_4byte() {
        // F0-F7 lead4 (F4 90-BF and F5-F7 always lead to too-large values, F0 80-8F leads to overlong encodings, )
        TestJsonStrings {
            input:   vec![         head(b"\"\xF0\x90\x80\x80\xF1\x80\x80\x80\xF2\x80\x80\x80\xF3\x80\x80\x80\xF4\x80\x80\x80\"".repeat(1)) ],
            strings: vec![ str::from_utf8(b"\xF0\x90\x80\x80\xF1\x80\x80\x80\xF2\x80\x80\x80\xF3\x80\x80\x80\xF4\x80\x80\x80").unwrap() ]
        }.test();
        TestJsonStrings {
            input:   vec![         head(b"\"\xF0\xBF\xBF\xBF\xF1\xBF\xBF\xBF\xF2\xBF\xBF\xBF\xF3\xBF\xBF\xBF\xF4\x8F\xBF\xBF\"".repeat(1)) ],
            strings: vec![ str::from_utf8(b"\xF0\xBF\xBF\xBF\xF1\xBF\xBF\xBF\xF2\xBF\xBF\xBF\xF3\xBF\xBF\xBF\xF4\x8F\xBF\xBF").unwrap() ]
        }.test();
    }
    #[test]
    fn valid_string_bytes_utf8_cont() {
        TestJsonStrings {
            input:   vec![         head(b"\"\xC2\x80\xC2\x81\xC2\x82\xC2\x83\xC2\x84\xC2\x85\xC2\x86\xC2\x87\xC2\x88\xC2\x89\xC2\x8A\xC2\x8B\xC2\x8C\xC2\x8D\xC2\x8E\xC2\x8F\"".repeat(1)) ],
            strings: vec![ str::from_utf8(b"\xC2\x80\xC2\x81\xC2\x82\xC2\x83\xC2\x84\xC2\x85\xC2\x86\xC2\x87\xC2\x88\xC2\x89\xC2\x8A\xC2\x8B\xC2\x8C\xC2\x8D\xC2\x8E\xC2\x8F").unwrap() ]
        }.test();
        TestJsonStrings {
            input:   vec![         head(b"\"\xC2\x90\xC2\x91\xC2\x92\xC2\x93\xC2\x94\xC2\x95\xC2\x96\xC2\x97\xC2\x98\xC2\x99\xC2\x9A\xC2\x9B\xC2\x9C\xC2\x9D\xC2\x9E\xC2\x9F\"".repeat(1)) ],
            strings: vec![ str::from_utf8(b"\xC2\x90\xC2\x91\xC2\x92\xC2\x93\xC2\x94\xC2\x95\xC2\x96\xC2\x97\xC2\x98\xC2\x99\xC2\x9A\xC2\x9B\xC2\x9C\xC2\x9D\xC2\x9E\xC2\x9F").unwrap() ]
        }.test();
        TestJsonStrings {
            input:   vec![         head(b"\"\xC2\xA0\xC2\xA1\xC2\xA2\xC2\xA3\xC2\xA4\xC2\xA5\xC2\xA6\xC2\xA7\xC2\xA8\xC2\xA9\xC2\xAA\xC2\xAB\xC2\xAC\xC2\xAD\xC2\xAE\xC2\xAF\"".repeat(1)) ],
            strings: vec![ str::from_utf8(b"\xC2\xA0\xC2\xA1\xC2\xA2\xC2\xA3\xC2\xA4\xC2\xA5\xC2\xA6\xC2\xA7\xC2\xA8\xC2\xA9\xC2\xAA\xC2\xAB\xC2\xAC\xC2\xAD\xC2\xAE\xC2\xAF").unwrap() ]
        }.test();
        TestJsonStrings {
            input:   vec![         head(b"\"\xC2\xB0\xC2\xB1\xC2\xB2\xC2\xB3\xC2\xB4\xC2\xB5\xC2\xB6\xC2\xB7\xC2\xB8\xC2\xB9\xC2\xBA\xC2\xBB\xC2\xBC\xC2\xBD\xC2\xBE\xC2\xBF\"".repeat(1)) ],
            strings: vec![ str::from_utf8(b"\xC2\xB0\xC2\xB1\xC2\xB2\xC2\xB3\xC2\xB4\xC2\xB5\xC2\xB6\xC2\xB7\xC2\xB8\xC2\xB9\xC2\xBA\xC2\xBB\xC2\xBC\xC2\xBD\xC2\xBE\xC2\xBF").unwrap() ]
        }.test();
    }
    #[test]
    fn invalid_string_bytes() {
        TestJsonStringParser {
            input:   vec![ head(br#"""#.concat((0x00..=0x19).map(|b| b    ).collect::<Vec<u8>>()).concat(br#"""#)) ],
            strings: vec![ "" ],
            errors: [(InvalidByteInString,
                                  vec![ head(br#" "#.concat((0x00..=0x19).map(|_| b'X' ).collect::<Vec<u8>>()).concat(br#" "#)) ]),
                    ].iter().cloned().collect()
        }.test()
    }

    #[test]
    fn no_backslashes_first_backslashed() {
        TestJsonStrings {
            input:   vec![ tail(br#""\"#), chunk(br#"""#,br#"""#) ],
            strings: vec![ r#"""#.to_string() + &" ".repeat(510) ],
        }.test()
    }
    #[test]
    fn all_backslashes() {
        TestJsonStrings {
            input:   vec![ tail(br#"""#), all(br#"\\"#), head(br#"""#) ],
            strings: vec![ r#"\"#.repeat(512/2) ]
        }.test()
    }
    #[test]
    fn all_backslashes_first_backslashed() {
        TestJsonStrings {
            input:   vec![ tail(br#""\"#), all(br#"\\"#), head(br#""""#) ],
            strings: vec![ r#"\"#.repeat(512/2) + r#"""# ],
        }.test()
    }
    #[test]
    fn all_backslashes_first_backslashed_multiple() {
        TestJsonStrings {
            input:   vec![ tail(br#""\"#), all(br#"\\"#), all(br#"\\"#), head(br#""""#) ],
            strings: vec![ r#"\"#.repeat((512/2)+(512/2)) + r#"""# ],
        }.test()
    }
    #[test]
    fn all_backslashes_first_backslashed_multiple_multiple() {
        TestJsonStrings {
            input:   vec![ tail(br#""\"#), all(br#"\\"#), all(br#"\\"#), chunk(br#""""#,br#""\\\"#), all(br#"\\"#), all(br#"\\"#), all(br#"\\"#), head(br#""""#) ],
            strings: vec![ r#"\"#.repeat((512/2)+(512/2)) + r#"""#, r#"\"#.repeat(1 + (512/2)+(512/2)+(512/2)) + r#"""# ],
        }.test()
    }
    #[test]
    fn four_backslashes() {
        TestJsonStrings {
            input:   vec![ tail(br#"""#), head(br#"\\\\""#) ],
            strings: vec![ r#"\\"# ]
        }.test()
    }
    #[test]
    fn four_backslashes_first_backslashed() {
        TestJsonStrings {
            input:   vec![ tail(br#""\"#), head(br#"\\\\"""#) ],
            strings: vec![ r#"\\""# ]
        }.test()
    }
    #[test]
    fn five_backslashes() {
        TestJsonStrings {
            input:   vec![ tail(br#"""#), head(br#"\\\\\"""#) ],
            strings: vec![ r#"\\""# ]
        }.test()
    }
    #[test]
    fn five_backslashes_first_backslashed() {
        TestJsonStrings {
            input:   vec![ tail(br#""\"#), head(br#"\\\\\""#) ],
            strings: vec![ r#"\\\"# ]
        }.test()
    }
    #[test]
    fn almost_all_backslashes() {
        TestJsonStrings {
            input:   vec![ tail(br#"""#), chunk(br#"\\"#.repeat((512-2)/2),br#"\""#), head(br#"""#) ],
            strings: vec![ r#"\"#.repeat((512-2)/2) + r#"""# ]
        }.test()
    }
    #[test]
    fn almost_all_backslashes_first_backslashed() {
        TestJsonStrings {
            input:   vec![ tail(br#""\"#), chunk(br#"\\"#.repeat((512-2)/2),br#"\""#) ],
            strings: vec![ r#"\"#.repeat((512-2)/2 + 1) ]
        }.test()
    }
    #[test]
    fn almost_all_backslashes_alternate() {
        TestJsonStrings {
            input:       vec![ tail(br#"""#), chunk(br#" "#, br#"\\"#.repeat((512-2)/2).concat(br#"\"#)), head(br#""""#) ],
            strings: vec![ r#" "#.to_string() + &r#"\"#.repeat((512-2)/2) + r#"""# ]
        }.test()
    }
    #[test]
    fn almost_all_backslashes_alternate_first_backslashed() {
        TestJsonStrings {
            input:       vec![ tail(br#""\"#), chunk(br#"""#,br#"\\"#.repeat((512-2)/2).concat(br#"\"#)), head(br#""""#) ],
            strings: vec![ r#"""#.to_string() + &r#"\"#.repeat((512-2)/2) + r#"""# ]
        }.test()
    }
    #[test]
    fn many_different_backslashes() {
        TestJsonStrings {
            input:   vec![ tail(br#"""#), head(br#"\"\\ \\\"\\\\ \\\\\"\\\\\\ \\\\\\\"\\\\\\\\ \\\\\\\\\"\\\\\\\\\\""#.repeat(1)) ],
            strings: vec![ r#""\ \"\\ \\"\\\ \\\"\\\\ \\\\"\\\\\"# ]
        }.test()
    }
    #[test]
    fn many_different_backslashes_first_backslashed() {
        TestJsonStrings {
            input:   vec![ tail(br#""\"#), head(br#"\ \\ \\\"\\\\ \\\\\"\\\\\\ \\\\\\\"\\\\\\\\ \\\\\\\\\"\\\\\\\\\\""#.repeat(1)) ],
            strings: vec![ r#"\ \ \"\\ \\"\\\ \\\"\\\\ \\\\"\\\\\"# ]
        }.test()
    }

    #[test]
    fn every_other_backslash() {
        TestJsonStrings {
            input:   vec![ tail(br#"""#),  all(br#"\""#), head(br#"""#) ],
            strings: vec![ r#"""#.repeat(512/2) ]
        }.test()
    }
    #[test]
    fn every_other_backslash_first_backslashed() {
        TestJsonStrings {
            input:   vec![ tail(br#""\"#), chunk(br#"\ "#,br#"\""#.repeat((512-2)/2)), head(br#"""#) ],
            strings: vec![ r#"\ "#.to_string() + &r#"""#.repeat((512-2)/2) ]
        }.test()
    }

    #[test]
    fn every_other_backslash_alternate() {
        TestJsonStrings {
            input:   vec![ tail(br#"""#), chunk(br#" \"#,br#""\"#.repeat((512-2)/2)), head(br#""""#) ],
            strings: vec![ r#" "#.to_string() + &r#"""#.repeat(512/2) ]
        }.test()
    }
    #[test]
    fn every_other_backslash_alternate_first_backslashed() {
        TestJsonStrings {
            input:   vec![ tail(br#""\"#), head(br#""\"#.repeat(512/2)), head(br#""""#) ],
            strings: vec![ r#"""#.repeat(512/2 + 1) ]
        }.test()
    }
    #[test]
    fn every_other_2_backslash() {
        TestJsonStrings {
            input:   vec![ tail(br#"""#),  head(br#"\\ "#.concat(br#"\\ "#.repeat((512-4)/3)).concat(br#"""#)) ],
            strings: vec![ r#"\ "#.repeat(1 + (512-4)/3) ]
        }.test()
    }
    #[test]
    fn every_other_2_backslash_first_backslashed() {
        TestJsonStrings {
            input:   vec![ tail(br#""\"#),  head(br#"\\""#.concat(br#"\\ "#.repeat((512-4)/3)).concat(br#"""#)) ],
            strings: vec![ r#"\""#.to_string() + &r#"\ "#.repeat((512-4)/3) ]
        }.test()
    }
    #[test]
    fn every_other_2_backslash_alternate() {
        TestJsonStrings {
            input:   vec![ tail(br#"""#),  head(br#" \\"#.concat(br#" \\"#.repeat((512-4)/3)).concat(br#"""#)) ],
            strings: vec![ r#" \"#.repeat(1 + (512-4)/3) ]
        }.test()
    }
    #[test]
    fn every_other_2_backslash_alternate_first_backslashed() {
        TestJsonStrings {
            input:   vec![ tail(br#""\"#),  head(br#""\\"#.concat(br#" \\"#.repeat((512-4)/3)).concat(br#"""#)) ],
            strings: vec![ r#""\"#.to_string() + &r#" \"#.repeat((512-4)/3) ]
        }.test()
    }
}
