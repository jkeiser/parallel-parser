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

    struct TestJsonStringParserOld {
        input: Vec<[u8;512]>,
        in_string: Vec<[u8;512]>,
        escapes: Vec<[u8;512]>,
        escaped: Vec<[u8;512]>,
    }
    impl TestJsonStringParserOld {
        fn test(mut self) {
            let num_chunks = self.input.len().max(self.in_string.len()).max(self.escapes.len()).max(self.escaped.len());
            while self.input.len() < num_chunks { self.input.push(SPACES.clone()); }
            while self.in_string.len() < num_chunks { self.in_string.push(SPACES.clone()); }
            while self.escapes.len() < num_chunks { self.escapes.push(SPACES.clone()); }
            while self.escaped.len() < num_chunks { self.escaped.push(SPACES.clone()); }
            let in_string: Vec<u64x8> = self.in_string.iter().map(|mask| from_x_str(mask)).collect();
            let escapes: Vec<u64x8> = self.escapes.iter().map(|mask| from_x_str(mask)).collect();
            let value: Vec<[u8;512]> = in_string.iter().scan(false, |overflow, &in_string| Some(in_string | in_string.prev(overflow))).map(into_x_slice).collect();
            let keep: Vec<[u8;512]> = in_string.iter().zip(escapes.iter()).scan(false, |overflow, (&in_string, &escapes)| Some(in_string & in_string.prev(overflow) & !escapes)).map(into_x_slice).collect();
            TestJsonStringParserOld2 {
                input: self.input,
                value,
                keep,
                errors: HashMap::default(),
            }.test()
        }
    }

    struct TestJsonStringParserOld2 {
        input: Vec<[u8;512]>,
        value: Vec<[u8;512]>,
        keep: Vec<[u8;512]>,
        errors: HashMap<JsonErrorKind, Vec<[u8;512]>>,
    }
    impl TestJsonStringParserOld2 {
        fn test(mut self) {
            // Extend all expected/actual values with spaces as necessary to be the same length
            let num_chunks = self.input.len().max(self.value.len()).max(self.keep.len()).max(self.errors.values().map(|e| e.len()).max().unwrap_or(0));
            while self.input.len() < num_chunks { self.input.push(SPACES.clone()); }
            while self.value.len() < num_chunks { self.value.push(SPACES.clone()); }
            while self.keep.len() < num_chunks { self.keep.push(SPACES.clone()); }
            for masks in self.errors.values_mut() {
                while masks.len() < num_chunks { masks.push(SPACES.clone()); }
            }

            for value in &self.value {
                println!("value    : {}", String::from_utf8_lossy(value));
            }
            for keep in &self.keep {
                println!("keep     : {}", String::from_utf8_lossy(keep));
            }

            // Figure out the actual strings
            let mut strings: Vec<String> = vec![];
            let mut current_string = None;
            for chunk_index in 0..num_chunks {
                for i in 0..512 {
                    assert!(self.value[chunk_index][i] == b'X' || self.keep[chunk_index][i] != b'X');
                    if self.value[chunk_index][i] == b'X' {
                        let mut vec = current_string.unwrap_or_else(|| Vec::new());
                        if self.keep[chunk_index][i] == b'X' {
                            vec.push(self.input[chunk_index][i]);
                        }
                        current_string = Some(vec);
                    } else if let Some(vec) = current_string {
                        strings.push(String::from_utf8(vec).unwrap().into());
                        current_string = None;
                    }
                }
            }
            if let Some(vec) = current_string {
                strings.push(String::from_utf8(vec).unwrap().into());
            }
            TestJsonStringParser {
                input: self.input,
                strings,
                errors: self.errors,
            }.test()
        }
    }
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
        TestJsonStrings::<String> {
            input:                vec![ all(br#" "#), all(br#" "#) ],
            strings:              vec![ ],
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
    fn many_strings() {
        TestJsonStringParserOld2 {
            input:                vec![ head((0..100).fold(Vec::<u8>::new(), |mut v, n| { if v.len() + n + 2 < 512 { v.push(b'"'); v.append(&mut b" ".repeat(n)); v.push(b'"'); }; v })) ],
            value:                vec![ head((0..100).fold(Vec::<u8>::new(), |mut v, n| { if v.len() + n + 2 < 512 { v.push(b'X'); v.append(&mut b"X".repeat(n)); v.push(b'X'); }; v })) ],
            keep:                 vec![ head((0..100).fold(Vec::<u8>::new(), |mut v, n| { if v.len() + n + 2 < 512 { v.push(b' '); v.append(&mut b"X".repeat(n)); v.push(b' '); }; v })) ],
            errors:               HashMap::default(),
        }.test()
    }
    #[test]
    fn many_strings_with_commas() {
        TestJsonStringParserOld2 {
            input:                vec![ head((0..100).fold(Vec::<u8>::new(), |mut v, n| { if v.len() + n + 3 < 512 { v.push(b'"'); v.append(&mut b" ".repeat(n)); v.push(b'"'); v.push(b',')}; v })) ],
            value:                vec![ head((0..100).fold(Vec::<u8>::new(), |mut v, n| { if v.len() + n + 3 < 512 { v.push(b'X'); v.append(&mut b"X".repeat(n)); v.push(b'X'); v.push(b' ')}; v })) ],
            keep:                 vec![ head((0..100).fold(Vec::<u8>::new(), |mut v, n| { if v.len() + n + 3 < 512 { v.push(b' '); v.append(&mut b"X".repeat(n)); v.push(b' '); v.push(b' ')}; v })) ],
            errors:               HashMap::default(),
        }.test()
    }
    #[test]
    fn one_string_510() {
        TestJsonStringParserOld {
            input:                vec![ head(br#"""#.concat(br#"X"#.repeat(510)).concat(br#"""#)) ],
            in_string:            vec![ head(br#"X"#.concat(br#"X"#.repeat(510)).concat(br#" "#)) ],
            escapes:              vec![ ],
            escaped:              vec![ ],
        }.test()
    }
    #[test]
    fn one_string_509() {
        TestJsonStringParserOld {
            input:                vec![ head(br#"""#.concat(br#"X"#.repeat(509)).concat(br#"""#)) ],
            in_string:            vec![ head(br#"X"#.concat(br#"X"#.repeat(509)).concat(br#" "#)) ],
            escapes:              vec![ ],
            escaped:              vec![ ],
        }.test()
    }
    #[test]
    fn one_string_509_alternate() {
        TestJsonStringParserOld {
            input:                vec![ tail(br#"""#.concat(br#"X"#.repeat(509)).concat(br#"""#)) ],
            in_string:            vec![ tail(br#"X"#.concat(br#"X"#.repeat(509)).concat(br#" "#)) ],
            escapes:              vec![ ],
            escaped:              vec![ ],
        }.test()
    }

    #[test]
    fn string_across_boundaries() {
        TestJsonStringParserOld {
            input:                vec![ tail(br#"""#), head(br#"""#) ],
            in_string:            vec![ tail(br#"X"#), head(br#" "#) ],
            escapes:              vec![ ],
            escaped:              vec![ ],
        }.test()
    }
    #[test]
    fn string_across_boundaries_multiple_chunks() {
        TestJsonStringParserOld {
            input:                vec![ tail(br#"""#), all(br#" "#), all(br#" "#), head(br#"""#) ],
            in_string:            vec![ tail(br#"X"#), all(br#"X"#), all(br#"X"#), head(br#" "#) ],
            escapes:              vec![ ],
            escaped:              vec![ ],
        }.test()
    }
    #[test]
    fn string_across_boundaries_long() {
        TestJsonStringParserOld {
            input:                vec![ head(br#"""#), chunk(br#" "#.repeat(511),br#"""#) ],
            in_string:            vec![  all(br#"X"#), chunk(br#"X"#.repeat(511),br#" "#) ],
            escapes:              vec![ ],
            escaped:              vec![ ],
        }.test()
    }
    #[test]
    fn string_across_boundaries_multiple_chunks_long() {
        TestJsonStringParserOld {
            input:                vec![ head(br#"""#), all(br#" "#), all(br#" "#), chunk(br#" "#.repeat(511),br#"""#) ],
            in_string:            vec![  all(br#"X"#), all(br#"X"#), all(br#"X"#), chunk(br#"X"#.repeat(511),br#" "#) ],
            escapes:              vec![ ],
            escaped:              vec![ ],
        }.test()
    }
    #[test]
    fn escaped_quote() {
        TestJsonStringParserOld {
            input:                vec![ head(br#""\"""#) ],
            in_string:            vec![ head(br#"XXX "#) ],
            escapes:              vec![ head(br#" X  "#) ],
            escaped:              vec![ head(br#"  X "#) ],
        }.test()
    }
    // These may be valid string bytes, but they are not a valid string :)
    // #[test]
    // fn valid_string_bytes() {
    //     TestJsonStringParserOld {
    //         input:                vec![ head(br#"""#.concat((0x20..=0xFF).filter(|&b| b != b'\\' && b != b'"').map(|b| b    ).collect::<Vec<u8>>()).concat(br#"""#)) ],
    //         in_string:            vec![ head(br#"X"#.concat((0x20..=0xFF).filter(|&b| b != b'\\' && b != b'"').map(|_| b'X' ).collect::<Vec<u8>>()).concat(br#" "#)) ],
    //         escapes:              vec![ ],
    //         escaped:              vec![ ],
    //     }.test()
    // }
    #[test]
    fn invalid_string_bytes() {
        TestJsonStringParserOld2 {
            input:                vec![ head(br#"""#.concat((0x00..=0x19).map(|b| b    ).collect::<Vec<u8>>()).concat(br#"""#)) ],
            value:                vec![ head(br#"X"#.concat((0x00..=0x19).map(|_| b'X' ).collect::<Vec<u8>>()).concat(br#"X"#)) ],
            keep:                 vec![ head(br#" "#.concat((0x00..=0x19).map(|_| b' ' ).collect::<Vec<u8>>()).concat(br#" "#)) ],
            errors: [(InvalidByteInString,
                                  vec![ head(br#" "#.concat((0x00..=0x19).map(|_| b'X' ).collect::<Vec<u8>>()).concat(br#" "#)) ]),
                    ].iter().cloned().collect()
        }.test()
    }


    #[test]
    fn no_backslashes() {
        TestJsonStringParserOld {
            input:       vec![ tail(br#"""#), all(br#" "#), head(br#"""#) ],
            in_string:   vec![ tail(br#"X"#), all(br#"X"#), head(br#" "#) ],
            escapes:     vec![ ],
            escaped:     vec![ ],
        }.test()
    }
    #[test]
    fn no_backslashes_first_backslashed() {
        TestJsonStringParserOld {
            input:       vec![ tail(br#""\"#), chunk(br#"""#,br#"""#) ],
            escapes:     vec![ tail(br#" X"#), chunk(br#" "#,br#" "#) ],
            escaped:     vec![ tail(br#"  "#), chunk(br#"X"#,br#" "#) ],
            in_string:   vec![ tail(br#"XX"#), chunk(br#"X"#.repeat(511),br#" "#) ],
        }.test()
    }
    #[test]
    fn all_backslashes() {
        TestJsonStringParserOld {
            input:       vec![ tail(br#"""#), all(br#"\\"#), head(br#"""#) ],
            escapes:     vec![ tail(br#" "#), all(br#"X "#), head(br#" "#) ],
            escaped:     vec![ tail(br#" "#), all(br#" X"#), head(br#" "#) ],
            in_string:   vec![ tail(br#"X"#), all(br#"XX"#), head(br#" "#) ],
        }.test()
    }
    #[test]
    fn all_backslashes_first_backslashed() {
        TestJsonStringParserOld {
            input:       vec![ tail(br#""\"#), all(br#"\\"#), head(br#""""#) ],
            escapes:     vec![ tail(br#" X"#), all(br#" X"#), head(br#"  "#) ],
            escaped:     vec![ tail(br#"  "#), all(br#"X "#), head(br#"X "#) ],
            in_string:   vec![ tail(br#"XX"#), all(br#"XX"#), head(br#"X "#) ],
        }.test()
    }
    #[test]
    fn all_backslashes_first_backslashed_multiple() {
        TestJsonStringParserOld {
            input:       vec![ tail(br#""\"#), all(br#"\\"#), all(br#"\\"#), head(br#""""#) ],
            escapes:     vec![ tail(br#" X"#), all(br#" X"#), all(br#" X"#), head(br#"  "#) ],
            escaped:     vec![ tail(br#"  "#), all(br#"X "#), all(br#"X "#), head(br#"X "#) ],
            in_string:   vec![ tail(br#"XX"#), all(br#"XX"#), all(br#"XX"#), head(br#"X "#) ],
        }.test()
    }
    #[test]
    fn all_backslashes_first_backslashed_multiple_multiple() {
        TestJsonStringParserOld {
            input:       vec![ tail(br#""\"#), all(br#"\\"#), all(br#"\\"#), chunk(br#"""#,br#"\\\"#), all(br#"\\"#), all(br#"\\"#), all(br#"\\"#), head(br#""""#) ],
            escapes:     vec![ tail(br#" X"#), all(br#" X"#), all(br#" X"#), chunk(br#" "#,br#"X X"#), all(br#" X"#), all(br#" X"#), all(br#" X"#), head(br#"  "#) ],
            escaped:     vec![ tail(br#"  "#), all(br#"X "#), all(br#"X "#), chunk(br#"X"#,br#" X "#), all(br#"X "#), all(br#"X "#), all(br#"X "#), head(br#"X "#) ],
            in_string:   vec![ tail(br#"XX"#), all(br#"XX"#), all(br#"XX"#),  all(br#"XX"#),  all(br#"XX"#), all(br#"XX"#), all(br#"XX"#), head(br#"X "#) ],
        }.test()
    }
    #[test]
    fn four_backslashes() {
        TestJsonStringParserOld {
            input:       vec![ tail(br#"""#), head(br#"\\\\""#) ],
            escapes:     vec![ tail(br#" "#), head(br#"X X  "#) ],
            escaped:     vec![ tail(br#" "#), head(br#" X X "#) ],
            in_string:   vec![ tail(br#"X"#), head(br#"XXXX "#) ],
        }.test()
    }
    #[test]
    fn four_backslashes_first_backslashed() {
        TestJsonStringParserOld {
            input:       vec![ tail(br#""\"#), head(br#"\\\\"""#) ],
            escapes:     vec![ tail(br#" X"#), head(br#" X X  "#) ],
            escaped:     vec![ tail(br#"  "#), head(br#"X X X "#) ],
            in_string:   vec![ tail(br#"XX"#), head(br#"XXXXX "#) ],
        }.test()
    }
    #[test]
    fn five_backslashes() {
        TestJsonStringParserOld {
            input:       vec![ tail(br#"""#), head(br#"\\\\\"""#) ],
            escapes:     vec![ tail(br#" "#), head(br#"X X X  "#) ],
            escaped:     vec![ tail(br#" "#), head(br#" X X X "#) ],
            in_string:   vec![ tail(br#"X"#), head(br#"XXXXXX "#) ],
        }.test()
    }
    #[test]
    fn five_backslashes_first_backslashed() {
        TestJsonStringParserOld {
            input:       vec![ tail(br#""\"#), head(br#"\\\\\""#) ],
            escapes:     vec![ tail(br#" X"#), head(br#" X X  "#) ],
            escaped:     vec![ tail(br#"  "#), head(br#"X X X "#) ],
            in_string:   vec![ tail(br#"XX"#), head(br#"XXXXX "#) ],
        }.test()
    }
    #[test]
    fn almost_all_backslashes() {
        TestJsonStringParserOld {
            input:       vec![ tail(br#"""#), chunk(br#"\\"#.repeat((512-2)/2),br#"\""#), head(br#"""#) ],
            escapes:     vec![ tail(br#" "#), chunk(br#"X "#.repeat((512-2)/2),br#"X "#), head(br#" "#) ],
            escaped:     vec![ tail(br#" "#), chunk(br#" X"#.repeat((512-2)/2),br#" X"#), head(br#" "#) ],
            in_string:   vec![ tail(br#"X"#), chunk(br#"XX"#.repeat((512-2)/2),br#"XX"#), head(br#" "#) ],
        }.test()
    }
    #[test]
    fn almost_all_backslashes_first_backslashed() {
        TestJsonStringParserOld {
            input:       vec![ tail(br#""\"#), chunk(br#"\\"#.repeat((512-2)/2),br#"\""#) ],
            escapes:     vec![ tail(br#" X"#), chunk(br#" X"#.repeat((512-2)/2),br#"  "#) ],
            escaped:     vec![ tail(br#"  "#), chunk(br#"X "#.repeat((512-2)/2),br#"X "#) ],
            in_string:   vec![ tail(br#"XX"#), chunk(br#"XX"#.repeat((512-2)/2),br#"X "#) ],
        }.test()
    }
    #[test]
    fn almost_all_backslashes_alternate() {
        TestJsonStringParserOld {
            input:       vec![ tail(br#"""#), chunk(br#" "#,br#"\\"#.repeat((512-2)/2).concat(br#"\"#)), head(br#""""#) ],
            escapes:     vec![ tail(br#" "#), chunk(br#" "#,br#"X "#.repeat((512-2)/2).concat(br#"X"#)), head(br#"  "#) ],
            escaped:     vec![ tail(br#" "#), chunk(br#" "#,br#" X"#.repeat((512-2)/2).concat(br#" "#)), head(br#"X "#) ],
            in_string:   vec![ tail(br#"X"#), chunk(br#"X"#,br#"XX"#.repeat((512-2)/2).concat(br#"X"#)), head(br#"X "#) ],
        }.test()
    }
    #[test]
    fn almost_all_backslashes_alternate_first_backslashed() {
        TestJsonStringParserOld {
            input:       vec![ tail(br#""\"#), chunk(br#"""#,br#"\\"#.repeat((512-2)/2).concat(br#"\"#)), head(br#""""#) ],
            escapes:     vec![ tail(br#" X"#), chunk(br#" "#,br#"X "#.repeat((512-2)/2).concat(br#"X"#)), head(br#"  "#) ],
            escaped:     vec![ tail(br#"  "#), chunk(br#"X"#,br#" X"#.repeat((512-2)/2).concat(br#" "#)), head(br#"X "#) ],
            in_string:   vec![ tail(br#"XX"#), chunk(br#"X"#,br#"XX"#.repeat((512-2)/2).concat(br#"X"#)), head(br#"X "#) ],
        }.test()
    }
    #[test]
    fn many_different_backslashes() {
        TestJsonStringParserOld {
            input:       vec![ tail(br#"""#), head(br#"\"\\ \\\"\\\\ \\\\\"\\\\\\ \\\\\\\"\\\\\\\\ \\\\\\\\\"\\\\\\\\\\""#.repeat(1)) ],
            escapes:     vec![ tail(br#" "#), head(br#"X X  X X X X  X X X X X X  X X X X X X X X  X X X X X X X X X X  "#.repeat(1)) ],
            escaped:     vec![ tail(br#" "#), head(br#" X X  X X X X  X X X X X X  X X X X X X X X  X X X X X X X X X X "#.repeat(1)) ],
            in_string:   vec![ tail(br#"X"#), head(br#"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX "#.repeat(1)) ],
        }.test()
    }
    #[test]
    fn many_different_backslashes_first_backslashed() {
        TestJsonStringParserOld {
            input:       vec![ tail(br#""\"#), head(br#"\ \\ \\\"\\\\ \\\\\"\\\\\\ \\\\\\\"\\\\\\\\ \\\\\\\\\"\\\\\\\\\\""#.repeat(1)) ],
            escapes:     vec![ tail(br#" X"#), head(br#"  X  X X X X  X X X X X X  X X X X X X X X  X X X X X X X X X X  "#.repeat(1)) ],
            escaped:     vec![ tail(br#"  "#), head(br#"X  X  X X X X  X X X X X X  X X X X X X X X  X X X X X X X X X X "#.repeat(1)) ],
            in_string:   vec![ tail(br#"XX"#), head(br#"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX "#.repeat(1)) ],
        }.test()
    }

    #[test]
    fn every_other_backslash() {
        TestJsonStringParserOld {
            input:       vec![ tail(br#"""#),  all(br#"\""#), head(br#"""#) ],
            escapes:     vec![ tail(br#" "#),  all(br#"X "#), head(br#" "#) ],
            escaped:     vec![ tail(br#" "#),  all(br#" X"#), head(br#" "#) ],
            in_string:   vec![ tail(br#"X"#),  all(br#"XX"#), head(br#" "#) ],
        }.test()
    }
    #[test]
    fn every_other_backslash_first_backslashed() {
        TestJsonStringParserOld {
            input:       vec![ tail(br#""\"#), chunk(br#"\ "#,br#"\""#.repeat((512-2)/2)), head(br#"""#) ],
            escapes:     vec![ tail(br#" X"#), chunk(br#"  "#,br#"X "#.repeat((512-2)/2)), head(br#" "#) ],
            escaped:     vec![ tail(br#"  "#), chunk(br#"X "#,br#" X"#.repeat((512-2)/2)), head(br#" "#) ],
            in_string:   vec![ tail(br#"XX"#), chunk(br#"XX"#,br#"XX"#.repeat((512-2)/2)), head(br#" "#) ],
        }.test()
    }

    #[test]
    fn every_other_backslash_alternate() {
        TestJsonStringParserOld {
            input:       vec![ tail(br#"""#), chunk(br#" \"#,br#""\"#.repeat((512-2)/2)), head(br#""""#) ],
            escapes:     vec![ tail(br#" "#), chunk(br#" X"#,br#" X"#.repeat((512-2)/2)), head(br#"  "#) ],
            escaped:     vec![ tail(br#" "#), chunk(br#"  "#,br#"X "#.repeat((512-2)/2)), head(br#"X "#) ],
            in_string:   vec![ tail(br#"X"#), chunk(br#"XX"#,br#"XX"#.repeat((512-2)/2)), head(br#"X "#) ],
        }.test()
    }
    #[test]
    fn every_other_backslash_alternate_first_backslashed() {
        TestJsonStringParserOld {
            input:       vec![ tail(br#""\"#), chunk(br#""\"#,br#""\"#.repeat((512-2)/2)), head(br#""""#) ],
            escapes:     vec![ tail(br#" X"#), chunk(br#" X"#,br#" X"#.repeat((512-2)/2)), head(br#"  "#) ],
            escaped:     vec![ tail(br#"  "#), chunk(br#"X "#,br#"X "#.repeat((512-2)/2)), head(br#"X "#) ],
            in_string:   vec![ tail(br#"XX"#), chunk(br#"XX"#,br#"XX"#.repeat((512-2)/2)), head(br#"X "#) ],
        }.test()
    }
    #[test]
    fn every_other_2_backslash() {
        TestJsonStringParserOld {
            input:       vec![ tail(br#"""#),  head(br#"\\ "#.concat(br#"\\ "#.repeat((512-4)/3)).concat(br#"""#)) ],
            escapes:     vec![ tail(br#" "#),  head(br#"X  "#.concat(br#"X  "#.repeat((512-4)/3)).concat(br#" "#)) ],
            escaped:     vec![ tail(br#" "#),  head(br#" X "#.concat(br#" X "#.repeat((512-4)/3)).concat(br#" "#)) ],
            in_string:   vec![ tail(br#"X"#),  head(br#"XXX"#.concat(br#"XXX"#.repeat((512-4)/3)).concat(br#" "#)) ],
        }.test()
    }
    #[test]
    fn every_other_2_backslash_first_backslashed() {
        TestJsonStringParserOld {
            input:       vec![ tail(br#""\"#),  head(br#"\\""#.concat(br#"\\ "#.repeat((512-4)/3)).concat(br#"""#)) ],
            escapes:     vec![ tail(br#" X"#),  head(br#" X "#.concat(br#"X  "#.repeat((512-4)/3)).concat(br#" "#)) ],
            escaped:     vec![ tail(br#"  "#),  head(br#"X X"#.concat(br#" X "#.repeat((512-4)/3)).concat(br#" "#)) ],
            in_string:   vec![ tail(br#"XX"#),  head(br#"XXX"#.concat(br#"XXX"#.repeat((512-4)/3)).concat(br#" "#)) ],
        }.test()
    }
    #[test]
    fn every_other_2_backslash_alternate() {
        TestJsonStringParserOld {
            input:       vec![ tail(br#"""#),  head(br#" \\"#.concat(br#" \\"#.repeat((512-4)/3)).concat(br#"""#)) ],
            escapes:     vec![ tail(br#" "#),  head(br#" X "#.concat(br#" X "#.repeat((512-4)/3)).concat(br#" "#)) ],
            escaped:     vec![ tail(br#" "#),  head(br#"  X"#.concat(br#"  X"#.repeat((512-4)/3)).concat(br#" "#)) ],
            in_string:   vec![ tail(br#"X"#),  head(br#"XXX"#.concat(br#"XXX"#.repeat((512-4)/3)).concat(br#" "#)) ],
        }.test()
    }
    #[test]
    fn every_other_2_backslash_alternate_first_backslashed() {
        TestJsonStringParserOld {
            input:       vec![ tail(br#""\"#),  head(br#""\\"#.concat(br#" \\"#.repeat((512-4)/3)).concat(br#"""#)) ],
            escapes:     vec![ tail(br#" X"#),  head(br#" X "#.concat(br#" X "#.repeat((512-4)/3)).concat(br#" "#)) ],
            escaped:     vec![ tail(br#"  "#),  head(br#"X X"#.concat(br#"  X"#.repeat((512-4)/3)).concat(br#" "#)) ],
            in_string:   vec![ tail(br#"XX"#),  head(br#"XXX"#.concat(br#"XXX"#.repeat((512-4)/3)).concat(br#" "#)) ],
        }.test()
    }
}
