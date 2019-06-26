pub use super::*;
pub use super::find_strings::*;
pub use crate::maskable::*;
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
                Some(find_strings(overflow, &separate_bits(input)))
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
            escapes:              vec![ ],
            escaped:              vec![ ],
            invalid_string_bytes: vec![ ],
        }.test()
    }
    #[test]
    fn all_empty_strings() {
        TestFindStrings {
            input:                vec![ all(br#""""#), all(br#""""#) ],
            strings:              vec![ all(br#"X "#), all(br#"X "#) ],
            escapes:              vec![ ],
            escaped:              vec![ ],
            invalid_string_bytes: vec![ ],
        }.test()
    }
    #[test]
    fn many_strings() {
        TestFindStrings {
            input:                vec![ head((0..100).fold(Vec::<u8>::new(), |mut v, n| { if v.len() + n + 2 < 512 { v.push(b'"'); v.append(&mut b" ".repeat(n)); v.push(b'"'); }; v })) ],
            strings:              vec![ head((0..100).fold(Vec::<u8>::new(), |mut v, n| { if v.len() + n + 2 < 512 { v.push(b'X'); v.append(&mut b"X".repeat(n)); v.push(b' '); }; v })) ],
            escapes:              vec![ ],
            escaped:              vec![ ],
            invalid_string_bytes: vec![ ],
        }.test()
    }
    #[test]
    fn many_strings_with_commas() {
        TestFindStrings {
            input:                vec![ head((0..100).fold(Vec::<u8>::new(), |mut v, n| { if v.len() + n + 3 < 512 { v.push(b'"'); v.append(&mut b" ".repeat(n)); v.push(b'"'); v.push(b',')}; v })) ],
            strings:              vec![ head((0..100).fold(Vec::<u8>::new(), |mut v, n| { if v.len() + n + 3 < 512 { v.push(b'X'); v.append(&mut b"X".repeat(n)); v.push(b' '); v.push(b' ')}; v })) ],
            escapes:              vec![ ],
            escaped:              vec![ ],
            invalid_string_bytes: vec![ ],
        }.test()
    }
    #[test]
    fn one_string_510() {
        TestFindStrings {
            input:                vec![ head(br#"""#.concat(br#"X"#.repeat(510)).concat(br#"""#)) ],
            strings:              vec![ head(br#"X"#.concat(br#"X"#.repeat(510)).concat(br#" "#)) ],
            escapes:              vec![ ],
            escaped:              vec![ ],
            invalid_string_bytes: vec![ ],
        }.test()
    }
    #[test]
    fn one_string_509() {
        TestFindStrings {
            input:                vec![ head(br#"""#.concat(br#"X"#.repeat(509)).concat(br#"""#)) ],
            strings:              vec![ head(br#"X"#.concat(br#"X"#.repeat(509)).concat(br#" "#)) ],
            escapes:              vec![ ],
            escaped:              vec![ ],
            invalid_string_bytes: vec![ ],
        }.test()
    }
    #[test]
    fn one_string_509_alternate() {
        TestFindStrings {
            input:                vec![ tail(br#"""#.concat(br#"X"#.repeat(509)).concat(br#"""#)) ],
            strings:              vec![ tail(br#"X"#.concat(br#"X"#.repeat(509)).concat(br#" "#)) ],
            escapes:              vec![ ],
            escaped:              vec![ ],
            invalid_string_bytes: vec![ ],
        }.test()
    }

    #[test]
    fn string_across_boundaries() {
        TestFindStrings {
            input:                vec![ tail(br#"""#), head(br#"""#) ],
            strings:              vec![ tail(br#"X"#), head(br#" "#) ],
            escapes:              vec![ ],
            escaped:              vec![ ],
            invalid_string_bytes: vec![ ],
        }.test()
    }
    #[test]
    fn string_across_boundaries_multiple_chunks() {
        TestFindStrings {
            input:                vec![ tail(br#"""#), all(br#" "#), all(br#" "#), head(br#"""#) ],
            strings:              vec![ tail(br#"X"#), all(br#"X"#), all(br#"X"#), head(br#" "#) ],
            escapes:              vec![ ],
            escaped:              vec![ ],
            invalid_string_bytes: vec![ ],
        }.test()
    }
    #[test]
    fn string_across_boundaries_long() {
        TestFindStrings {
            input:                vec![ head(br#"""#), chunk(br#" "#.repeat(511),br#"""#) ],
            strings:              vec![  all(br#"X"#), chunk(br#"X"#.repeat(511),br#" "#) ],
            escapes:              vec![ ],
            escaped:              vec![ ],
            invalid_string_bytes: vec![ ],
        }.test()
    }
    #[test]
    fn string_across_boundaries_multiple_chunks_long() {
        TestFindStrings {
            input:                vec![ head(br#"""#), all(br#" "#), all(br#" "#), chunk(br#" "#.repeat(511),br#"""#) ],
            strings:              vec![  all(br#"X"#), all(br#"X"#), all(br#"X"#), chunk(br#"X"#.repeat(511),br#" "#) ],
            escapes:              vec![ ],
            escaped:              vec![ ],
            invalid_string_bytes: vec![ ],
        }.test()
    }
    #[test]
    fn escaped_quote() {
        TestFindStrings {
            input:                vec![ head(br#""\"""#) ],
            strings:              vec![ head(br#"XXX "#) ],
            escapes:              vec![ head(br#" X  "#) ],
            escaped:              vec![ head(br#"  X "#) ],
            invalid_string_bytes: vec![ ],
        }.test()
    }
    #[test]
    fn valid_string_bytes() {
        TestFindStrings {
            input:                vec![ head(br#"""#.concat((0x20..=0xFF).filter(|&b| b != b'\\' && b != b'"').map(|b| b    ).collect::<Vec<u8>>()).concat(br#"""#)) ],
            strings:              vec![ head(br#"X"#.concat((0x20..=0xFF).filter(|&b| b != b'\\' && b != b'"').map(|_| b'X' ).collect::<Vec<u8>>()).concat(br#" "#)) ],
            escapes:              vec![ ],
            escaped:              vec![ ],
            invalid_string_bytes: vec![ ],
        }.test()
    }
    #[test]
    fn invalid_string_bytes() {
        TestFindStrings {
            input:                vec![ head(br#"""#.concat((0x00..=0x19).map(|b| b    ).collect::<Vec<u8>>()).concat(br#"""#)) ],
            strings:              vec![ head(br#"X"#.concat((0x00..=0x19).map(|_| b'X' ).collect::<Vec<u8>>()).concat(br#" "#)) ],
            escapes:              vec![ ],
            escaped:              vec![ ],
            invalid_string_bytes: vec![ head(br#" "#.concat((0x00..=0x19).map(|_| b'X' ).collect::<Vec<u8>>()).concat(br#" "#))],
        }.test()
    }


    #[test]
    fn no_backslashes() {
        TestFindStrings {
            input:       vec![ tail(br#"""#), all(br#" "#), head(br#"""#) ],
            strings:     vec![ tail(br#"X"#), all(br#"X"#), head(br#" "#) ],
            escapes:     vec![ ],
            escaped:     vec![ ],
            invalid_string_bytes: vec![ ],
        }.test()
    }
    #[test]
    fn no_backslashes_first_backslashed() {
        TestFindStrings {
            input:       vec![ tail(br#""\"#), chunk(br#" "#,br#"""#) ],
            escapes:     vec![ tail(br#" X"#), chunk(br#" "#,br#" "#) ],
            escaped:     vec![ tail(br#"  "#), chunk(br#"X"#,br#" "#) ],
            strings:     vec![ tail(br#"XX"#), chunk(br#"X"#.repeat(511),br#" "#) ],
            invalid_string_bytes: vec![ ],
        }.test()
    }
    #[test]
    fn all_backslashes() {
        TestFindStrings {
            input:       vec![ tail(br#"""#), all(br#"\\"#) ],
            escapes:     vec![ tail(br#" "#), all(br#"X "#) ],
            escaped:     vec![ tail(br#" "#), all(br#" X"#) ],
            strings:     vec![ tail(br#"X"#), all(br#"XX"#) ],
            invalid_string_bytes: vec![ ],
        }.test()
    }
    #[test]
    fn all_backslashes_first_backslashed() {
        TestFindStrings {
            input:       vec![ tail(br#""\"#), all(br#"\\"#), head(br#""""#) ],
            escapes:     vec![ tail(br#" X"#), all(br#" X"#), head(br#"  "#) ],
            escaped:     vec![ tail(br#"  "#), all(br#"X "#), head(br#"X "#) ],
            strings:     vec![ tail(br#"XX"#), all(br#"XX"#), head(br#"X "#) ],
            invalid_string_bytes: vec![ ],
        }.test()
    }
    #[test]
    fn all_backslashes_first_backslashed_multiple() {
        TestFindStrings {
            input:       vec![ tail(br#""\"#), all(br#"\\"#), all(br#"\\"#), head(br#""""#) ],
            escapes:     vec![ tail(br#" X"#), all(br#" X"#), all(br#" X"#), head(br#"  "#) ],
            escaped:     vec![ tail(br#"  "#), all(br#"X "#), all(br#"X "#), head(br#"X "#) ],
            strings:     vec![ tail(br#"XX"#), all(br#"XX"#), all(br#"XX"#), head(br#"X "#) ],
            invalid_string_bytes: vec![ ],
        }.test()
    }
    #[test]
    fn all_backslashes_first_backslashed_multiple_multiple() {
        TestFindStrings  {
            input:       vec![ tail(br#""\"#), all(br#"\\"#), all(br#"\\"#), chunk(br#" "#,br#"\\\"#), all(br#"\\"#), all(br#"\\"#), all(br#"\\"#), head(br#""""#) ],
            escapes:     vec![ tail(br#" X"#), all(br#" X"#), all(br#" X"#), chunk(br#" "#,br#"X X"#), all(br#" X"#), all(br#" X"#), all(br#" X"#), head(br#"  "#) ],
            escaped:     vec![ tail(br#"  "#), all(br#"X "#), all(br#"X "#), chunk(br#"X"#,br#" X "#), all(br#"X "#), all(br#"X "#), all(br#"X "#), head(br#"X "#) ],
            strings:     vec![ tail(br#"XX"#), all(br#"XX"#), all(br#"XX"#),  all(br#"XX"#),  all(br#"XX"#), all(br#"XX"#), all(br#"XX"#), head(br#"X "#) ],
            invalid_string_bytes: vec![ ],
        }.test()
    }
    #[test]
    fn four_backslashes() {
        TestFindStrings {
            input:       vec![ tail(br#"""#), head(br#"\\\\""#) ],
            escapes:     vec![ tail(br#" "#), head(br#"X X  "#) ],
            escaped:     vec![ tail(br#" "#), head(br#" X X "#) ],
            strings:     vec![ tail(br#"X"#), head(br#"XXXX "#) ],
            invalid_string_bytes: vec![ ],
        }.test()
    }
    #[test]
    fn four_backslashes_first_backslashed() {
        TestFindStrings {
            input:       vec![ tail(br#""\"#), head(br#"\\\\"""#) ],
            escapes:     vec![ tail(br#" X"#), head(br#" X X  "#) ],
            escaped:     vec![ tail(br#"  "#), head(br#"X X X "#) ],
            strings:     vec![ tail(br#"XX"#), head(br#"XXXXX "#) ],
            invalid_string_bytes: vec![ ],
        }.test()
    }
    #[test]
    fn five_backslashes() {
        TestFindStrings {
            input:       vec![ tail(br#"""#), head(br#"\\\\\"""#) ],
            escapes:     vec![ tail(br#" "#), head(br#"X X X  "#) ],
            escaped:     vec![ tail(br#" "#), head(br#" X X X "#) ],
            strings:     vec![ tail(br#"X"#), head(br#"XXXXXX "#) ],
            invalid_string_bytes: vec![ ],
        }.test()
    }
    #[test]
    fn five_backslashes_first_backslashed() {
        TestFindStrings {
            input:       vec![ tail(br#""\"#), head(br#"\\\\\""#) ],
            escapes:     vec![ tail(br#" X"#), head(br#" X X  "#) ],
            escaped:     vec![ tail(br#"  "#), head(br#"X X X "#) ],
            strings:     vec![ tail(br#"XX"#), head(br#"XXXXX "#) ],
            invalid_string_bytes: vec![ ],
        }.test()
    }
    #[test]
    fn almost_all_backslashes() {
        TestFindStrings {
            input:       vec![ tail(br#"""#), chunk(br#"\\"#.repeat((512-2)/2),br#"\""#), head(br#"""#) ],
            escapes:     vec![ tail(br#" "#), chunk(br#"X "#.repeat((512-2)/2),br#"X "#), head(br#" "#) ],
            escaped:     vec![ tail(br#" "#), chunk(br#" X"#.repeat((512-2)/2),br#" X"#), head(br#" "#) ],
            strings:     vec![ tail(br#"X"#), chunk(br#"XX"#.repeat((512-2)/2),br#"XX"#), head(br#" "#) ],
            invalid_string_bytes: vec![ ],
        }.test()
    }
    #[test]
    fn almost_all_backslashes_first_backslashed() {
        TestFindStrings {
            input:       vec![ tail(br#""\"#), chunk(br#"\\"#.repeat((512-2)/2),br#"\""#) ],
            escapes:     vec![ tail(br#" X"#), chunk(br#" X"#.repeat((512-2)/2),br#"  "#) ],
            escaped:     vec![ tail(br#"  "#), chunk(br#"X "#.repeat((512-2)/2),br#"X "#) ],
            strings:     vec![ tail(br#"XX"#), chunk(br#"XX"#.repeat((512-2)/2),br#"X "#) ],
            invalid_string_bytes: vec![ ],
        }.test()
    }
    #[test]
    fn almost_all_backslashes_alternate() {
        TestFindStrings {
            input:       vec![ tail(br#"""#), chunk(br#" "#,br#"\\"#.repeat((512-2)/2).concat(br#"\"#)), head(br#""""#) ],
            escapes:     vec![ tail(br#" "#), chunk(br#" "#,br#"X "#.repeat((512-2)/2).concat(br#"X"#)), head(br#"  "#) ],
            escaped:     vec![ tail(br#" "#), chunk(br#" "#,br#" X"#.repeat((512-2)/2).concat(br#" "#)), head(br#"X "#) ],
            strings:     vec![ tail(br#"X"#), chunk(br#"X"#,br#"XX"#.repeat((512-2)/2).concat(br#"X"#)), head(br#"X "#) ],
            invalid_string_bytes: vec![ ],
        }.test()
    }
    #[test]
    fn almost_all_backslashes_alternate_first_backslashed() {
        TestFindStrings {
            input:       vec![ tail(br#""\"#), chunk(br#" "#,br#"\\"#.repeat((512-2)/2).concat(br#"\"#)), head(br#""""#) ],
            escapes:     vec![ tail(br#" X"#), chunk(br#" "#,br#"X "#.repeat((512-2)/2).concat(br#"X"#)), head(br#"  "#) ],
            escaped:     vec![ tail(br#"  "#), chunk(br#"X"#,br#" X"#.repeat((512-2)/2).concat(br#" "#)), head(br#"X "#) ],
            strings:     vec![ tail(br#"XX"#), chunk(br#"X"#,br#"XX"#.repeat((512-2)/2).concat(br#"X"#)), head(br#"X "#) ],
            invalid_string_bytes: vec![ ],
        }.test()
    }
    #[test]
    fn many_different_backslashes() {
        TestFindStrings {
            input:       vec![ tail(br#"""#), head(br#"\ \\ \\\ \\\\ \\\\\ \\\\\\ \\\\\\\ \\\\\\\\ \\\\\\\\\ \\\\\\\\\\""#.repeat(1)) ],
            escapes:     vec![ tail(br#" "#), head(br#"X X  X X X X  X X X X X X  X X X X X X X X  X X X X X X X X X X  "#.repeat(1)) ],
            escaped:     vec![ tail(br#" "#), head(br#" X X  X X X X  X X X X X X  X X X X X X X X  X X X X X X X X X X "#.repeat(1)) ],
            strings:     vec![ tail(br#"X"#), head(br#"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX "#.repeat(1)) ],
            invalid_string_bytes: vec![ ],
        }.test()
    }
    #[test]
    fn many_different_backslashes_first_backslashed() {
        TestFindStrings {
            input:       vec![ tail(br#""\"#), head(br#"\ \\ \\\ \\\\ \\\\\ \\\\\\ \\\\\\\ \\\\\\\\ \\\\\\\\\ \\\\\\\\\\""#.repeat(1)) ],
            escapes:     vec![ tail(br#" X"#), head(br#"  X  X X X X  X X X X X X  X X X X X X X X  X X X X X X X X X X  "#.repeat(1)) ],
            escaped:     vec![ tail(br#"  "#), head(br#"X  X  X X X X  X X X X X X  X X X X X X X X  X X X X X X X X X X "#.repeat(1)) ],
            strings:     vec![ tail(br#"XX"#), head(br#"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX "#.repeat(1)) ],
            invalid_string_bytes: vec![ ],
        }.test()
    }

    #[test]
    fn every_other_backslash() {
        TestFindStrings {
            input:       vec![ tail(br#"""#),  all(br#"\ "#), head(br#"""#) ],
            escapes:     vec![ tail(br#" "#),  all(br#"X "#), head(br#" "#) ],
            escaped:     vec![ tail(br#" "#),  all(br#" X"#), head(br#" "#) ],
            strings:     vec![ tail(br#"X"#),  all(br#"XX"#), head(br#" "#) ],
            invalid_string_bytes: vec![ ],
        }.test()
    }
    #[test]
    fn every_other_backslash_first_backslashed() {
        TestFindStrings {
            input:       vec![ tail(br#""\"#), chunk(br#"\ "#,br#"\ "#.repeat((512-2)/2)), head(br#"""#) ],
            escapes:     vec![ tail(br#" X"#), chunk(br#"  "#,br#"X "#.repeat((512-2)/2)), head(br#" "#) ],
            escaped:     vec![ tail(br#"  "#), chunk(br#"X "#,br#" X"#.repeat((512-2)/2)), head(br#" "#) ],
            strings:     vec![ tail(br#"XX"#), chunk(br#"XX"#,br#"XX"#.repeat((512-2)/2)), head(br#" "#) ],
            invalid_string_bytes: vec![ ],
        }.test()
    }

    #[test]
    fn every_other_backslash_alternate() {
        TestFindStrings {
            input:       vec![ tail(br#"""#), chunk(br#" \"#,br#" \"#.repeat((512-2)/2)), head(br#""""#) ],
            escapes:     vec![ tail(br#" "#), chunk(br#" X"#,br#" X"#.repeat((512-2)/2)), head(br#"  "#) ],
            escaped:     vec![ tail(br#" "#), chunk(br#"  "#,br#"X "#.repeat((512-2)/2)), head(br#"X "#) ],
            strings:     vec![ tail(br#"X"#), chunk(br#"XX"#,br#"XX"#.repeat((512-2)/2)), head(br#"X "#) ],
            invalid_string_bytes: vec![ ],
        }.test()
    }
    #[test]
    fn every_other_backslash_alternate_first_backslashed() {
        TestFindStrings {
            input:       vec![ tail(br#""\"#), chunk(br#" \"#,br#" \"#.repeat((512-2)/2)), head(br#""""#) ],
            escapes:     vec![ tail(br#" X"#), chunk(br#" X"#,br#" X"#.repeat((512-2)/2)), head(br#"  "#) ],
            escaped:     vec![ tail(br#"  "#), chunk(br#"X "#,br#"X "#.repeat((512-2)/2)), head(br#"X "#) ],
            strings:     vec![ tail(br#"XX"#), chunk(br#"XX"#,br#"XX"#.repeat((512-2)/2)), head(br#"X "#) ],
            invalid_string_bytes: vec![ ],
        }.test()
    }
    #[test]
    fn every_other_2_backslash() {
        TestFindStrings {
            input:       vec![ tail(br#"""#),  head(br#"\\ "#.concat(br#"\\ "#.repeat((512-4)/3)).concat(br#"""#)) ],
            escapes:     vec![ tail(br#" "#),  head(br#"X  "#.concat(br#"X  "#.repeat((512-4)/3)).concat(br#" "#)) ],
            escaped:     vec![ tail(br#" "#),  head(br#" X "#.concat(br#" X "#.repeat((512-4)/3)).concat(br#" "#)) ],
            strings:     vec![ tail(br#"X"#),  head(br#"XXX"#.concat(br#"XXX"#.repeat((512-4)/3)).concat(br#" "#)) ],
            invalid_string_bytes: vec![ ],
        }.test()
    }
    #[test]
    fn every_other_2_backslash_first_backslashed() {
        TestFindStrings {
            input:       vec![ tail(br#""\"#),  head(br#"\\ "#.concat(br#"\\ "#.repeat((512-4)/3)).concat(br#"""#)) ],
            escapes:     vec![ tail(br#" X"#),  head(br#" X "#.concat(br#"X  "#.repeat((512-4)/3)).concat(br#" "#)) ],
            escaped:     vec![ tail(br#"  "#),  head(br#"X X"#.concat(br#" X "#.repeat((512-4)/3)).concat(br#" "#)) ],
            strings:     vec![ tail(br#"XX"#),  head(br#"XXX"#.concat(br#"XXX"#.repeat((512-4)/3)).concat(br#" "#)) ],
            invalid_string_bytes: vec![ ],
        }.test()
    }
    #[test]
    fn every_other_2_backslash_alternate() {
        TestFindStrings {
            input:       vec![ tail(br#"""#),  head(br#" \\"#.concat(br#" \\"#.repeat((512-4)/3)).concat(br#"""#)) ],
            escapes:     vec![ tail(br#" "#),  head(br#" X "#.concat(br#" X "#.repeat((512-4)/3)).concat(br#" "#)) ],
            escaped:     vec![ tail(br#" "#),  head(br#"  X"#.concat(br#"  X"#.repeat((512-4)/3)).concat(br#" "#)) ],
            strings:     vec![ tail(br#"X"#),  head(br#"XXX"#.concat(br#"XXX"#.repeat((512-4)/3)).concat(br#" "#)) ],
            invalid_string_bytes: vec![ ],
        }.test()
    }
    #[test]
    fn every_other_2_backslash_alternate_first_backslashed() {
        TestFindStrings {
            input:       vec![ tail(br#""\"#),  head(br#" \\"#.concat(br#" \\"#.repeat((512-4)/3)).concat(br#"""#)) ],
            escapes:     vec![ tail(br#" X"#),  head(br#" X "#.concat(br#" X "#.repeat((512-4)/3)).concat(br#" "#)) ],
            escaped:     vec![ tail(br#"  "#),  head(br#"X X"#.concat(br#"  X"#.repeat((512-4)/3)).concat(br#" "#)) ],
            strings:     vec![ tail(br#"XX"#),  head(br#"XXX"#.concat(br#"XXX"#.repeat((512-4)/3)).concat(br#" "#)) ],
            invalid_string_bytes: vec![ ],
        }.test()
    }
}
