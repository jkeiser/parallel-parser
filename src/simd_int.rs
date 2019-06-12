use std::num::ParseIntError;
use packed_simd::*;
use crate::bitmask::*;
use std::ops;

///
/// 512-bit integer, implemented using SIMD intrinsics for speed.
/// 
/// Stored internally as u64x8, where:
/// - bit 0 is u64x8.extract(0) & 0x1
/// - bit 63 is u64x8.extract(0) & 0xFFFFFFFFFFFFFFFF
/// - bit 64 is u64x8.extract(1) & 0x1
/// - bit 511 is u64x8.extract(7) & 0xFFFFFFFFFFFFFFFF
/// 
#[derive(Copy,Clone,Debug)]
pub struct U512(u64x8);

#[allow(unused_macros)]
macro_rules! u512_literal {
    ($literal:tt) => ( crate::simd_int::U512::parse_literal(stringify!($literal)) )
}

macro_rules! impl_num_cast_up_to_64 {
    { $($type:ident),+ } => {
        $(
            impl From<$type> for U512 {
                fn from(from: $type) -> U512 {
                    U512(u64x8::new(from as u64,0,0,0,0,0,0,0))
                }
            }
            impl From<U512> for $type {
                fn from(from: U512) -> $type {
                    assert!(from & !$type::ALL_BITS == 0);
                    from.0.extract(0) as $type
                }
            }
        )+
    }
}
impl_num_cast_up_to_64! { u8,u16,u32,u64,usize,i8,i16,i32,i64,isize }

impl<T: Into<U512>+Copy> PartialEq<T> for U512 {
    fn eq(&self, other: &T) -> bool {
        self.0 == (*other).into().0
    }
}
impl From<u128> for U512 {
    fn from(from: u128) -> U512 {
        U512(u64x8::new((from & (u64::ALL_BITS as u128)) as u64,((from >> 64) & (u64::ALL_BITS as u128)) as u64,0,0,0,0,0,0))
    }
}
impl From<i128> for U512 {
    fn from(from: i128) -> U512 {
        U512(u64x8::new((from & (u64::ALL_BITS as i128)) as u64,((from >> 64) & (u64::ALL_BITS as i128)) as u64,0,0,0,0,0,0))
    }
}

impl Bitmask for U512 {
    const SIZE: u32 = 512;
    const NO_BITS: Self   = U512(u64x8::new(0x0000000000000000,0x0000000000000000,0x0000000000000000,0x0000000000000000,0x0000000000000000,0x0000000000000000,0x0000000000000000,0x0000000000000000));
    const ALL_BITS: Self  = U512(u64x8::new(0xFFFFFFFFFFFFFFFF,0xFFFFFFFFFFFFFFFF,0xFFFFFFFFFFFFFFFF,0xFFFFFFFFFFFFFFFF,0xFFFFFFFFFFFFFFFF,0xFFFFFFFFFFFFFFFF,0xFFFFFFFFFFFFFFFF,0xFFFFFFFFFFFFFFFF));
    const EVEN_BITS: Self = U512(u64x8::new(0x5555555555555555,0x5555555555555555,0x5555555555555555,0x5555555555555555,0x5555555555555555,0x5555555555555555,0x5555555555555555,0x5555555555555555));
    const ODD_BITS: Self  = U512(u64x8::new(0xAAAAAAAAAAAAAAAA,0xAAAAAAAAAAAAAAAA,0xAAAAAAAAAAAAAAAA,0xAAAAAAAAAAAAAAAA,0xAAAAAAAAAAAAAAAA,0xAAAAAAAAAAAAAAAA,0xAAAAAAAAAAAAAAAA,0xAAAAAAAAAAAAAAAA));
    const TOP_BIT: Self   = U512(u64x8::new(0x0000000000000000,0x0000000000000000,0x0000000000000000,0x0000000000000000,0x0000000000000000,0x0000000000000000,0x0000000000000000,0x8000000000000000));
}

impl U512 {
    ///
    /// Rotate the bits in this u512 left.
    ///
    pub fn rotate_left(self, count: u32) -> Self {
        self.rotate_words_left(count / 64).rotate_left_64(count % 64)
    }

    ///
    /// Rotate the bits in this u512 right.
    ///
    pub fn rotate_right(self, count: u32) -> Self {
        //
        // If this is a rotate of more than 64 bits, do that first with a shuffle.
        //
        self.rotate_words_right(count / 64).rotate_right_64(count % 64)
    }

    ///
    /// Rotate the bits in this u512 left, by LESS than 64 bits.
    ///
    pub fn rotate_left_64(self, count: u32) -> Self {
        assert!(count < 64);

        // Example we'll look at throughout:
        //
        //  1111111111111110_2222222222222220_3333333333333330_4444444444444440_5555555555555550_6666666666666660_7777777777777770_8888888888888888
        //  rol 4
        //  --------------------------------------------------------------------------------------------------------------------------------------
        //  1111111111111102_2222222222222203_3333333333333304_4444444444444405_5555555555555506_6666666666666607_7777777777777708_8888888888888801
        //

        // First, shift the bits that will stay in their current spot
        //  1111111111111100_2222222222222200_3333333333333300_4444444444444400_5555555555555500_6666666666666600_7777777777777700_8888888888888800
        let shifted_in_place = U512(self.0 << count);
        // Next, shift the bits that will carry WAAAAY over to the right, which is where they will end up in the next word.
        //  0000000000000002_0000000000000003_0000000000000004_0000000000000005_0000000000000006_0000000000000007_8000000000000008_0000000000000001
        let carried = U512(self.0 >> (64 - count));
        //  1111111111111102_2222222222222203_3333333333333304_4444444444444405_5555555555555506_6666666666666607_7777777777777708_8888888888888801
        shifted_in_place | carried.rotate_words_left(1)
    }

    ///
    /// Rotate the bits in this u512 right, by LESS than 64 bits.
    ///
    pub fn rotate_right_64(self, count: u32) -> Self {
        assert!(count < 64);

        // Example we'll look at throughout:
        //
        //  0111111111111111_0222222222222222_0333333333333333_0444444444444444_0555555555555555_0666666666666666_0777777777777777_0888888888888888
        //  ror 4
        //  --------------------------------------------------------------------------------------------------------------------------------------
        //  8011111111111111_1022222222222222_2033333333333333_3044444444444444_4055555555555555_5066666666666666_6077777777777777_7088888888888888

        // First, shift the bits that will stay in their current spot
        //  0011111111111111_0022222222222222_0033333333333333_0044444444444444_0055555555555555_0066666666666666_0077777777777777_0088888888888888
        let shifted_in_place = U512(self.0 >> count);
        // Next, shift the bits that will carry WAAAAY over to the left, which is where they will end up in the next word.
        //  1000000000000000_2000000000000000_3000000000000000_4000000000000000_5000000000000000_6000000000000000_7000000000000000_8000000000000000
        let carried = U512(self.0 << (64 - count));
        //  8011111111111111_1022222222222222_2033333333333333_3044444444444444_4055555555555555_5066666666666666_6077777777777777_7088888888888888
        shifted_in_place | carried.rotate_words_right(1)
    }

    pub fn shift_left(self, count: u32) -> Self {
        self.shift_words_left(count / 64).shift_left_overflowing_64(count % 64).0
    }

    pub fn shift_right(self, count: u32) -> Self {
        self.shift_words_right(count / 64).shift_right_overflowing_64(count % 64).0
    }

    ///
    /// Shift the bits in this u512 right (less than 64 bits), and return any overflow.
    ///
    pub fn shift_left_overflowing_64(self, count: u32) -> (Self, u64) {
        assert!(count < 64);

        // Example we'll look at throughout:
        //
        //  1111111111111111_2222222222222222_3333333333333333_4444444444444444_5555555555555555_6666666666666666_7777777777777777_8888888888888888
        //  ror 128
        //  -------------------------------------------------------------------
        //  777777777777777_88888888888888888_1111111111111111_2222222222222222_3333333333333333_4444444444444444_5555555555555555_6666666666666666
        //

        // First, shift the bits that will stay in their current spot
        //  1111111111111100_2222222222222200_3333333333333300_4444444444444400_5555555555555500_6666666666666600_7777777777777700_8888888888888800
        let shifted_in_place = U512(self.0 << count);
        // Next, shift the bits that will carry WAAAAY over to the right, which is where they will end up in the next word.
        //  0000000000000002_0000000000000003_0000000000000004_0000000000000005_0000000000000006_0000000000000007_8000000000000008_0000000000000001
        let carried = U512(self.0 >> (64 - count));
        let (carried, overflow) = carried.shift_words_left_1_overflowing();
        (shifted_in_place | carried, overflow)
    }

    ///
    /// Shift the bits in this u512 right (less than 64 bits), and return any overflow.
    ///
    pub fn shift_right_overflowing_64(self, count: u32) -> (Self, u64) {
        assert!(count < 64);

        // Example we'll look at throughout:
        //
        //  1111111111111111_2222222222222222_3333333333333333_4444444444444444_5555555555555555_6666666666666666_7777777777777777_8888888888888888
        //  ror 128
        //  -------------------------------------------------------------------
        //  777777777777777_88888888888888888_1111111111111111_2222222222222222_3333333333333333_4444444444444444_5555555555555555_6666666666666666
        //

        // First, shift the bits that will stay in their current spot
        //  1111111111111100_2222222222222200_3333333333333300_4444444444444400_5555555555555500_6666666666666600_7777777777777700_8888888888888800
        let shifted_in_place = U512(self.0 >> count);
        // Next, shift the bits that will carry WAAAAY over to the right, which is where they will end up in the next word.
        //  0000000000000002_0000000000000003_0000000000000004_0000000000000005_0000000000000006_0000000000000007_8000000000000008_0000000000000001
        let carried = U512(self.0 << (64 - count));
        let (carried, overflow) = carried.shift_words_right_1_overflowing();
        (shifted_in_place | carried, overflow)
    }

    pub fn overflowing_add(self, other: U512) -> (Self, bool) {
        use std::u64;
        use std::mem;
        // Instructions: 3 (+ > select)
        let half_add = self.0 + other.0;
        // Collect bits that actually carried, and put them into an 8 byte mask, with 1 in each byte that carried.
        let carries = half_add.gt(self.0).select(u8x8::splat(1), u8x8::splat(0));
        let overflow = carries.extract(7);

        // Carry the 1s
        // NOTE: when 64 1's is uncommon, this is a lot of extra instructions. When it's not, it's
        // better than looping.
        let carried: u64x8 = {
            // Collect the "carrythrough mask"--i.e. any lane with FFFFFFFFFFFFFFFF becomes FF, and anything else becomes 00.
            let carrythroughs = half_add.eq(u64x8::splat(u64::MAX));
            let carrythrough_mask: u64 = unsafe { mem::transmute(
                carrythroughs.select(u8x8::splat(0xFF), u8x8::splat(0x00)))
            };
            let carries: u64 = unsafe { mem::transmute(carries) };
            // add the 2 u64s to cause the 1's travel *past* any FF's and fall into the non-FF "holes," making them == 1.
            let carried: u8x8 = unsafe { mem::transmute((carries << 8) + carrythrough_mask) };
            carried.eq(u8x8::splat(1)).select(u64x8::splat(1), u64x8::splat(0))
        };

        // Add the carries to the result
        (U512(half_add + carried), overflow > 0)
    }

    pub fn from_str_hex(string: &str) -> Result<Self, ParseIntError> {
        let radix = 16;
        // Each 64-bit hex number is 16 bytes
        let bytes_per_word = 16;
        // Go from the end of the string to the beginning, parsing each number from least to most significant
        let mut result = u64x8::splat(0);
        let mut index = 0;
        let mut unparsed = string;
        while unparsed.len() >= bytes_per_word {
            let start = unparsed.len() - bytes_per_word;
            result = result.replace(index, u64::from_str_radix(&unparsed[start..], radix)?);
            index += 1;
            unparsed = &unparsed[0..start];
        }
        if unparsed.len() > 0 {
            result = result.replace(index, u64::from_str_radix(unparsed, radix)?);
        }
        Ok(Self(result))
    }

    pub fn from_str_binary(string: &str) -> Result<Self, ParseIntError> {
        let radix = 2;
        // Each 64-bit binary number is 64 bytes
        let bytes_per_word = 64;
        // Go from the end of the string to the beginning, parsing each number from least to most significant
        let mut result = u64x8::splat(0);
        let mut index = 0;
        let mut unparsed = string;
        while unparsed.len() >= bytes_per_word {
            let start = unparsed.len() - bytes_per_word;
            result = result.replace(index, u64::from_str_radix(&unparsed[start..], radix)?);
            index += 1;
            unparsed = &unparsed[0..start];
        }
        if unparsed.len() > 0 {
            result = result.replace(index, u64::from_str_radix(unparsed, radix)?);
        }
        Ok(Self(result))
    }

    pub fn to_string_hex(self) -> String {
        (0..8).rev().map(|i| format!("{:08x}", self.0.extract(i))).collect::<Vec<String>>().join("")
    }

    pub fn to_string_binary(self) -> String {
        (0..8).rev().map(|i| format!("{:064b}", self.0.extract(i))).collect::<Vec<String>>().join("")
    }

    pub fn parse_literal(literal: &str) -> Self {
        let literal = literal.replace('_', "");
        if literal.starts_with("0x") {
            Self::from_str_hex(&literal[2..]).unwrap()
        } else if literal.starts_with("0b") {
            Self::from_str_binary(&literal[2..]).unwrap()
        } else {
            panic!("u512::parse_literal() only works with 0x and 0b (hex and binary)");
        }
    }

    const INDICES: [u64;8] = [0,1,2,3,4,5,6,7];

    //
    // We write numbers in big-endian (most significant bit first). So ror 128 looks like this:
    //
    //  1111111111111111_2222222222222222_3333333333333333_4444444444444444_5555555555555555_6666666666666666_7777777777777777_8888888888888888
    //  rol 128
    //  -------------------------------------------------------------------
    //  3333333333333333_4444444444444444_5555555555555555_6666666666666666_777777777777777_88888888888888888_1111111111111111_2222222222222222
    //
    // Remember though, that we're storing the numbers in *little-endian* order, where 0 is the *least-significant* number. Therefore, the
    // u64x8 version looks like this:
    //
    // (8888888888888888,7777777777777777,6666666666666666,5555555555555555,4444444444444444,3333333333333333,2222222222222222,1111111111111111)
    //  rol 128
    //  -------------------------------------------------------------------
    // (2222222222222222,1111111111111111,8888888888888888,7777777777777777,6666666666666666,5555555555555555,4444444444444444,3333333333333333)
    //
    // So, we shuffle the actual words to the right.
    //
    fn rotate_words_left(self, count: u32) -> Self {
        let mut indices: [u64;8] = Self::INDICES;
        indices.rotate_right(count as usize);
        U512(self.0.shuffle1_dyn(indices.into()))
    }

    fn shift_words_left(self, count: u32) -> Self {
        if count >= 8 {
            U512::NO_BITS
        } else if count == 0 {
            self
        } else {
            // 1 2 3 4 5 6 7 8 << 2 == 3 4 5 6 7 8 0 0
            // [8,7,6,5,4,3,2,1] >> 2 == [0,0,8,7,6,5,4,3]
            // zeroes+[0..count-2]+zeroes
            let mut indices: [u64;8] = [7,7,7,7,7,7,7,7];
            indices[count as usize..].copy_from_slice(&Self::INDICES[..(8-count) as usize]);
            U512(self.0.replace(7,0).shuffle1_dyn(indices.into()))
        }
    }

    fn shift_words_right(self, count: u32) -> Self {
        if count >= 8 {
            U512::NO_BITS
        } else if count == 0 {
            self
        } else {
            // 1 2 3 4 5 6 7 8 >> 2 == 0 0 1 2 3 4 5 6
            // [8,7,6,5,4,3,2,1] >> 2 == [6,5,4,3,2,1,0,0]
            // [count..]+zeroes
            let mut indices: [u64;8] = [0,0,0,0,0,0,0,0];
            indices[..(8-count) as usize].copy_from_slice(&Self::INDICES[count as usize..]);
            U512(self.0.replace(0,0).shuffle1_dyn(indices.into()))
        }
    }

    //
    // We write numbers in big-endian (most significant bit first). So ror 128 looks like this:
    //
    //  1111111111111111_2222222222222222_3333333333333333_4444444444444444_5555555555555555_6666666666666666_7777777777777777_8888888888888888
    //  ror 128
    //  -------------------------------------------------------------------
    //  777777777777777_88888888888888888_1111111111111111_2222222222222222_3333333333333333_4444444444444444_5555555555555555_6666666666666666
    //
    // Remember though, that we're storing the numbers in *little-endian* order, where 0 is the *least-significant* number. Therefore, the
    // u64x8 version looks like this:
    //
    // (8888888888888888,7777777777777777,6666666666666666,5555555555555555,4444444444444444,3333333333333333,2222222222222222,1111111111111111)
    //  ror 128
    //  -------------------------------------------------------------------
    // (6666666666666666,5555555555555555,4444444444444444,3333333333333333,2222222222222222,1111111111111111,8888888888888888,7777777777777777)
    //
    // So, we shuffle the actual words to the left.
    //
    fn rotate_words_right(self, count: u32) -> Self {
        let mut indices: [u64;8] = Self::INDICES;
        indices.rotate_left(count as usize);
        U512(self.0.shuffle1_dyn(indices.into()))
    }

    fn shift_words_left_1_overflowing(self) -> (Self, u64) {
        // 1 2 3 4 5 6 7 8 << 1 == 2 3 4 5 6 7 8 0
        // [8,7,6,5,4,3,2,1] << 1 == [0,8,7,6,5,4,3,2]
        // [1..]+zeroes
        let overflow = self.0.extract(7);
        let overflow_zeroed = self.0.replace(7, 0);
        let result = shuffle!(overflow_zeroed, [7,0,1,2,3,4,5,6]);
        (U512(result), overflow)
    }

    fn shift_words_right_1_overflowing(self) -> (Self, u64) {
        // 1 2 3 4 5 6 7 8 >> 1 == 0 1 2 3 4 5 6 7
        // [8,7,6,5,4,3,2,1] >> 1 == [7,6,5,4,3,2,1,0]
        // [1..]+zeroes
        let overflow = self.0.extract(0);
        let overflow_zeroed = self.0.replace(0, 0);
        let result = shuffle!(overflow_zeroed, [1,2,3,4,5,6,7,0]);
        (U512(result), overflow)
    }
}

macro_rules! impl_shifts {
    ($($type:ty),+) => {
        $(
            impl ops::Shl<$type> for U512 {
                type Output = Self;
                fn shl(self, count: $type) -> U512 {
                    self.shift_left(count as u32)
                }
            }
            impl ops::Shr<$type> for U512 {
                type Output = Self;
                fn shr(self, count: $type) -> U512 {
                    self.shift_right(count as u32)
                }
            }
        )+
    }
}
impl_shifts! { u8,u16,u32,u64,usize,i8,i16,i32,i64,isize }

impl<T: Into<U512>> ops::BitXor<T> for U512 {
    type Output = Self;
    fn bitxor(self, other: T) -> U512 {
        U512(self.0.bitxor(other.into().0))
    }
}

impl<T: Into<U512>> ops::BitOr<T> for U512 {
    type Output = Self;
    fn bitor(self, other: T) -> U512 {
        U512(self.0.bitor(other.into().0))
    }
}

impl<T: Into<U512>> ops::BitAnd<T> for U512 {
    type Output = Self;
    fn bitand(self, other: T) -> U512 {
        U512(self.0.bitand(other.into().0))
    }
}

impl ops::Not for U512 {
    type Output = Self;
    fn not(self) -> U512 {
        U512(self.0.not())
    }
}

mod tests {
    use super::U512;

    fn test_all_shifts(original_bytes: &[u8]) {
        assert_eq!(original_bytes.len(), 512);
        use std::str;
        let original: U512 = U512::from_str_binary(str::from_utf8(original_bytes).unwrap()).unwrap();
        for i in 0..512 {
            let i = i as usize;
            let mut expected = original_bytes.to_vec();
            expected.rotate_left(i);
            assert_eq!(original.rotate_left(i as u32).to_string_binary(), str::from_utf8(&expected).unwrap());

            let mut expected = original_bytes.to_vec();
            expected.rotate_right(i);
            assert_eq!(original.rotate_right(i as u32).to_string_binary(), str::from_utf8(&expected).unwrap());

            let mut expected: [u8; 512] = [b'0'; 512];
            expected[0..(512 - i)].copy_from_slice(&original_bytes[i..512]);
            assert_eq!(original.shift_left(i as u32).to_string_binary(), str::from_utf8(&expected).unwrap());
            if i < 64 {
                let mut expected_overflow: [u8; 64] = [b'0'; 64];
                expected_overflow[(64-i)..64].copy_from_slice(&original_bytes[0..i]);
                let (actual_shifted, actual_overflow) = original.shift_left_overflowing_64(i as u32);
                assert_eq!(actual_shifted.to_string_binary(), str::from_utf8(&expected).unwrap());
                assert_eq!(format!("{:064b}", actual_overflow), str::from_utf8(&expected_overflow).unwrap());
            }

            let mut expected: [u8; 512] = [b'0'; 512];
            expected[i..512].copy_from_slice(&original_bytes[0..(512 - i)]);
            assert_eq!(original.shift_right(i as u32).to_string_binary(), str::from_utf8(&expected).unwrap());
            if i < 64 {
                let mut expected_overflow: [u8; 64] = [b'0'; 64];
                expected_overflow[0..i].copy_from_slice(&original_bytes[(512-i)..512]);
                let (actual_shifted, actual_overflow) = original.shift_right_overflowing_64(i as u32);
                assert_eq!(actual_shifted.to_string_binary(), str::from_utf8(&expected).unwrap());
                assert_eq!(format!("{:064b}", actual_overflow), str::from_utf8(&expected_overflow).unwrap());
            }
        }
    }

    #[test]
    fn test_shifts() {
        let repeated = [
            "0000".repeat(16),
            "0001".repeat(16),
            "0010".repeat(16),
            "0011".repeat(16),
            "0100".repeat(16),
            "0101".repeat(16),
            "0110".repeat(16),
            "0111".repeat(16),
            "1000".repeat(16),
            "1001".repeat(16),
            "1010".repeat(16),
            "1011".repeat(16),
            "1100".repeat(16),
            "1101".repeat(16),
            "1110".repeat(16),
            "1111".repeat(16),
        ];
        let test = format!("{}{}{}{}{}{}{}{}", repeated[1], repeated[2], repeated[3], repeated[4], repeated[5], repeated[6], repeated[7], repeated[8]);
        test_all_shifts(test.as_bytes());
        for i in 0..16 {
            let test = format!("{}{}{}{}{}{}{}{}", repeated[i], repeated[i], repeated[i], repeated[i], repeated[i], repeated[i], repeated[i], repeated[i]);
            test_all_shifts(test.as_bytes());
        }
        for i in 0..16 {
            let test = format!("{}{}{}{}{}{}{}{}", repeated[(i+0)%16], repeated[(i+1)%16], repeated[(i+2)%16], repeated[(i+3)%16], repeated[(i+4)%16], repeated[(i+5)%16], repeated[(i+6)%16], repeated[(i+7)%16]);
            test_all_shifts(test.as_bytes());
        }
    }

    #[test]
    fn ror_128() {
        let x = u512_literal!(0x1111111111111111_2222222222222222_3333333333333333_4444444444444444_5555555555555555_6666666666666666_7777777777777777_8888888888888888);
        assert_eq!(x.rotate_right(128), u512_literal!(0x7777777777777777_8888888888888888_1111111111111111_2222222222222222_3333333333333333_4444444444444444_5555555555555555_6666666666666666));
    }

    #[test]
    fn rol_128() {
        let x = u512_literal!(0x1111111111111111_2222222222222222_3333333333333333_4444444444444444_5555555555555555_6666666666666666_7777777777777777_8888888888888888);
        assert_eq!(x.rotate_left(128), u512_literal!(0x3333333333333333_4444444444444444_5555555555555555_6666666666666666_7777777777777777_8888888888888888_1111111111111111_2222222222222222));
    }
    #[test]
    fn check_it() {
        use std::mem;
        let u8_8: [u8;8] = [0x00,0x11,0x22,0x33,0x44,0x55,0x66,0x77];
        let u64_as_u64: u64 = 0x7766554433221100;
        let u64_as_u8: [u8;8] = unsafe { mem::transmute(u64_as_u64) };
        assert_eq!(u8_8,u64_as_u8);
    }
        //  1111111111111111_2222222222222222_3333333333333333_4444444444444444_5555555555555555_6666666666666666_7777777777777777_8888888888888888
        //  rol 128
        //  -------------------------------------------------------------------
        //  3333333333333333_4444444444444444_5555555555555555_6666666666666666_777777777777777_88888888888888888_1111111111111111_2222222222222222
}