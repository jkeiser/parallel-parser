use std::convert::TryFrom;
use std;
use std::fmt::*;
use std::hash::*;
use std::num::*;
use std::ops::*;
use std::result::Result;
use std::str::*;
use std::mem::size_of;
use packed_simd::*;

pub trait PrimitiveBase:
    Copy + Clone + Default + Hash + Debug +
    PartialEq<Self> + Eq + PartialOrd<Self> + Ord +
    Send + Sync + Sized
{
    const BYTE_WIDTH: usize = size_of::<Self>();
    const BIT_WIDTH: u32 = Self::BYTE_WIDTH as u32 * 8;
    type Mask: PrimitiveMask;
}

pub trait PrimitiveInteger: PrimitiveBase +
    From<bool> +
    TryFrom<u8> + TryFrom<u16> + TryFrom<u32> + TryFrom<u64> + TryFrom<u128> + TryFrom<usize> +
    TryFrom<i8> + TryFrom<i16> + TryFrom<i32> + TryFrom<i64> + TryFrom<i128> + TryFrom<isize> +
    FromStr + LowerHex + UpperHex + Octal + Binary + Display +
    Add<Self, Output=Self> + Sub<Self, Output=Self> + Div<Self, Output=Self> + Rem<Self, Output=Self> + Mul<Self, Output=Self> +
    AddAssign<Self> + SubAssign<Self> + DivAssign<Self> + RemAssign<Self> + MulAssign<Self> +
    BitAnd<Self, Output=Self> + BitXor<Self, Output=Self> + BitOr<Self, Output=Self> + Not<Output=Self> +
    BitAndAssign<Self> + BitXorAssign<Self> + BitOrAssign<Self> +
    Shl<u32, Output=Self> + Shr<u32, Output=Self> +
    ShlAssign<u32> + ShrAssign<u32>
{
    const MAX: Self;
    const MIN: Self;
    fn min_value() -> Self;
    fn max_value() -> Self;
    fn from_str_radix(src: &str, radix: u32) -> Result<Self, ParseIntError>;
    fn count_ones(self) -> u32;
    fn count_zeros(self) -> u32;
    fn leading_zeros(self) -> u32;
    fn trailing_zeros(self) -> u32;
    fn rotate_left(self, n: u32) -> Self;
    fn rotate_right(self, n: u32) -> Self;
    fn swap_bytes(self) -> Self;
    fn reverse_bits(self) -> Self;
    fn from_be(x: Self) -> Self;
    fn from_le(x: Self) -> Self;
    fn to_be(self) -> Self;
    fn to_le(self) -> Self;
    fn checked_add(self, rhs: Self) -> Option<Self>;
    fn checked_sub(self, rhs: Self) -> Option<Self>;
    fn checked_mul(self, rhs: Self) -> Option<Self>;
    fn checked_div(self, rhs: Self) -> Option<Self>;
    fn checked_rem(self, rhs: Self) -> Option<Self>;
    fn checked_neg(self) -> Option<Self>;
    fn checked_shl(self, rhs: u32) -> Option<Self>;
    fn checked_shr(self, rhs: u32) -> Option<Self>;
    fn checked_pow(self, exp: u32) -> Option<Self>;
    fn saturating_add(self, rhs: Self) -> Self;
    fn saturating_sub(self, rhs: Self) -> Self;
    fn saturating_mul(self, rhs: Self) -> Self;
    fn saturating_pow(self, exp: u32) -> Self;
    fn wrapping_add(self, rhs: Self) -> Self;
    fn wrapping_sub(self, rhs: Self) -> Self;
    fn wrapping_mul(self, rhs: Self) -> Self;
    fn wrapping_div(self, rhs: Self) -> Self;
    fn wrapping_rem(self, rhs: Self) -> Self;
    fn wrapping_neg(self) -> Self;
    fn wrapping_shl(self, rhs: u32) -> Self;
    fn wrapping_shr(self, rhs: u32) -> Self;
    fn wrapping_pow(self, exp: u32) -> Self;
    fn overflowing_add(self, rhs: Self) -> (Self, bool);
    fn overflowing_sub(self, rhs: Self) -> (Self, bool);
    fn overflowing_mul(self, rhs: Self) -> (Self, bool);
    fn overflowing_div(self, rhs: Self) -> (Self, bool);
    fn overflowing_rem(self, rhs: Self) -> (Self, bool);
    fn overflowing_neg(self) -> (Self, bool);
    fn overflowing_shl(self, rhs: u32) -> (Self, bool);
    fn overflowing_shr(self, rhs: u32) -> (Self, bool);
    fn overflowing_pow(self, exp: u32) -> (Self, bool);
    fn pow(self, exp: u32) -> Self;
    // fn to_be_bytes(self) -> [u8; Self::BYTE_WIDTH];;
    // fn to_le_bytes(self) -> [u8; Self::BYTE_WIDTH];
    // fn to_ne_bytes(self) -> [u8; Self::BYTE_WIDTH];
    // fn from_be_bytes(bytes: [u8; Self::BYTE_WIDTH]) -> Self;
    // fn from_le_bytes(bytes: [u8; Self::BYTE_WIDTH]) -> Self;
    // fn from_ne_bytes(bytes: [u8; Self::BYTE_WIDTH]) -> Self;
}

pub trait PrimitiveUnsigned: PrimitiveInteger + From<u8> {
    fn is_power_of_two(self) -> bool;
    fn next_power_of_two(self) -> Self;
    fn checked_next_power_of_two(self) -> Option<Self>;
}

pub trait PrimitiveSigned: PrimitiveInteger + From<i8> {
    fn checked_abs(self) -> Option<Self>;
    fn wrapping_abs(self) -> Self;
    fn overflowing_abs(self) -> (Self, bool);
    fn abs(self) -> Self;
    fn signum(self) -> Self;
    fn is_positive(self) -> bool;
    fn is_negative(self) -> bool;
}

pub trait PrimitiveMask: PrimitiveBase {
    fn new(x: bool) -> Self;
    fn test(&self) -> bool;
}

macro_rules! impl_native_bytes {
    ( $type:ident, $mask:ident ) => {
        impl PrimitiveBase for $type {
            type Mask = $mask;
        }
    }
}

macro_rules! impl_native_integer {
    ( $type:ident, $mask:ident ) => {
        impl_native_bytes!($type, $mask);
        use std::$type;
        impl PrimitiveInteger for $type {
            const MAX: Self = <$type>::min_value();
            const MIN: Self = <$type>::max_value();
            fn min_value() -> Self { <$type>::min_value() }
            fn max_value() -> Self { <$type>::max_value() }
            fn from_str_radix(src: &str, radix: u32) -> Result<Self, ParseIntError> { <$type>::from_str_radix(src, radix) }
            fn count_ones(self) -> u32 { self.count_ones() }
            fn count_zeros(self) -> u32 { self.count_zeros() }
            fn leading_zeros(self) -> u32 { self.leading_zeros() }
            fn trailing_zeros(self) -> u32 { self.trailing_zeros() }
            fn rotate_left(self, n: u32) -> Self { self.rotate_left(n) }
            fn rotate_right(self, n: u32) -> Self { self.rotate_right(n) }
            fn swap_bytes(self) -> Self { self.swap_bytes() }
            fn reverse_bits(self) -> Self { self.reverse_bits() }
            fn from_be(x: Self) -> Self { <$type>::from_be(x) }
            fn from_le(x: Self) -> Self { <$type>::from_le(x) }
            fn to_be(self) -> Self { self.to_be() }
            fn to_le(self) -> Self { self.to_le() }
            fn checked_add(self, rhs: Self) -> Option<Self> { self.checked_add(rhs) }
            fn checked_sub(self, rhs: Self) -> Option<Self> { self.checked_sub(rhs) }
            fn checked_mul(self, rhs: Self) -> Option<Self> { self.checked_mul(rhs) }
            fn checked_div(self, rhs: Self) -> Option<Self> { self.checked_div(rhs) }
            fn checked_rem(self, rhs: Self) -> Option<Self> { self.checked_rem(rhs) }
            fn checked_neg(self) -> Option<Self> { self.checked_neg() }
            fn checked_shl(self, rhs: u32) -> Option<Self> { self.checked_shl(rhs) }
            fn checked_shr(self, rhs: u32) -> Option<Self> { self.checked_shr(rhs) }
            fn checked_pow(self, exp: u32) -> Option<Self> { self.checked_pow(exp) }
            fn saturating_add(self, rhs: Self) -> Self { self.saturating_add(rhs) }
            fn saturating_sub(self, rhs: Self) -> Self { self.saturating_sub(rhs) }
            fn saturating_mul(self, rhs: Self) -> Self { self.saturating_mul(rhs) }
            fn saturating_pow(self, exp: u32) -> Self { self.saturating_pow(exp) }
            fn wrapping_add(self, rhs: Self) -> Self { self.wrapping_add(rhs) }
            fn wrapping_sub(self, rhs: Self) -> Self { self.wrapping_sub(rhs) }
            fn wrapping_mul(self, rhs: Self) -> Self { self.wrapping_mul(rhs) }
            fn wrapping_div(self, rhs: Self) -> Self { self.wrapping_div(rhs) }
            fn wrapping_rem(self, rhs: Self) -> Self { self.wrapping_rem(rhs) }
            fn wrapping_neg(self) -> Self { self.wrapping_neg() }
            fn wrapping_shl(self, rhs: u32) -> Self { self.wrapping_shl(rhs) }
            fn wrapping_shr(self, rhs: u32) -> Self { self.wrapping_shr(rhs) }
            fn wrapping_pow(self, exp: u32) -> Self { self.wrapping_pow(exp) }
            fn overflowing_add(self, rhs: Self) -> (Self, bool) { self.overflowing_add(rhs) }
            fn overflowing_sub(self, rhs: Self) -> (Self, bool) { self.overflowing_sub(rhs) }
            fn overflowing_mul(self, rhs: Self) -> (Self, bool) { self.overflowing_mul(rhs) }
            fn overflowing_div(self, rhs: Self) -> (Self, bool) { self.overflowing_div(rhs) }
            fn overflowing_rem(self, rhs: Self) -> (Self, bool) { self.overflowing_rem(rhs) }
            fn overflowing_neg(self) -> (Self, bool) { self.overflowing_neg() }
            fn overflowing_shl(self, rhs: u32) -> (Self, bool) { self.overflowing_shl(rhs) }
            fn overflowing_shr(self, rhs: u32) -> (Self, bool) { self.overflowing_shr(rhs) }
            fn overflowing_pow(self, exp: u32) -> (Self, bool) { self.overflowing_pow(exp) }
            fn pow(self, exp: u32) -> Self { self.pow(exp) }
            // fn to_be_bytes(self) -> [u8; Self::WIDTH] { self.to_be_bytes() }
            // fn to_le_bytes(self) -> [u8; Self::WIDTH] { self.to_le_bytes() }
            // fn to_ne_bytes(self) -> [u8; Self::WIDTH] { self.to_ne_bytes() }
            // fn from_be_bytes(bytes: [u8; Self::WIDTH]) -> Self { <$type>::from_be_bytes(bytes) }
            // fn from_le_bytes(bytes: [u8; Self::WIDTH]) -> Self { <$type>::from_le_bytes(bytes) }
            // fn from_ne_bytes(bytes: [u8; Self::WIDTH]) -> Self { <$type>::from_ne_bytes(bytes) }
        }
    }
}

macro_rules! impl_native_unsigned {
    ( $type:ident, $mask:ident ) => {
        impl_native_integer!($type, $mask);
        impl PrimitiveUnsigned for $type {
            fn is_power_of_two(self) -> bool { self.is_power_of_two() }
            fn next_power_of_two(self) -> Self { self.next_power_of_two() }
            fn checked_next_power_of_two(self) -> Option<Self> { self.checked_next_power_of_two() }
        }
    }
}

macro_rules! impl_native_signed {
    ( $type:ident, $mask:ident ) => {
        impl_native_integer!($type, $mask);
        impl PrimitiveSigned for $type {
            fn checked_abs(self) -> Option<Self> { self.checked_abs() }
            fn wrapping_abs(self) -> Self { self.wrapping_abs() }
            fn overflowing_abs(self) -> (Self, bool) { self.overflowing_abs() }
            fn abs(self) -> Self { self.abs() }
            fn signum(self) -> Self { self.signum() }
            fn is_positive(self) -> bool { self.is_positive() }
            fn is_negative(self) -> bool { self.is_negative() }
        }
    }
}

macro_rules! impl_native_mask {
    ( $type:ident ) => {
        impl_native_bytes!($type, $type);
        impl PrimitiveMask for $type {
            fn new(x: bool) -> Self { <$type>::new(x) }
            fn test(&self) -> bool { self.test() }
        }
    }
}

impl_native_unsigned!(u8, m8);
impl_native_unsigned!(u16, m16);
impl_native_unsigned!(u32, m32);
impl_native_unsigned!(u64, m64);
impl_native_unsigned!(u128, m128);
impl_native_unsigned!(usize, msize);
impl_native_signed!(i8, m8);
impl_native_signed!(i16, m16);
impl_native_signed!(i32, m32);
impl_native_signed!(i64, m64);
impl_native_signed!(i128, m128);
impl_native_signed!(isize, msize);
impl_native_mask!(m8);
impl_native_mask!(m16);
impl_native_mask!(m32);
impl_native_mask!(m64);
impl_native_mask!(m128);
impl_native_mask!(msize);

#[cfg(test)]
mod tests {
    use super::*;
    fn min_value<T: PrimitiveInteger>() -> T { T::MIN }
    fn count_ones<T: PrimitiveInteger>(t: T) -> u32 { t.count_ones() }

    #[test]
    fn no_recursion() {
        assert_eq!(min_value::<u64>(), 0 as u64);
        assert_eq!(u64::BIT_WIDTH, 64);
        assert_eq!(u64::BYTE_WIDTH, 64);
        assert_eq!(count_ones(2 as u64), 1);
    }
}
