use std;
use std::fmt::*;
use std::hash::*;
use std::num::*;
use std::ops::*;
use std::result::Result;
use std::str::*;

pub trait NativeInteger:
    Copy + Clone + Default + Hash + FromStr +
    Eq + Ord +
    LowerHex + UpperHex + Octal + Binary + Debug + Display +
    Add<Self, Output=Self> + Sub<Self, Output=Self> + Div<Self> + Rem<Self, Output=Self> + Mul<Self, Output=Self> +
    BitAnd<Self, Output=Self> + BitXor<Self, Output=Self> + BitOr<Self, Output=Self> + Not<Output=Self> +
    Shl<Self, Output=Self> + Shr<Self, Output=Self> +
    Send + Sync {
    const MAX: Self;
    const MIN: Self;
    const WIDTH: u32;
    const NUM_BYTES: usize = Self::WIDTH as usize / 8;
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
    // fn to_be_bytes(self) -> [u8; Self::NUM_BYTES];;
    // fn to_le_bytes(self) -> [u8; Self::NUM_BYTES];
    // fn to_ne_bytes(self) -> [u8; Self::NUM_BYTES];
    // fn from_be_bytes(bytes: [u8; Self::NUM_BYTES]) -> Self;
    // fn from_le_bytes(bytes: [u8; Self::NUM_BYTES]) -> Self;
    // fn from_ne_bytes(bytes: [u8; Self::NUM_BYTES]) -> Self;
}

pub trait NativeUnsigned: NativeInteger {
    fn is_power_of_two(self) -> bool;
    fn next_power_of_two(self) -> Self;
    fn checked_next_power_of_two(self) -> Option<Self>;
}

pub trait NativeSigned: NativeInteger {
    fn checked_abs(self) -> Option<Self>;
    fn wrapping_abs(self) -> Self;
    fn overflowing_abs(self) -> (Self, bool);
    fn abs(self) -> Self;
    fn signum(self) -> Self;
    fn is_positive(self) -> bool;
    fn is_negative(self) -> bool;
}

macro_rules! impl_native_integer {
    ( $type:ident ) => {
        use std::$type;
        impl NativeInteger for $type {
            const MAX: Self = $type::MAX;
            const MIN: Self = $type::MIN;
            const WIDTH: u32 = $type::MAX.count_ones();
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
    ( $type:ident ) => {
        impl_native_integer!($type);
        impl NativeUnsigned for $type {
            fn is_power_of_two(self) -> bool { self.is_power_of_two() }
            fn next_power_of_two(self) -> Self { self.next_power_of_two() }
            fn checked_next_power_of_two(self) -> Option<Self> { self.checked_next_power_of_two() }
        }
    }
}

macro_rules! impl_native_signed {
    ( $type:ident ) => {
        impl_native_integer!($type);
        impl NativeSigned for $type {
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

impl_native_unsigned!(u8);
impl_native_unsigned!(u16);
impl_native_unsigned!(u32);
impl_native_unsigned!(u64);
impl_native_unsigned!(u128);
impl_native_signed!(i8);
impl_native_signed!(i16);
impl_native_signed!(i32);
impl_native_signed!(i64);
impl_native_signed!(i128);

#[cfg(test)]
mod tests {
    use super::NativeInteger;
    fn min_value<T: NativeInteger>() -> T { T::MIN }
    fn count_ones<T: NativeInteger>(t: T) -> u32 { t.count_ones() }

    #[test]
    fn no_recursion() {
        assert_eq!(min_value::<u64>(), 0 as u64);
        assert_eq!(<u64 as NativeInteger>::WIDTH, 64);
        assert_eq!(count_ones(2 as u64), 1);
    }
}
