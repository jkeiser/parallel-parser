pub trait Bitmask: Sized {
    const SIZE: u32;
    const ALL_BITS: Self;
    const NO_BITS: Self;
    const EVEN_BITS: Self;
    const ODD_BITS: Self;
    const TOP_BIT: Self;
}

macro_rules! impl_bitmask_up_to_128 {
    ($($type:ident),+) => {
        $(
            impl Bitmask for $type {
                const SIZE: u32 = Self::NO_BITS.count_zeros();
                const NO_BITS: Self = 0 as Self;
                const ALL_BITS: Self = !Self::NO_BITS;
                const EVEN_BITS: Self = (0b01010101_01010101_01010101_01010101_01010101_01010101_01010101_01010101_01010101_01010101_01010101_01010101_01010101_01010101_01010101_01010101 & Self::ALL_BITS as u128) as Self;
                const ODD_BITS: Self = !Self::EVEN_BITS;
                const TOP_BIT: Self = (1 as Self) << (Self::SIZE - 1);
            }
        )+
    }
}

impl_bitmask_up_to_128! { u8,u16,u32,u64,u128,usize,i8,i16,i32,i64,i128,isize }

