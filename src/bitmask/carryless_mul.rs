#[cfg(target_arch = "x86")]
use std::arch::x86::*;
#[cfg(target_arch = "x86_64")]
use std::arch::x86_64::*;
use packed_simd::*;
use std::mem;

pub trait CarrylessMul<T>: Sized {
    type Output;
    fn clmul(self, other: T) -> Self::Output;
}

impl CarrylessMul<u64> for u64 {
    type Output = u64x2;
    fn clmul(self, other: u64) -> Self::Output {
        let a: __m128i = unsafe { mem::transmute(u64x2::new(self, 0)) };
        let b: __m128i = unsafe { mem::transmute(u64x2::new(other, 0)) };
        let result = unsafe { _mm_clmulepi64_si128(a, b, 0) };
        unsafe { mem::transmute(result) }
    }
}
