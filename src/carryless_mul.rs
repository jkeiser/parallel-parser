#[cfg(target_arch = "x86")]
use std::arch::x86::*;
#[cfg(target_arch = "x86_64")]
use std::arch::x86_64::*;
use packed_simd::*;
use std::mem;

pub trait CarrylessMul: Sized {
    type ExpandedOutput;
    fn clmul(self, other: Self) -> Self;
    fn clmul_expand(self, other: Self) -> Self::ExpandedOutput;
}

impl CarrylessMul for u64 {
    type ExpandedOutput = u64x2;
    fn clmul(self, other: Self) -> Self {
        self.clmul_expand(other).extract(0)
    }
    fn clmul_expand(self, other: Self) -> Self::ExpandedOutput {
        let a: __m128i = unsafe { mem::transmute(u64x2::new(self, 0)) };
        let b: __m128i = unsafe { mem::transmute(u64x2::new(other, 0)) };
        let result: u64x2 = unsafe { mem::transmute(_mm_clmulepi64_si128(a, b, 0)) };
        result
    }
}

