
#[macro_use]
extern crate criterion;

use criterion::{Criterion,Fun};
use criterion::black_box;

use packed_simd::*;

#[cfg(target_arch = "x86_64")]
use std::arch::x86_64::*;

#[allow(overflowing_literals)]
fn criterion_benchmark(c: &mut Criterion) {
    c.bench_functions("bitmask 8x32",
        vec![
            Fun::new("packed_simd.bitmask()", |b, _| {
                let test_mask: m8x32 = m8x32::new(
                    false,false,false,false,false,false,false,false,
                    false,false,false,false,true,true,true,true,
                    true,true,true,true,false,false,false,false,
                    true,true,true,true,true,true,true,true,
                );
                b.iter(|| black_box(test_mask).bitmask());
            }),
            Fun::new("_mm256_movemask_epi8", |b, _| {
                let test_mask: __m256i = unsafe { _mm256_setr_epi8(
                    0,0,0,0,0,0,0,0,
                    0,0,0,0,0xFF,0xFF,0xFF,0xFF,
                    0xFF,0xFF,0xFF,0xFF,0,0,0,0,
                    0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
                ) };
                b.iter(|| unsafe { _mm256_movemask_epi8(black_box(test_mask)) });
            }),
        ],
        0,
    );
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);