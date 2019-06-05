
#[macro_use]
extern crate criterion;

use criterion::Criterion;
use criterion::black_box;

use packed_simd::*;

#[cfg(target_arch = "x86")]
use std::arch::x86::*;
#[cfg(target_arch = "x86_64")]
use std::arch::x86_64::*;

#[bench]
fn bench_bitmask(b: &mut Bencher) {
    let test_mask = m8x32::new(
        false,false,false,false,false,false,false,false,
        false,false,false,false,true,true,true,true,
        true,true,true,true,false,false,false,false,
        true,true,true,true,true,true,true,true,
    );
    let compare_to: u32 = 0xff0ff000;
    b.iter(|| assert_eq!(test_mask.bitmask(), compare_to))
}
#[bench]
#[allow(overflowing_literals)]
fn bench_better_bitmask(b: &mut Bencher) {
    let test_mask = unsafe { _mm256_setr_epi8(
        0,0,0,0,0,0,0,0,
        0,0,0,0,-1,-1,-1,-1,
        -1,-1,-1,-1,0,0,0,0,
        -1,-1,-1,-1,-1,-1,-1,-1,
    ) };
    let compare_to = 0xff0ff000 as i32;
    b.iter(|| assert_eq!(unsafe { _mm256_movemask_epi8(test_mask) }, compare_to))
}

#[allow(overflowing_literals)]
fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("packed_simd", |b| {
        let test_mask = m8x32::new(
            false,false,false,false,false,false,false,false,
            false,false,false,false,true,true,true,true,
            true,true,true,true,false,false,false,false,
            true,true,true,true,true,true,true,true,
        );
        let compare_to: u32 = 0xff0ff000;
        b.iter(|| assert_eq!(black_box(test_mask.bitmask()), compare_to))
    });
    c.bench_function("_mm256_movemask_epi8", |b| {
        let test_mask = unsafe { _mm256_setr_epi8(
            0,0,0,0,0,0,0,0,
            0,0,0,0,-1,-1,-1,-1,
            -1,-1,-1,-1,0,0,0,0,
            -1,-1,-1,-1,-1,-1,-1,-1,
        ) };
        let compare_to = 0xff0ff000 as i32;
        b.iter(|| assert_eq!(unsafe { _mm256_movemask_epi8(black_box(test_mask)) }, compare_to))
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);