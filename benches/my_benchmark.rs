// // extern crate core_affinity;
// #[macro_use]
// extern crate criterion;

// #[cfg(feature = "jemallocator")]
// extern crate jemallocator;
// #[cfg(feature = "jemallocator")]
// #[global_allocator]
// static ALLOC: jemallocator::Jemalloc = jemallocator::Jemalloc;

// use criterion::{Criterion, ParameterizedBenchmark, Throughput};
// #[cfg(feature = "bench-serder")]
// use serde_json;
// use parallel_parser::parser;
// #[cfg(feature = "simd_json-rust")]
// use simd_json_rust::build_parsed_json;
// use std::fs::File;
// use std::io::Read;
// use std::default::Default;
// use std::mem;

// macro_rules! bench_file {
//     ($name:ident) => {
//         fn $name(c: &mut Criterion) {
//             // let core_ids = core_affinity::get_core_ids().unwrap();
//             // core_affinity::set_for_current(core_ids[0]);

//             let mut vec = Vec::new();
//             File::open(format!("data/jsonexamples/{}.json", stringify!($name)))
//                 .unwrap()
//                 .read_to_end(&mut vec)
//                 .unwrap();

//             let b = ParameterizedBenchmark::new(
//                 "le_parser",
//                 |b, data| {
//                     b.iter(|| {
//                         let mut overflow = Default::default();
//                         for chunk in data.chunks(512) {
//                             if chunk.len() < 512 {
//                                 let mut chunk512 = [0 as u8; 512];
//                                 chunk512[0..chunk.len()].copy_from_slice(chunk);
//                                 parser::parse_chunk(&chunk512, &mut overflow);
//                             } else {
//                                 parser::parse_chunk(unsafe { mem::transmute(&chunk) }, &mut overflow);
//                             }
//                         }
//                     })
//                 },
//                 vec![vec],
//             );
//             c.bench(
//                 stringify!($name),
//                 b.throughput(|data| Throughput::Bytes(data.len() as u32)),
//             );
//         }
//     };
// }

// bench_file!(apache_builds);
// bench_file!(canada);
// bench_file!(citm_catalog);
// bench_file!(log);
// bench_file!(twitter);

// criterion_group!(benches, apache_builds, canada, citm_catalog, log, twitter);
// criterion_main!(benches);
