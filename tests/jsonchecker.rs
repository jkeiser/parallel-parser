// use parallel_parser::parser;
// use std::fs::File;
// use std::io::Read;
// use std::mem;

// macro_rules! check_json {
//     ($file:ident) => {
//         #[test]
//         fn $file() {
//             let mut data = Vec::new();
//             let f = format!("data/jsonchecker/{}.json", stringify!($name));
//             File::open(f).unwrap().read_to_end(&mut data).unwrap();
//             let mut overflow = Default::default();
//             for chunk in data.chunks(512) {
//                 if chunk.len() < 512 {
//                     let mut chunk512 = [0 as u8; 512];
//                     chunk512[0..chunk.len()].copy_from_slice(chunk);
//                     parser::parse_chunk(&chunk512, &mut overflow);
//                 } else {
//                     parser::parse_chunk(unsafe { mem::transmute(&chunk) }, &mut overflow);
//                 }
//             }
//         }
//     };
// }

// check_json!(pass01);
// check_json!(pass02);
// check_json!(pass03);
// check_json!(pass04);
// check_json!(pass05);
// check_json!(pass06);
// check_json!(pass07);
// check_json!(pass08);
// check_json!(pass09);
// check_json!(pass10);
// check_json!(pass11);
// check_json!(pass12);
// check_json!(pass13);
// check_json!(pass14);
// check_json!(pass15);
// check_json!(pass16);


// check_json!(fail01);
// check_json!(fail02);
// check_json!(fail03);
// check_json!(fail04);
// check_json!(fail05);
// check_json!(fail06);
// check_json!(fail07);
// check_json!(fail08);
// check_json!(fail09);
// check_json!(fail10);
// check_json!(fail11);
// check_json!(fail12);
// check_json!(fail13);
// check_json!(fail14);
// check_json!(fail15);
// check_json!(fail16);
// check_json!(fail17);
// check_json!(fail18);
// check_json!(fail19);
// check_json!(fail20);
// check_json!(fail21);
// check_json!(fail22);
// check_json!(fail23);
// check_json!(fail24);
// check_json!(fail25);
// check_json!(fail26);
// check_json!(fail27);
// check_json!(fail28);
// check_json!(fail29);
// check_json!(fail30);
// check_json!(fail31);
// check_json!(fail32);
// check_json!(fail33);
// check_json!(fail34);
// check_json!(fail35);
// check_json!(fail36);
// check_json!(fail37);
// check_json!(fail38);
// check_json!(fail39);
// check_json!(fail40);
// check_json!(fail41);
// check_json!(fail42);
// check_json!(fail43);
// check_json!(fail44);
// check_json!(fail45);
// check_json!(fail46);
// check_json!(fail47);
// check_json!(fail48);
// check_json!(fail49);
// check_json!(fail50);
// check_json!(fail51);
// check_json!(fail52);
// check_json!(fail53);
// check_json!(fail54);
// check_json!(fail55);
// check_json!(fail56);
// check_json!(fail57);
// check_json!(fail58);
// check_json!(fail59);
// check_json!(fail60);
// check_json!(fail61);
// check_json!(fail62);
// check_json!(fail63);
// check_json!(fail64);
// check_json!(fail65);
