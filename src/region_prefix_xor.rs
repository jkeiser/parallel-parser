#[inline]
fn calc_region_prefix_xor_64_pair(mut xor: u64, mut all_ones: u64, group_size: u64) -> (u64, u64) {
    println!("Level {:2}: xor = {:064b}, all_ones = {:064b}", group_size, xor.reverse_bits(), all_ones.reverse_bits());
    println!("   << {:2}        {:064b}", group_size, (xor << group_size).reverse_bits());
    println!(  "    & all_ones   {:064b}", ((xor << group_size) & all_ones).reverse_bits());
    println!(  "    ^ xor       {:064b}", (xor.bitxor((xor << group_size) & all_ones)).reverse_bits());
    xor ^= (xor << group_size) & all_ones;
    all_ones &= all_ones << group_size;
    println!("After {:2}: xor = {:064b}, all_ones = {:064b}", group_size, xor.reverse_bits(), all_ones.reverse_bits());
    (xor, all_ones)
}

pub fn region_prefix_xor_64(bits: u64) -> (u64, bool) {
    // xor the previous bit into each bit, unless the current bit is 0. This works out to NAND.
    // let xor_2 = bits & !(bits << 1);
    // let all_ones_2 = bits & (bits << 1);
    let (xor, all_ones) = calc_region_prefix_xor_64_pair(bits, bits, 1);
    let (xor, all_ones) = calc_region_prefix_xor_64_pair(xor, all_ones, 2);
    let (xor, all_ones) = calc_region_prefix_xor_64_pair(xor, all_ones, 4);
    let (xor, all_ones) = calc_region_prefix_xor_64_pair(xor, all_ones, 8);
    let (xor, all_ones) = calc_region_prefix_xor_64_pair(xor, all_ones, 16);
    let (xor, all_ones) = calc_region_prefix_xor_64_pair(xor, all_ones, 32);
    println!("Result:   xor = {:064b}, all_ones = {:064b}", xor.reverse_bits(), all_ones.reverse_bits());
    (xor, (all_ones & 0x8000_0000_0000_0000) > 0)
}

pub fn simple_region_prefix_xor_64(bits: u64) -> (u64, bool) {
    println!("Input:  {:064b}", bits.reverse_bits());
    let mut result = 0;
    let mut prev = 0;
    for i in 0..64 {
        let mut masked = bits & (1 << i);
        if masked > 0 && prev > 0 {
            masked = 0;
        }
        result |= masked;
        prev = masked;
    }
    println!("Result: {:064b}", result.reverse_bits());
    (result, bits == 0xffff_ffff_ffff_ffff)
}

    #[test]
    fn all_one() {
        assert_eq!(region_prefix_xor_64(0xffff_ffff_ffff_ffff), (0x5555_5555_5555_5555, true))
    }
    #[test]
    fn all_zero() {
        assert_eq!(region_prefix_xor_64(0x0000_0000_0000_0000), (0x0000_0000_0000_0000, false))
    }
    #[test]
    fn every_other() {
        assert_eq!(region_prefix_xor_64(0b10101010_10101010_10101010_10101010_10101010_10101010_10101010_10101010), (0b10101010_10101010_10101010_10101010_10101010_10101010_10101010_10101010, false))
    }
    #[test]
    fn every_other_reversed() {
        assert_eq!(region_prefix_xor_64(0b01010101_01010101_01010101_01010101_01010101_01010101_01010101_01010101), (0b01010101_01010101_01010101_01010101_01010101_01010101_01010101_01010101, false))
    }
    #[test]
    fn every_other_2() {
        assert_eq!(region_prefix_xor_64(0b11001100_11001100_11001100_11001100_11001100_11001100_11001100_11001100), (0b01000100_01000100_01000100_01000100_01000100_01000100_01000100_01000100, false))
    }
    #[test]
    fn every_other_2_reversed() {
        assert_eq!(region_prefix_xor_64(0b00110011_00110011_00110011_00110011_00110011_00110011_00110011_00110011), (0b00010001_00010001_00010001_00010001_00010001_00010001_00010001_00010001, false))
    }
    #[test]
    fn every_other_3() {
        assert_eq!(region_prefix_xor_64(0b11100011_10001110_00111000_11100011_10001110_00111000_11100011_10001110), (0b10100010_10001010_00101000_10100010_10001010_00101000_10100010_10001010, false))
    }
    #[test]
    fn every_other_3_reversed() {
        assert_eq!(region_prefix_xor_64(0b01110001_11000111_00011100_01110001_11000111_00011100_01110001_11000111), (0b01010001_01000101_00010100_01010001_01000101_00010100_01010001_01000101, false))
    }

    #[test]
    fn all_one_simple() {
        assert_eq!(simple_region_prefix_xor_64(0xffff_ffff_ffff_ffff), (0x5555_5555_5555_5555, true))
    }
    #[test]
    fn all_zero_simple() {
        assert_eq!(simple_region_prefix_xor_64(0x0000_0000_0000_0000), (0x0000_0000_0000_0000, false))
    }
    // #[test]
    // fn it_works() {
    //     let s = u8x64::from(*b"1111111111111111222222222222222233333333333333334444444444444444");
    //     assert_eq!(eq_mask(s, b'2'),
    //         m8x64::new(
    //             false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,
    //             true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,
    //             false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,
    //             false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,
    //         )
    //     );
    // }
}
