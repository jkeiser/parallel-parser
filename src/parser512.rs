use crate::parser64;
use packed_simd::*;

pub type Bitmask = u64x8;
pub type Chunks = [u8x64;NUM_CHUNKS];
pub type Bytes = [u8;BYTE_WIDTH];
pub const BYTES_PER_CHUNK: usize = 64;
pub const NUM_CHUNKS: usize = 8;
pub const BYTE_WIDTH: usize = NUM_CHUNKS*BYTES_PER_CHUNK;

pub struct BlockData {
    pub escape_mask: Bitmask,
    pub string_mask: Bitmask,
    pub first_character_is_escaped: bool,
}
pub struct NextData {
    pub still_in_string: bool,
    pub next_character_is_escaped: bool,
}

pub fn parse(input: &Chunks, next_data: NextData) -> (BlockData, NextData) {
    let (escape_mask, first_character_is_escaped, next_character_is_escaped) = find_backslashes(input, &next_data);
    (BlockData { escape_mask, string_mask: u8x64::splat(0), first_character_is_escaped }, NextData { next_character_is_escaped, still_in_string: false })
}

pub fn backslash_mask(input: &Chunks) -> Bitmask {
    // Make the backslash mask by SIMD eq'ing each 64 byte chunk of input.
    let backslash_mask: &[parser64::Bitmask;NUM_CHUNKS] = input.map(|input64| parser64::backslash_mask(input64).collect());
    backslash_mask as Bitmask
}


fn find_backslashes(input: &Chunks, next_data: NextData) -> (BlockData, NextData) {
    let first_character_is_escaped = next_data.next_character_is_escaped as u64;

    // Make the backslash mask by SIMD eq'ing each 64 byte chunk of input.
    let backslash_mask: Bitmask = backslash_mask;
    // If the first character is escaped, pretend it's not a backslash. We'll fix it in post.
    backslash_mask.set(0, backslash_mask.extract(0) & !next_data.next_character_is_escaped);
    
    // Find the position of escaped characters (the character after any odd-length series of backslashes)
    let (escape_mask, next_character_is_escaped) = escape_mask(backslash_mask);
    // Denote that the first character is escaped (if it is supposed to be)
    escape_mask.set(0, escape_mask.extract(0) | first_character_is_escaped);
}

fn escape_mask(backslash_mask: Bitmask) -> (Bitmask, bool) {
    // Find the first backslash in each series of backslashes
    let escape_starts = backslash_mask & !(backslash_mask << 1);

    // Find out where runs of bits end by adding the starts to the bits, causing addition to overflow.
    // e.g.
    //      00011100011110010 (bits)
    //      00010000010000010 (run starts)
    //      00000010000001001 (run ends)
    //
    // We actually do this separately for runs that start on even indices, and runs that start
    // on odd indices. (We'll use this to check for odd vs. even length later). In this case
    // only the runs we want actually get a carry, so we have to mask out the original bits
    // to get rid of the runs we didn't check. i.e.:
    //      00011100011110010 (bits)
    //      00000000000000010 (even starts)
    //      00011100011110001 (even sum)
    //      00000000000000001 (even ends)
    let (even_sum, _) = backslash_mask.overflowing_add(escape_starts & u64::EVEN_BITS);
    let (odd_sum, odd_overflow) = backslash_mask.overflowing_add(escape_starts & u64::ODD_BITS);

    // To find runs with odd length, we find even runs that ended on odd bits (and vice versa): 
    //      00011100011110010 (bits)
    //      00000000000000001 (even ends)
    //      00000000000000001 (even ends on odd bits)
    //      00000010000001000 (odd ends)
    //      00000010000000000 (odd ends on even bits)
    //      00000010000000001 (odd length ends)
    let escaped_characters = ((even_sum & u64::ODD_BITS) | (odd_sum & u64::EVEN_BITS)) & !backslash_mask;

    // Overflow from odd_carries means an odd backslash run goes all the way to the end of the
    // input.
    let next_character_is_escaped = odd_overflow;

    println!("{:30}: {:064b}", "backslash_mask", backslash_mask.reverse_bits());
    println!("{:30}: {:064b}", "escaped_characters", escaped_characters.reverse_bits());
    let escape_mask = backslash_mask | escaped_characters;
    (escape_mask, next_character_is_escaped)
}
