Tokenizer
---------

Output:

// In this scheme, arrays and hashes are flat and unmatched, meaning you have to iterate all the way through an array or hash to skip it.
// Only really good for full iteration (which is actually a typical case). For random access, you need to match.

type Chunk = [u8;512];
struct Parsed {
    tokens: Vec<Token>,
    line_starts: Vec<usize>,
    bytes: Vec<ByteChunk>,
    last_chunk_length: usize,
}
type ByteIndex = usize;
type TokenIndex = usize;
enum Token {
    ArrayStart { position: ByteIndex},
    ArrayEnd { position: ByteIndex },
    HashStart { position: ByteIndex },
    HashEnd { position: ByteIndex },
    Integer { end: ByteIndex, sign: bool },
    Float { end: ByteIndex, exp_size: u16, decimal_size: u16, exp_sign: Option<bool>, sign: Option<bool> }
    String { start_quote: ByteIndex, end_quote: ByteIndex },
    HashKey { start_quote: ByteIndex, end_quote: ByteIndex },
    True { position: ByteIndex },
    False { position: ByteIndex },
    Null { position: ByteIndex },
}

1. Recognize: token types + mask:
    a. [validate] valid utf-8 characters and multibyte
        - short-circuit ASCII
        - Invalid single bytes: C0, C1, F5-FF
        - Unexpected or insufficient continuations after 11000000, 11100000 and 11110000
        - Overlong 3 byte: E0 < 0800, ED >= D800
        - Overlong 4 byte: F0 < 10000, F4 >= 10FFFF
    c. token info: (start quote + end quote) + (true + false + null start) + (number end + exponent + sign + dot) + ({ } [ ] : ,) masks
        - escapes: escape start -> odd escapes -> escape mask
        - strings: string starts + mask
            - quote mask - escape mask -> string mask
            - [validate] valid characters inside strings
            - [validate] final character is not inside a string
            - [optional] deescaped strings: strings with backslashes dealt with internally
        - single character tokens: { } [ ] : , types + mask
        - fixed length tokens: true false null ends + types + mask
        - numbers: float / int ends + mask, exponent mask, sign mask, dot mask
            - digit = 0-9
            - decimal = <digit> . | . <digit>
            - signed = !<decimal> (+|-) <decimal>
            - exp = <signed> (e|E) <signed>
            - float vs. int: put a 1 at the start of each (<exp> &~ +/-), add it to <digit mask>, shift left, and treat the end of the mask as int if 1, or float if 0 (because it fell into a "." or "e" trap).
            - [validation] decimal after decimal (1.1.2), decimal after exponent (1e1.2):
                put a 1 at "e" or after "." and add to the non-dot mask, and see if they fall into a hole
                ((<exp> &~ <. mask>) + (<e mask> | (<. mask> >> 1)) & ~<exp>
        - token mask: single + number + string start + 
        - [validate] non-whitespace non-tokens
        - [optional] line starts: for debug info
2. Tokenize: token list
    a. tokens:
        - token types -> index map
        - list of 

8. tokens: token starts + types (string, {, }, [, ], :, true, false, null)
    - atom mask: word + number + string
    - [validate] only
    - atom starts + types: structure + word + number + string
    - token positions -> index map -> tokens
    - string 
    - token positions + index map -> tokens
    - token starts + index map -> list
    - token types + index map ->
    - token starts
    - final character is not inside } or ]
 
9. validation
    - token
Parser
------
 
1. nesting: parent types + parent levels
    - start mask = { + [
    - end mask = ] + }
    - reduce() validate pairs: slope of both sides, everything else is handled
 
8. invalid structure:
    - , outside [] or {} (parent type != [ or {)
    - : outside {} (parent type != {)
    - atom atom
    - !atom ,
    - , !atom
    - !string :
   - : !atom
8.
 
Object: ParsedJson+index
    - Type: String|Hash|Array|true|false|null
    - Index: index in file
    -
- Child(index)
 
 
slashes             11101100
cmul const          11111111 | >> const         1
= flipped           10110111 | =  prev_slashes  01110110
                             | &~ slashes       11101100
                             | =  after_slashes 00010001
& after_slashes     00010001
= flipped_regions   00010001
cmul const          11111111
= escapes           10110111
flipped
cmul flipped_regions 0001000
| const             10000000
=                   10000100
cmul flipped        10110111
=                   10110011
cmul flipped       
^ const             11111111
=                  
&~ backslashes      11101100
 
 
 
string        \\\\  \\\ \\ \\\
              1111001110110111
slashes       0111001110110111 &~ prev_is_escape
prev_slashes  0011100111011011 << 1
starts        0100001000100100 &~
even starts   0000001000100000 & const   | odd starts   01000000000000100 & const
even ends     0000000001001000 +         | odd ends     00001000000000000 +
even both     0000001001101000           | odd both     01001000000000100 |
even ranges   0000001110110000 cmul FFFF | odd ranges   01110000000000111 cmul FFFF
even escapes  0000001010100000 &         | odd escapes  01010000000000101 &
escapes       0101001010100101 |
string        \\\\  \\\ \\ \\
 
serial: 14 ops (2 adds, 2 cmul)
parallel: 9 ops (1 add, 1 cmul)
 
--------------------------------
 
string        \\\\  \\\ \\ \\\
              1111001110110111
slashes       0111001110110111 &~ prev_is_escape
prev_slashes  0011100111011011 << 1
starts        0100001000100100 &~
even starts   0000001000100000 & const   | odd starts   01000000000000100 & const
even ends     0000000001001000 +         | odd ends     00001000000000000 +
even escaped  0000000001000000 & const   | odd escaped  00001000000000000 & const
local escaped 0000100001000000 |
escaped       1000100001000000 | prev_is_escape
string        \\\\  \\\ \\ \\
 
serial: 12 ops (2 adds)
parallel: 9 ops (1 add)
 
--------------------------------
 
string        \\\\  \\\ \\ \\\
PHASE ONE: escape region + last_is_escaped (last_is_escaped shared with next bit).
              1111001110110111
<parallel> pull backslash from neighbor, bring in prev_is_escape from last batch into lowest slash batch: see https://stackoverflow.com/questions/25248766/emulating-shifts-on-32-bytes-with-avx
slashes       0111001110110111 &~ prev_is_escape
prev_slashes  0011100111011011 << 1
starts        0100001000100100 &~
even starts   0000001000100000 & const   | odd starts   01000000000000100 & const
<parallel> test registers for overflow and for FFFFFFFFFFFFFFFF, increment registers per result:
<parallel>    FFFF    01110100 01110100
<parallel>    overflo 11110100 00001000
<parallel>            11110100 01111100
<parallel>    >>      01111010
<parallel>    desired 00001010 ^
<parallel>    increment above lanes
even ends     0000000001001000 +         | odd ends     00001000000000000 +
even escaped  0000000001000000 & const   | odd escaped  00001000000000000 & const
local escaped 0000100001000000 |
with escaped  0111101111110111 |
escape region 1111101111110111 | prev_is_escape
 
serial (64): 13 ops (2 adds)
parallel (64): 10 ops (1 add)
serial (512): 21 ops (2 adds, 1 inc)
parallel (512): 12 ops (1 add, 1 inc)
 
 
PHASE TWO: cmul
PHASE TWO:
escapes       0101001010100101 cmul FFFF
<parallel>    8 separate cmuls
 
serial: 12 ops (2 adds, 1 cmul)
parallel: 9 ops (1 add, 1 cmul)
superparallel (8 at a time): 27 ops (2 adds, 8 cmul) - 2 extra ops for pull backslash from neighbor, 3 extra normal ops per add, 7 extra cmuls
superparallel (8 at a time) parallel: 14 ops (1 add, 1 cmul)
 
uberparallel (*): need to know last backslash and even/odd carry, from previous chunk.
 
NOTE: can be done 8 together except clmul, and will be more efficient (no load-into-SSE-register instruction)
add doesn't cross the boundary, but we can add the carry from each operation into the next until there are no more carries (up to 7 times). "while" is fine for the last 6 of these, it'll basically never go more than once (requires 64 backslashes).
clmul crosses the boundary, however, and thus we can XOR it into the results cumulatively, allowing all the other operations to be treated as a single unit
 
escape1 =
