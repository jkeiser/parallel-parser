mod parser;
mod result;

pub(crate) use parser::*;
pub use result::*;

pub fn parse_chunks(input: Vec<[u8;512]>) -> JsonResult {
    let mut parser = JsonParser::default();
    let mut chunk = JsonChunk::default();
    for chunk_index in 0..input.len() {
        chunk = chunk.next(&input[chunk_index], chunk_index);
        parser.parse(&mut chunk);
    }
    parser.finish(&mut chunk);
    chunk.finish()
}
