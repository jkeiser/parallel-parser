mod parser;
mod result;

pub(crate) use parser::*;
pub use result::*;

pub fn parse_chunks(input: &mut Vec<[u8;512]>) -> JsonResult {
    JsonParser::parse_chunks(input)
}

