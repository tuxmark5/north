use {
  ::core::{
    str::{from_utf8, utf8_char_width},
  },
  north_core::{
    pos::BytePos,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct DecodeUtf8<'a> {
  curr_offset: usize,
  curr_pos: BytePos,
  input_bytes: &'a [u8],
}

impl<'a> DecodeUtf8<'a> {
  pub fn new(base_pos: BytePos, input_bytes: &'a [u8]) -> Self {
    Self {
      curr_offset: 0,
      curr_pos: base_pos,
      input_bytes,
    }
  }

  pub fn decode_char(&self) -> Option<(usize, char)> {
    let curr_slice = &self.input_bytes[self.curr_offset..];
    let byte_0 = *curr_slice.get(0)?;
    let char_len = utf8_char_width(byte_0);
    let char_slice = &curr_slice[..char_len];
    let str_slice = from_utf8(char_slice).unwrap();
    let curr_char = str_slice.chars().next().unwrap();
    Some((char_len, curr_char))
  }

  pub fn current_pos(&self) -> BytePos {
    self.curr_pos
  }

  pub fn peek(&self) -> Option<char> {
    self.decode_char().map(|p| p.1)
  }
}

impl<'a> Iterator for DecodeUtf8<'a> {
  type Item = char;

  fn next(&mut self) -> Option<char> {
    let (delta, curr_char) = self.decode_char()?;
    self.curr_offset += delta;
    self.curr_pos.0 += delta as u32;
    Some(curr_char)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
