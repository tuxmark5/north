use {
  crate::{
    runtime::PosId,
  },
  north_core::{
    quote::{
      QuoteType, RustQuoter,
      rust_type::{Type},
    },
  },
  std::{
    ptr, slice,
    fmt::{self, Debug},
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Copy)]
#[repr(C)]
pub struct Cursor {
  pub start: *const u8,
  pub end: *const u8,
}

impl Cursor {
  pub fn empty() -> Self {
    Self {
      start: ptr::null(),
      end: ptr::null(),
    }
  }

  pub fn new(bytes: &[u8]) -> Self {
    Self {
      start: bytes.as_ptr(),
      end: unsafe { bytes.as_ptr().add(bytes.len()) },
    }
  }

  pub fn adjust_begin(&mut self, pos: PosId) -> *const u8 {
    let delta = pos.idx() as isize;
    self.start = unsafe { self.start.offset(delta) };
    self.start
  }

  pub fn adjust_end(&self, old_start: *const u8, pos: &mut PosId) {
    let size = unsafe { self.start.offset_from(old_start) };
    *pos = pos.advance(size as u32);
  }

  pub fn advance(&mut self) {
    self.start = unsafe { self.start.offset(1) };
  }

  pub fn len(&self) -> u32 {
    self.end.wrapping_offset_from(self.start) as u32
  }

  pub fn next(&mut self) -> Option<u8> {
    let result = self.peek();
    self.advance();
    result
  }

  pub fn peek(&self) -> Option<u8> {
    if self.start < self.end {
      unsafe { Some(*self.start) }
    } else {
      None
    }
  }

  pub fn to_slice(&self) -> &str {
    let len = self.end.wrapping_offset_from(self.start) as usize;
    let slice = unsafe { slice::from_raw_parts(self.start, len) };
    std::str::from_utf8(slice).unwrap_or("<utf-8 decode error>")
  }
}

impl Debug for Cursor {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "<{}>", self.to_slice())
  }
}

impl QuoteType for Cursor {
  fn quote(q: &mut RustQuoter) -> Type {
    Type::make_struct(vec![
      q.quote::<*const u8>(),
      q.quote::<*const u8>(),
    ])
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////