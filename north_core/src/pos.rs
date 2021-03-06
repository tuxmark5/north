////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Copy, Clone)]
pub struct BytePos(pub u32);

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Copy, Clone)]
pub struct Span {
  lo: BytePos,
  hi: BytePos,
}

impl Span {
  pub fn new(lo: BytePos, hi: BytePos) -> Self {
    Self { lo, hi }
  }

  pub fn empty() -> Self {
    Self::new(BytePos(0), BytePos(0))
  }

  pub fn hi(&self) -> BytePos {
    self.hi
  }

  pub fn lo(&self) -> BytePos {
    self.lo
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
