use {
  std::cmp
};

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Copy)]
pub struct Layout {
  pub size: usize,
  pub alignment: usize,
}

impl Layout {
  pub fn new(size: usize, alignment: usize) -> Self {
    Self { size, alignment }
  }

  pub fn mul(&self, num: usize) -> Self {
    Self { size: self.size * num, alignment: self.alignment }
  }

  pub fn seq(self, other: Self) -> Self {
    let padding = other.alignment - self.size % other.alignment;
    let padding = if padding == other.alignment { 0 } else { padding };
    let size = self.size + padding + other.size;
    let alignment = cmp::max(self.alignment, other.alignment);
    Self::new(size, alignment)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
