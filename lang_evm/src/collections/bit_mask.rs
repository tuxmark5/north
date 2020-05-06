use {
  crate::{
    alloc::{ABox, ReverseAlloc},
  },
  std::{
    alloc::{Global},
    cmp::min,
    mem::size_of,
    ops::BitOrAssign,
  }
};

////////////////////////////////////////////////////////////////////////////////////////////////
pub type BitBlock = u64;
////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone)]
pub struct BitMask<A: ReverseAlloc = Global> {
  blocks: ABox<[BitBlock], A>
}

impl<A: ReverseAlloc> BitMask<A> {
  pub fn is_zero(&self) -> bool {
    self.blocks.iter().all(|b| *b == 0)
  }

  pub fn subtract<B: ReverseAlloc>(&mut self, other: &BitMask<B>) {
    let size = min(self.blocks.len(), other.blocks.len());
    for i in 0..size {
      self.blocks[i] &= !other.blocks[i];
    }
  }
}

impl<'a> BitOrAssign<&'a BitMask> for BitMask {
  fn bitor_assign(&mut self, other: &'a BitMask) {
    let size = min(self.blocks.len(), other.blocks.len());
    for i in 0..size {
      self.blocks[i] |= other.blocks[i];
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Default)]
pub struct BitMaskBuilder {
  blocks: Vec<u64>
}

impl BitMaskBuilder {
  pub fn new() -> Self {
    Self::default()
  }

  pub fn block_size() -> usize {
    size_of::<BitBlock>() * 8
  }

  pub fn insert(&mut self, index: usize) {
    let slot_idx = index / Self::block_size();
    let bit_idx = index % Self::block_size();

    if self.blocks.len() <= slot_idx {
      self.blocks.resize(slot_idx + 1, 0);
    }

    self.blocks[slot_idx] |= 1 << bit_idx;
  }

  pub fn into_mask<A>(mut self) -> BitMask<A> where
    A: Default + ReverseAlloc
  {
    BitMask {
      blocks: ABox::from_vec(&mut self.blocks)
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
