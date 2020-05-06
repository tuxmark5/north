use {
  crate::{
    alloc::{FreeList, FreeListDealloc, ReverseAlloc},
  },
  north_core::{
    quote::{QuoteType, RustQuoter, rust_type::Type},
  },
  std::{
    alloc::{Alloc, AllocErr, Layout},
    mem::{size_of},
    pin::Pin,
    ptr::{NonNull},
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////
const WORD_SIZE: usize = size_of::<usize>();
////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Fridge<A: Alloc> {
  crate pools: Box<[Pin<Box<FreeList<A>>>]>,
}

impl<A> Fridge<A> where
  A: Alloc + Clone
{
  pub fn new(parent_alloc: A) -> Self {
    let mut pools = Vec::new();

    for i in 0..16 {
      let elem_size = i * WORD_SIZE;
      let pool = FreeList::new(elem_size, parent_alloc.clone());
      pools.push(pool);
    }

    Self { 
      pools: pools.into_boxed_slice(),
    }
  }

  pub fn dump_info(&self) {
    for pool in self.pools.iter() {
      pool.dump_info();
    }
  }
}

unsafe impl<A: Alloc> Alloc for Fridge<A> {
  unsafe fn alloc(&mut self, layout: Layout) -> Result<NonNull<u8>, AllocErr> {
    let pool_idx = layout.size() / WORD_SIZE;
    let pool = self.pools[pool_idx].as_mut().get_unchecked_mut();
    pool.alloc(layout)
  }

  unsafe fn dealloc(&mut self, ptr: NonNull<u8>, layout: Layout) {
    let pool_idx = layout.size() / WORD_SIZE;
    let pool = &mut self.pools[pool_idx].as_mut().get_unchecked_mut();
    pool.dealloc(ptr, layout)
  }
}

impl<A: Alloc + Clone + Default> Default for Fridge<A> {
  fn default() -> Self {
    Self::new(<_>::default())
  }
}

impl<A: 'static + Alloc> QuoteType for Fridge<A> {
  fn quote(_q: &mut RustQuoter) -> Type {
    Type::Unit
  }
}

impl<A: Alloc> ReverseAlloc for Fridge<A> {
  type Dealloc = FreeListDealloc<A>;
}

////////////////////////////////////////////////////////////////////////////////////////////////
