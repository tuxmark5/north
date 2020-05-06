use {
  crate::{
    alloc::ReverseAlloc,
  },
  std::{
    alloc::{AllocErr, AllocInit, AllocRef, Layout, MemoryBlock},
    cell::{RefCell},
    marker::PhantomData,
    mem::replace,
    ptr::{NonNull},
    rc::{Rc},
  }
};

////////////////////////////////////////////////////////////////////////////////////////////////
#[thread_local]
static mut SCOPED_ALLOC: Option<NonNull<()>> = None;
////////////////////////////////////////////////////////////////////////////////////////////////

pub struct ScopedAlloc<A> {
  alloc_type: PhantomData<A>
}

impl<A> ScopedAlloc<A> {
  pub fn get_mut<'a>() -> &'a mut A {
    unsafe {
      let ptr = SCOPED_ALLOC.unwrap().cast::<A>();
      &mut *ptr.as_ptr()
    }
  }

  pub fn with<F, R>(alloc: &mut A, f: F) -> R where
    F: FnOnce() -> R
  {
    unsafe {
      let new_alloc = NonNull::new(alloc).unwrap().cast();
      let old_alloc = replace(&mut SCOPED_ALLOC, Some(new_alloc));
      let result = f();
      SCOPED_ALLOC = old_alloc;
      result
    }
  }

  pub fn with_cell<F, R>(alloc: Rc<RefCell<A>>, f: F) -> R where
    F: FnOnce() -> R
  {
    let mut alloc = alloc.borrow_mut();
    Self::with(&mut *alloc, f)
  }
}

unsafe impl<A> AllocRef for ScopedAlloc<A> where
  A: AllocRef
{
  fn alloc(&mut self, layout: Layout, init: AllocInit) -> Result<MemoryBlock, AllocErr> {
    let alloc = Self::get_mut();
    alloc.alloc(layout, init)
  }

  unsafe fn dealloc(&mut self, ptr: NonNull<u8>, layout: Layout) {
    let alloc = Self::get_mut();
    alloc.dealloc(ptr, layout)
  }
}

impl<A> Clone for ScopedAlloc<A> {
  fn clone(&self) -> Self {
    Self { alloc_type: PhantomData }
  }
}

impl<A> Default for ScopedAlloc<A> {
  fn default() -> Self {
    Self { alloc_type: PhantomData }
  }
}

impl<A: ReverseAlloc> ReverseAlloc for ScopedAlloc<A> {
  type Dealloc = A::Dealloc;
}

////////////////////////////////////////////////////////////////////////////////////////////////
