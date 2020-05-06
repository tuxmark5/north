use {
  std::{
    alloc::{AllocRef, AllocInit, Global, Layout},
    ptr::{NonNull},
  },
};


////////////////////////////////////////////////////////////////////////////////////////////////

pub mod abox;
pub mod free_list;
pub mod fridge;
pub mod scoped_alloc;

pub use self::{
  abox::{ABox},
  free_list::{FreeList, FreeListDealloc},
  fridge::{Fridge},
  scoped_alloc::{ScopedAlloc},
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait Dealloc {
  unsafe fn alloc_again(ptr: NonNull<u8>, layout: Layout) -> NonNull<u8>;
  unsafe fn dealloc(ptr: NonNull<u8>, layout: Layout);
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait ReverseAlloc: AllocRef {
  type Dealloc: Dealloc;
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct GlobalDealloc { }

impl Dealloc for GlobalDealloc {
  unsafe fn alloc_again(_ptr: NonNull<u8>, layout: Layout) -> NonNull<u8> {
    let mut global = Global::default();
    global.alloc(layout, AllocInit::Uninitialized).unwrap().ptr
  }

  unsafe fn dealloc(ptr: NonNull<u8>, layout: Layout) {
    let mut global = Global::default();
    global.dealloc(ptr, layout);
  }
}

impl ReverseAlloc for Global {
  type Dealloc = GlobalDealloc;
}

////////////////////////////////////////////////////////////////////////////////////////////////
