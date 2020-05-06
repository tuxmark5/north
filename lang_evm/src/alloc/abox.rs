use {
  crate::{
    alloc::{Dealloc, ReverseAlloc},
  },
  std::{
    alloc::{AllocInit, Global, Layout},
    fmt::{self, Debug},
    marker::{PhantomData},
    mem::{forget},
    ops::{Deref, DerefMut},
    ptr::{self, NonNull, drop_in_place, read, write},
    slice,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

// #[fundamental]
pub struct ABox<T, A = Global> where
  T: ?Sized, A: ReverseAlloc
{
  crate ptr: NonNull<T>,
  crate alloc_type: PhantomData<A>,
}

impl<T, A> ABox<T, A> where
  A: ReverseAlloc
{
  pub fn new_in(value: T, alloc: &mut A) -> Self {
    let layout = Layout::new::<T>();
    let init = AllocInit::Uninitialized;
    let block = alloc.alloc(layout, init).unwrap();
    let mut ptr = block.ptr.cast::<T>();
    unsafe { write(ptr.as_mut(), value); }
    Self { ptr, alloc_type: PhantomData }
  }

  pub fn unbox(mut self) -> T {
    let ptr = self.ptr.as_ptr();
    unsafe {
      let value = read(ptr);
      self.dealloc();
      forget(self);
      value
    }
  }
}

impl<T, A> ABox<T, A> where
  T: ?Sized, A: ReverseAlloc
{
  pub fn as_ptr(&self) -> *mut T {
    self.ptr.as_ptr()
  }

  pub unsafe fn clone_bits(&self) -> Self {
    let layout = Layout::for_value(&**self);
    let src = self.ptr.cast();
    let dst = A::Dealloc::alloc_again(src, layout);
    ptr::copy_nonoverlapping(src.as_ptr(), dst.as_ptr(), layout.size());

    Self { 
      ptr: attach_vtable(self.ptr, dst), 
      alloc_type: PhantomData 
    }
  }

  pub unsafe fn dealloc(&mut self) {
    let ptr = self.ptr.cast::<u8>();
    let layout = Layout::for_value(&**self);
    A::Dealloc::dealloc(ptr, layout);
  }

  pub unsafe fn from_box(box_: Box<T>) -> Self {
    let raw = Box::into_raw(box_);
    Self::from_raw(raw)
  }

  pub unsafe fn from_raw(raw: *mut T) -> Self {
    let ptr = NonNull::new(raw).unwrap();
    Self { ptr, alloc_type: PhantomData }
  }

  pub unsafe fn into_box(self_: Self) -> Box<T> {
    let ptr = self_.ptr.as_ptr();
    forget(self_);
    Box::from_raw(ptr)
  }
}

impl<T, A> ABox<[T], A> where
  A: ReverseAlloc
{
  pub fn from_vec_in(vec: &mut Vec<T>, alloc: &mut A) -> Self {
    unsafe {
      let size = vec.len();
      let layout = Layout::array::<T>(size).unwrap();
      let block = alloc.alloc(layout, AllocInit::Uninitialized).unwrap();
      let ptr = block.ptr.cast::<T>();

      ptr::copy_nonoverlapping(vec.as_ptr(), ptr.as_ptr(), size);
      vec.set_len(0);

      Self { 
        ptr: make_slice_ptr(ptr, size),
        alloc_type: PhantomData
      }
    }
  }
}

impl<T, A> ABox<[T], A> where
  A: Default + ReverseAlloc
{
  pub fn from_vec(vec: &mut Vec<T>) -> Self {
    let mut alloc = <A>::default();
    Self::from_vec_in(vec, &mut alloc)
  }
}

impl<T, A> Clone for ABox<T, A> where
  T: Clone, A: ReverseAlloc
{
  fn clone(&self) -> Self {
    unimplemented!()
  }
}

impl<T, A> Clone for ABox<[T], A> where
  T: Clone, A: ReverseAlloc
{
  fn clone(&self) -> Self {
    unsafe {
      let layout = Layout::for_value(&**self);
      let ptr = A::Dealloc::alloc_again(self.ptr.cast(), layout);
      clone_into_uninit(self, ptr.cast().as_ptr());
      let ptr = make_slice_ptr(ptr.cast(), self.len());
      Self { ptr, alloc_type: PhantomData }
    }
  }
}

impl<T, A> Debug for ABox<T, A> where
  T: Debug + ?Sized, A: ReverseAlloc
{
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    self.deref().fmt(f)
  }
}

impl<T, A> Deref for ABox<T, A> where
  T: ?Sized, A: ReverseAlloc
{
  type Target = T;

  fn deref(&self) -> &T {
    unsafe { self.ptr.as_ref() }
  }
}

impl<T, A> DerefMut for ABox<T, A> where
  T: ?Sized, A: ReverseAlloc
{
  fn deref_mut(&mut self) -> &mut T {
    unsafe { self.ptr.as_mut() }
  }
}

impl<T, A> Drop for ABox<T, A> where
  T: ?Sized, A: ReverseAlloc
{
  fn drop(&mut self) {
    unsafe {
      drop_in_place(self.ptr.as_ptr());
      self.dealloc();
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

unsafe fn attach_vtable<T: ?Sized>(src: NonNull<T>, ptr: NonNull<u8>) -> NonNull<T> {
  union Caster<A: ?Sized, B> { ptr_a: NonNull<A>, ptr_b: NonNull<B> }
  let mut caster = Caster { ptr_a: src };
  caster.ptr_b = ptr;
  caster.ptr_a
}

unsafe fn clone_into_uninit<T: Clone>(src: &[T], dst: *mut T) {
  for (i, elem) in src.iter().enumerate() {
    ptr::write(dst.offset(i as isize), elem.clone());
  }
}

unsafe fn make_slice_ptr<T>(ptr: NonNull<T>, len: usize) -> NonNull<[T]> {
  let slice = slice::from_raw_parts_mut(ptr.as_ptr(), len);
  NonNull::new_unchecked(slice)
}

////////////////////////////////////////////////////////////////////////////////////////////////
