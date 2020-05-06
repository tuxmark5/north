use {
  std::{
    marker::Unsize,
    mem::{self, size_of, transmute_copy},
    ops::{CoerceUnsized, Deref, DerefMut},
    ptr::drop_in_place,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

const MAX_INLINE_SIZE: usize = size_of::<InlineData>();

type InlineData = [usize; 2];

////////////////////////////////////////////////////////////////////////////////////////////////

pub enum SmallBox<T: ?Sized> {
  Inline(InlineData),
  Heap(Box<T>),
}

impl<T> SmallBox<T> {
  pub fn new(value: T) -> Self {
    if size_of::<T>() <= MAX_INLINE_SIZE {
      unsafe {
        let data = transmute_copy::<_, InlineData>(&value);
        mem::forget(value);
        SmallBox::Inline(data)
      }
    } else {
      SmallBox::Heap(box value)
    }
  }

  pub unsafe fn cast_unchecked<U>(&self) -> &U {
    match self {
      SmallBox::Inline(ref data) => unsafe { transmute_copy::<_, &U>(&data) },
      SmallBox::Heap(ref heap) => unsafe { transmute_copy::<_, &U>(heap) },
    }
  }

  pub fn up<U: ?Sized>(self) -> SmallBox<U> where
    T: Unsize<U>
  {
    match self {
      SmallBox::Inline(ref data) => {
        let value = unsafe { transmute_copy::<_, U>(data) };
        SmallBox::new(value)
      },

      SmallBox::Heap(heap) => {
        SmallBox::Heap(heap)
      },
    }
  }

  /*pub fn unbox(self) -> T {
    match self {
      SmallBox::Inline(data) => unsafe { transmute_copy::<_, T>(&data) },
      SmallBox::Heap(box heap) => {
        let result = heap;
        mem::forget(self);
        result
      },
    }
  }*/
}

impl<T: ?Sized> Deref for SmallBox<T> {
  type Target = T;

  fn deref(&self) -> &Self::Target {
    match self {
      SmallBox::Inline(ref data) => unsafe { transmute_copy::<_, &T>(&data) },
      SmallBox::Heap(heap) => heap.deref(),
    }
  }
}

impl<T: ?Sized> DerefMut for SmallBox<T> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    match self {
      SmallBox::Inline(ref mut data) => unsafe { transmute_copy::<_, &mut T>(&data) },
      SmallBox::Heap(heap) => heap.deref_mut(),
    }
  }
}

impl<T: ?Sized> Drop for SmallBox<T> {
  fn drop(&mut self) {
    if let SmallBox::Inline(ref mut data) = self {
      unsafe {
        let value = transmute_copy::<_, *mut T>(&data);
        drop_in_place(value);
      }
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
  use super::SmallBox;
  use std::{
    ops::{Deref, DerefMut},
    rc::Rc,
  };

  #[test]
  fn test_inline_deref() {
    let mut value = SmallBox::new(10);
    assert_eq!(*value.deref(), 10);
    assert_eq!(*value.deref_mut(), 10);
  }

  #[test]
  fn test_inline_drop() {
    let ptr = Rc::new(10);
    assert_eq!(Rc::strong_count(&ptr), 1);
    {
      let value = SmallBox::new(ptr.clone());
      assert_eq!(Rc::strong_count(&ptr), 2);
    }
    assert_eq!(Rc::strong_count(&ptr), 1);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
