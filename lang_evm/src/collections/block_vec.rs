use {
  crate::{
    collections::Slot,
  },
  std::{
    cmp::Ordering,
    marker::{PhantomData},
    ptr::{self},
  },
  std_alloc::{
    raw_vec::{RawVec},
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Block<T> {
  entries: RawVec<T>
}

impl<T> Block<T> {
  crate fn new() -> Self {
    Self { 
      entries: RawVec::with_capacity(Self::capacity()) 
    }
  }

  crate fn block_end(&self) -> *mut T {
    let capacity = Self::capacity() as isize;
    unsafe { self.entries.ptr().offset(capacity) }
  }

  crate fn block_start(&self) -> *mut T {
    self.entries.ptr()
  }

  crate fn capacity() -> usize {
    1024
  }

  crate unsafe fn get(&self, index: usize) -> &T {
    &*self.slot(index)
  }

  crate unsafe fn get_mut(&mut self, index: usize) -> &mut T {
    &mut *self.slot_mut(index)
  }

  crate unsafe fn slot(&self, index: usize) -> *const T {
    self.entries.ptr().offset(index as isize)
  }

  crate unsafe fn slot_mut(&mut self, index: usize) -> *mut T {
    self.entries.ptr().offset(index as isize)
  }

  crate unsafe fn write(&mut self, index: usize, value: T) {
    ptr::write(self.slot_mut(index), value);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct BlockVec<T> {
  size: usize,
  blocks: Vec<Block<T>>,
}

impl<T> BlockVec<T> {
  pub fn new() -> Self {
    Self::default()
  }

  pub fn block_cap() -> usize {
    Block::<T>::capacity()
  }

  pub fn block_mut(&mut self, block_id: usize) -> &mut Block<T> {
    match block_id.cmp(&self.blocks.len()) {
      Ordering::Less => {
        &mut self.blocks[block_id]
      }
      Ordering::Equal => {
        self.blocks.push(Block::new());
        self.blocks.last_mut().unwrap()
      }
      Ordering::Greater => {
        panic!("block index out of bounds");
      }
    }
  }

  pub fn clear(&mut self) {
    let drop = |e: &mut T| unsafe { ptr::drop_in_place(e) };
    self.iter_mut().for_each(drop);
    self.blocks.clear();
  }

  pub fn get(&self, index: usize) -> Option<&T> {
    if index < self.size {
      unsafe { Some(self.get_unchecked(index)) }
    } else {
      None
    }
  }

  pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
    if index < self.size {
      unsafe { Some(self.get_unchecked_mut(index)) }
    } else {
      None
    }
  }

  pub unsafe fn get_unchecked(&self, index: usize) -> &T {
    let (block_id, slot_id) = Self::split_index(index);
    let block = self.blocks.get_unchecked(block_id);
    block.get(slot_id)
  }

  pub unsafe fn get_unchecked_mut(&mut self, index: usize) -> &mut T {
    let (block_id, slot_id) = Self::split_index(index);
    let block = self.blocks.get_unchecked_mut(block_id);
    block.get_mut(slot_id)
  }

  // pub fn get_mut_or<F>(&mut self, index: usize, ctor: F) -> &mut T where
  //   F: FnOnce() -> T
  // {
  //   if index > self.size { panic!("index out of bounds"); }
  //   if index == self.size { self.push(ctor()); }
  //   unsafe { self.get_unchecked_mut(index) }
  // }

  pub fn iter(&self) -> Iter<T> {
    Iter {
      index: 0,
      count: self.size,
      vec: self,
    }
  }

  pub fn iter_mut(&mut self) -> IterMut<T> {
    let non_empty = self.size > 0;
    match self.first_block() {
      Some(ref mut curr_block) if non_empty => unsafe {
        let elem_curr = curr_block.block_start();
        let curr_block_end = curr_block.block_end();
        let curr_block = *curr_block as *mut Block<T>;
        let elem_final_end = self.last_mut().unwrap() as *mut T;
        let elem_final_end = elem_final_end.offset(1);

        IterMut {
          elem_curr,
          elem_final_end,
          curr_block,
          curr_block_end,
          lifetime: PhantomData,
        }
      }

      _ => {
        IterMut {
          elem_curr: ptr::null_mut(),
          elem_final_end: ptr::null_mut(),
          curr_block: ptr::null_mut(),
          curr_block_end: ptr::null_mut(),
          lifetime: PhantomData,
        }
      }
    }
  }

  pub fn last(&self) -> Option<&T> {
    if self.size > 0 {
      unsafe { Some(self.get_unchecked(self.size - 1)) }
    } else {
      None
    }
  }

  pub fn last_mut(&mut self) -> Option<&mut T> {
    if self.size > 0 {
      unsafe { Some(self.get_unchecked_mut(self.size - 1)) }
    } else {
      None
    }
  }

  pub fn len(&self) -> usize {
    self.size
  }

  pub fn push(&mut self, value: T) {
    let (block_id, slot_id) = Self::split_index(self.size);
    let block = self.block_mut(block_id);
    unsafe { block.write(slot_id, value); }
    self.size += 1;
  }

  pub fn slot(&mut self, index: usize) -> Slot<T> {
    if index > self.size { panic!("index out of bounds"); }

    let (block_id, slot_id) = Self::split_index(self.size);
    let block = self.block_mut(block_id);

    unsafe {
      let slot = block.slot_mut(slot_id);
      if index == self.size { 
        self.size += 1;
        Slot::new_uninit(slot)
      } else {
        Slot::new(&mut *slot)
      }
    }
  }
}

impl<T> BlockVec<T> {
  fn first_block(&mut self) -> Option<&mut Block<T>> {
    self.blocks.first_mut()
  }

  fn split_index(index: usize) -> (usize, usize) {
    let block_id = index / Self::block_cap();
    let slot_id = index % Self::block_cap();
    (block_id, slot_id)
  }
}

impl<T> Default for BlockVec<T> {
  fn default() -> Self {
    Self { 
      size: 0,
      blocks: <_>::default() 
    }
  }
}

impl<T> Drop for BlockVec<T> {
  fn drop(&mut self) {
    self.clear();
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Iter<'a, T: 'a> {
  index: usize,
  count: usize,
  vec: &'a BlockVec<T>,
}

impl<'a, T> Iterator for Iter<'a, T> {
  type Item = &'a T;

  fn next(&mut self) -> Option<Self::Item> {
    if self.index < self.count {
      let result = unsafe { self.vec.get_unchecked(self.index) };
      self.index += 1;
      Some(result)
    } else {
      None
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct IterMut<'a, T: 'a> {
  elem_curr: *mut T,
  elem_final_end: *mut T,
  curr_block: *mut Block<T>,
  curr_block_end: *mut T,
  lifetime: PhantomData<&'a mut ()>,
}

impl<'a, T> Iterator for IterMut<'a, T> {
  type Item = &'a mut T;

  fn next(&mut self) -> Option<Self::Item> {
    unsafe {
      if self.elem_curr == self.elem_final_end { return None; }
      let result = Some(&mut *self.elem_curr);

      self.elem_curr = self.elem_curr.offset(1);
      let at_block_end = self.elem_curr == self.curr_block_end;
      let at_final_end = self.elem_curr == self.elem_final_end;
      if at_block_end && !at_final_end {
        self.curr_block = self.curr_block.offset(1);
        self.elem_curr = (*self.curr_block).block_start();
        self.curr_block_end = (*self.curr_block).block_end();
      }

      result
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
