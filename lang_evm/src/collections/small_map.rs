use {
  crate::{
    alloc::{ReverseAlloc},
    collections::{Bucket, LinkedList, Slot},
    util::ManualInit,
  },
  std::{
    alloc::{Global},
    marker::{PhantomData},
    mem::{ManuallyDrop, uninitialized},
    ptr::{self, write},
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub union SmallMap<K, V, A = Global> where
  K: Eq, A: Default + ReverseAlloc
{
  size: u32,
  heap: ManuallyDrop<InnerHeap<K, V, A>>,
  inline: ManuallyDrop<InnerInline<K, V, A>>,
}

impl<K, V, A> SmallMap<K, V, A> where
  K: Eq, A: Default + ReverseAlloc
{
  pub fn new() -> Self {
    Self::default()
  }

  pub fn clear(&mut self) {
    match self.inner_mut() {
      Inner::Inline(inner) => inner.clear(),
      Inner::Heap(inner) => inner.clear(),
    }
  }

  pub fn entry(&mut self, key: K) -> Entry<K, V, A> {
    let self_ = self as *mut Self;

    match self.inner_mut() {
      Inner::Inline(inner) => match inner.bucket_mut(&key) {
        Some(bucket) => Entry::OccupiedInline(bucket),
        None => Entry::VacantInline(unsafe { &mut *self_ }, key),
      }

      Inner::Heap(inner) => match inner.bucket_mut(&key) {
        Some(bucket) => Entry::OccupiedHeap(bucket),
        None => Entry::VacantHeap(unsafe { &mut *self_ }, key),
      }
    }
  }

  pub fn inner_mut(&mut self) -> Inner<K, V, A> {
    unsafe {
      if self.size <= 2 {
        Inner::Inline(&mut self.inline)
      } else {
        Inner::Heap(&mut self.heap)
      }
    }
  }

  pub unsafe fn insert_heap(&mut self, key: K, value: V) {
    self.size += 1;
    self.heap.insert(Bucket { key, value });
  }

  pub fn insert_fold<F, R>(&mut self, key: K, value: V, fold: F) -> Option<R> where
    F: FnOnce(&mut V, V) -> R
  {
    match self.inner_mut() {
      Inner::Inline(inner) => match inner.bucket_mut(&key) {
        Some(bucket) => Some(fold(&mut bucket.value, value)),
        None => unsafe { self.insert_inline(key, value); None },
      }

      Inner::Heap(inner) => match inner.bucket_mut(&key) {
        Some(bucket) => Some(fold(&mut bucket.value, value)),
        None => unsafe { self.insert_heap(key, value); None },
      }
    }
  }

  pub unsafe fn insert_inline(&mut self, key: K, value: V) {
    if self.size >= 2 {
      self.promote_to_heap();
      self.insert_heap(key, value);
    } else {
      let bucket = Bucket { key, value };
      let index = self.size as usize;
      let slot = &mut self.inline.entries[index];
      slot.init(bucket);
      self.size += 1;
    }
  }

  pub fn len(&self) -> usize {
    unsafe { self.size as usize }
  }

  pub unsafe fn promote_to_heap(&mut self) {
    let mut list = LinkedList::default();

    for bucket in self.inline.entries.iter_mut() {
      let bucket = bucket.take();
      list.push_back(bucket);
    }

    write(&mut self.heap.entries, list);
  }

  pub fn slot(&mut self, key: K) -> Slot<V> {
    let self_ = self as *mut Self;

    match self.inner_mut() {
      Inner::Inline(inner) => match inner.bucket_mut(&key) {
        Some(bucket) => Slot::new(&mut bucket.value),
        None => unsafe { (*self_).slot_inline(key) },
      }

      Inner::Heap(inner) => match inner.bucket_mut(&key) {
        Some(bucket) => Slot::new(&mut bucket.value),
        None => unsafe { (*self_).slot_heap(key) },
      }
    }
  }

  pub unsafe fn slot_heap(&mut self, key: K) -> Slot<V> {
    self.size += 1;
    let bucket = Bucket { key, value: uninitialized() };
    let bucket = self.heap.insert(bucket);
    Slot::new_uninit(&mut bucket.value)
  }

  pub unsafe fn slot_inline(&mut self, key: K) -> Slot<V> {
    if self.size >= 2 {
      self.promote_to_heap();
      return self.slot_heap(key);
    } 
    
    let index = self.size as usize;
    self.size += 1;

    let entry = &mut self.inline.entries[index];
    let bucket = Bucket { key, value: uninitialized() };
    let bucket = entry.init(bucket);
    Slot::new_uninit(&mut bucket.value)
  }
}

impl<K, V, A> Default for SmallMap<K, V, A> where
  K: Eq, A: Default + ReverseAlloc
{
  fn default() -> Self {
    Self { size: 0 }
  }
}

impl<K, V, A> Drop for SmallMap<K, V, A> where
  K: Eq, A: Default + ReverseAlloc
{
  fn drop(&mut self) {
    self.clear();
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub enum Entry<'a, K, V, A> where
  K: 'a + Eq, V: 'a, A: 'a + Default + ReverseAlloc
{
  OccupiedHeap(&'a mut Bucket<K, V>),
  OccupiedInline(&'a mut Bucket<K, V>),
  VacantHeap(&'a mut SmallMap<K, V, A>, K),
  VacantInline(&'a mut SmallMap<K, V, A>, K),
}

impl<'a, K, V, A> Entry<'a, K, V, A> where
  K: 'a + Eq, V: 'a, A: 'a + Default + ReverseAlloc
{
  pub fn is_vacant(&self) -> bool {
    match self {
      Entry::OccupiedHeap(_) => false,
      Entry::OccupiedInline(_) => false,
      Entry::VacantHeap(_, _) => true,
      Entry::VacantInline(_, _) => true
    }
  }

  pub fn or_insert_with<F>(self, ctor: F) -> &'a mut V where
    F: FnOnce() -> V
  {
    match self {
      Entry::OccupiedHeap(bucket) => &mut bucket.value,
      Entry::OccupiedInline(bucket) => &mut bucket.value,
      Entry::VacantHeap(map, key) => unsafe { 
        map.size += 1;
        let bucket = Bucket { key, value: ctor() };
        &mut map.heap.insert(bucket).value
      }
      Entry::VacantInline(map, key) => unsafe {
        let bucket = Bucket { key, value: ctor() };
        map.size += 1;

        if map.size >= 3 {
          map.promote_to_heap();
          &mut map.heap.insert(bucket).value
        } else {
          let index = (map.size - 1) as usize;
          let slot = &mut map.inline.entries[index];
          slot.init(bucket);
          &mut slot.get_mut().value
        }
      }
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub enum Inner<'a, K, V, A> where
  K: 'a, V: 'a, A: 'a + ReverseAlloc
{
  Heap(&'a mut InnerHeap<K, V, A>),
  Inline(&'a mut InnerInline<K, V, A>),
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[repr(C)]
pub struct InnerHeap<K, V, A> where
  A: ReverseAlloc
{
  size: u32,
  entries: LinkedList<Bucket<K, V>, A>,
}

impl<K, V, A> InnerHeap<K, V, A> where
  K: Eq, A: ReverseAlloc
{
  pub fn bucket_mut(&mut self, key: &K) -> Option<&mut Bucket<K, V>> {
    self.entries.iter_mut().find(|b| b.key.eq(&key)) 
  }

  pub fn clear(&mut self) {
    self.entries.clear();
    self.size = 0;
  }

  pub fn insert(&mut self, bucket: Bucket<K, V>) -> &mut Bucket<K, V> {
    self.entries.push_back(bucket);
    self.entries.back_mut().unwrap()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[repr(C)]
pub struct InnerInline<K, V, A> {
  size: u32,
  entries: [ManualInit<Bucket<K, V>>; 2],
  alloc_type: PhantomData<A>,
}

impl<K, V, A> InnerInline<K, V, A> where
  K: Eq, A: ReverseAlloc
{
  pub fn bucket_mut(&mut self, key: &K) -> Option<&mut Bucket<K, V>> {
    let buckets = self.buckets_mut();
    buckets.iter_mut().find(|b| b.key.eq(&key)) 
  }

  pub fn buckets_mut(&mut self) -> &mut [Bucket<K, V>] {
    unsafe {
      let size = self.size as usize;
      let slice = &mut self.entries[..size];
      let slice = slice as *mut _ as *mut [Bucket<K, V>];
      &mut *slice
    }
  }

  pub fn clear(&mut self) {
    let drop = |e: &mut Bucket<K, V>| unsafe { ptr::drop_in_place(e) };
    self.buckets_mut().iter_mut().for_each(drop);
    self.size = 0;
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
