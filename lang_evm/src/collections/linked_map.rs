use {
  crate::{
    alloc::{ReverseAlloc},
    collections::{Bucket, LinkedList},
  },
  std::{
    alloc::{Global},
  }
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct LinkedMap<K, V, A: ReverseAlloc = Global> {
  entries: LinkedList<Bucket<K, V>, A>
}

impl<K, V> LinkedMap<K, V, Global> {
  pub fn new() -> Self {
    Self::default()
  }
}

impl<K, V, A> LinkedMap<K, V, A> where
  K: Eq, A: ReverseAlloc
{
  pub fn bucket_mut(&mut self, key: &K) -> Option<&mut Bucket<K, V>> {
    self.entries.iter_mut()
      .find(|b| b.key.eq(key))
  }

  pub fn entry(&mut self, key: K) -> Entry<K, V, A> {
    let self_ = self as *mut Self;
    match self.bucket_mut(&key) {
      Some(bucket) => Entry::Occupied(bucket),
      None => Entry::Vacant(unsafe { &mut *self_ }, key),
    }
  }

  pub fn get(&self, key: &K) -> Option<&V> {
    self.entries.iter()
      .find(|b| b.key.eq(key))
      .map(|b| &b.value)
  }

  pub fn get_mut(&mut self, key: &K) -> Option<&mut V> {
    self.entries.iter_mut()
      .find(|b| b.key.eq(key))
      .map(|b| &mut b.value)
  }

  pub fn insert_new(&mut self, key: K, value: V) -> &mut V {
    let bucket = Bucket { key, value };
    self.entries.push_back(bucket);
    let bucket = self.entries.back_mut().unwrap();
    &mut bucket.value
  }

  pub fn len(&self) -> usize {
    self.entries.len()
  }
}

impl<K, V, A> Default for LinkedMap<K, V, A> where
  A: Default + ReverseAlloc
{
  fn default() -> Self {
    Self { entries: <_>::default() }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub enum Entry<'a, K, V, A> where
  K: 'a, V: 'a, A: 'a + ReverseAlloc
{
  Occupied(&'a mut Bucket<K, V>),
  Vacant(&'a mut LinkedMap<K, V, A>, K),
}

impl<'a, K, V, A> Entry<'a, K, V, A> where
  K: Eq, A: ReverseAlloc
{
  pub fn is_vacant(&self) -> bool {
    match self {
      Entry::Occupied(_) => false,
      Entry::Vacant(_, _) => true,
    }
  }

  pub fn or_insert_with<F>(self, ctor: F) -> &'a mut V where
    F: FnOnce() -> V
  {
    match self {
      Entry::Occupied(bucket) => &mut bucket.value,
      Entry::Vacant(map, key) => map.insert_new(key, ctor()),
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
