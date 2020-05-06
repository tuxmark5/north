use {
  std::{
    cmp::{Ordering, PartialEq},
    marker::PhantomData,
  }
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct LinearMap<K, V> {
  crate keys: Vec<K>,
  crate values: Vec<V>,
}

impl<K, V> LinearMap<K, V> where
  K: Ord
{
  pub fn new() -> Self {
    Self::default()
  }

  pub fn entry(&mut self, key: K) -> Entry<K, V> {
    match self.find_insert_index(&key) {
      Ok(index) => Entry::Occupied(OccupiedEntry {
        key_type: PhantomData,
        value: unsafe { self.values.get_unchecked_mut(index) },
      }),
      Err(index) => Entry::Vacant(VacantEntry {
        parent: self, index, key
      }),
    }
  }

  pub fn find_index(&self, key: &K) -> Option<usize> {
    for i in 0..self.keys.len() {
      if self.keys[i].eq(key) { return Some(i); }
    }
    None
  }

  pub fn find_insert_index(&self, key: &K) -> Result<usize, usize> {
    let len = self.keys.len();

    for i in 0..len {
      match self.keys[i].cmp(key) { 
        Ordering::Less => {},
        Ordering::Equal => return Ok(i),
        Ordering::Greater => return Err(i),
      }
    }

    Err(len)
  }

  pub fn get(&self, key: &K) -> Option<&V> {
    self.find_index(key).map(|i| unsafe {
      self.values.get_unchecked(i)
    })
  }

  pub fn get_mut(&mut self, key: &K) -> Option<&mut V> {
    self.find_index(key).map(move |i| unsafe {
      self.values.get_unchecked_mut(i)
    })
  }

  pub fn get_mut_or<F>(&mut self, key: K, ctor: F) -> &mut V where
    F: FnOnce() -> V
  {
    match self.find_insert_index(&key) {
      Ok(index) => &mut self.values[index],
      Err(index) => self.insert_unchecked(index, key, ctor()),
    }
  }

  fn insert_unchecked(&mut self, index: usize, key: K, value: V) -> &mut V {
    self.keys.insert(index, key);
    self.values.insert(index, value);
    unsafe {
      self.values.get_unchecked_mut(index)
    }
  }

  pub fn len(&self) -> usize {
    self.values.len()
  }
}

impl<K, V> Default for LinearMap<K, V> where
  K: PartialEq
{
  fn default() -> Self {
    Self {
      keys: <_>::default(),
      values: <_>::default(),
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub enum Entry<'a, K: 'a, V: 'a> {
  Occupied(OccupiedEntry<'a, K, V>),
  Vacant(VacantEntry<'a, K, V>),
}

impl<'a, K, V> Entry<'a, K, V> where
  K: Ord
{
  pub fn or_insert_with<F>(self, ctor: F) -> &'a mut V where
    F: FnOnce() -> V
  {
    match self {
      Entry::Occupied(occupied) => occupied.value,
      Entry::Vacant(vacant) => vacant.insert(ctor()),
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct OccupiedEntry<'a, K: 'a, V: 'a> {
  key_type: PhantomData<K>,
  value: &'a mut V,
}

impl<'a, K, V> OccupiedEntry<'a, K, V> {
  pub fn into_mut(self) -> &'a mut V {
    self.value
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct VacantEntry<'a, K: 'a, V: 'a> {
  parent: &'a mut LinearMap<K, V>,
  index: usize,
  key: K,
}

impl<'a, K, V> VacantEntry<'a, K, V> where
  K: Ord
{
  pub fn insert(self, value: V) -> &'a mut V {
    self.parent.insert_unchecked(self.index, self.key, value)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
