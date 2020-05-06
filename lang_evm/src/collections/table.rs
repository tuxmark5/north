use {
  std::{
    marker::PhantomData,
    mem::replace,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Table<I, V> {
  entries: Vec<Option<V>>,
  index_type: PhantomData<I>,
}

impl<I, V> Table<I, V> where
  I: Clone + From<usize> + Into<usize>
{
  pub fn new() -> Self {
    Self::default()
  }

  pub fn with_capacity(capacity: usize) -> Self {
    Self {
      entries: Vec::with_capacity(capacity),
      index_type: PhantomData,
    }
  }

  pub fn clear(&mut self) {
    self.entries.clear();
  }

  pub fn entry(&mut self, idx: I) -> Entry<I, V> {
    let index = idx.clone().into();
    let self_ = self as *mut Self;

    match self.entries.get_mut(index) {
      Some(Some(value)) => Entry::Occupied(OccupiedEntry { 
        value 
      }),
      _ => Entry::Vacant(VacantEntry { 
        table: unsafe { &mut *self_ }, index: idx 
      })
    }
  }

  pub fn get(&self, idx: I) -> Option<&V> {
    let idx: usize = idx.into();
    match self.entries.get(idx) {
      Some(Some(value)) => Some(value),
      _ => None,
    }
  }

  pub fn get_mut(&mut self, idx: I) -> Option<&mut V> {
    let idx: usize = idx.into();
    match self.entries.get_mut(idx) {
      Some(Some(value)) => Some(value),
      _ => None,
    }
  }

  pub fn insert(&mut self, idx: I, value: V) -> Option<V> {
    let slot = self.slot_mut(idx);
    replace(slot, Some(value))
  }

  pub fn insert_get(&mut self, idx: I, value: V) -> &mut V {
    let slot = self.slot_mut(idx);
    *slot = Some(value);
    slot.as_mut().unwrap()
  }

  pub fn len(&self) -> usize {
    self.entries.len()
  }

  pub fn remove(&mut self, idx: I) {
    let idx: usize = idx.into();
    if let Some(slot) = self.entries.get_mut(idx) {
      *slot = None;
    }
  }

  pub fn slot_mut(&mut self, idx: I) -> &mut Option<V> {
    let idx: usize = idx.into();
    let new_len = idx + 1;

    if new_len > self.entries.len() {
      self.entries.resize_with(new_len, || None);
    }

    &mut self.entries[idx]
  }
}

impl<I, V> Default for Table<I, V> where
  I: From<usize> + Into<usize>
{
  fn default() -> Self {
    Self {
      entries: Vec::default(),
      index_type: PhantomData,
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub enum Entry<'a, I: 'a, V: 'a> {
  Occupied(OccupiedEntry<'a, V>),
  Vacant(VacantEntry<'a, I, V>),
}

impl<'a, I, V> Entry<'a, I, V> where
  I: Clone + From<usize> + Into<usize>
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

pub struct OccupiedEntry<'a, V: 'a> {
  value: &'a mut V,
}

impl<'a, V> OccupiedEntry<'a, V> {
  pub fn get_mut(self) -> &'a mut V {
    self.value
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct VacantEntry<'a, I: 'a, V: 'a> {
  table: &'a mut Table<I, V>,
  index: I,
}

impl<'a, I, V> VacantEntry<'a, I, V> where
  I: Clone + From<usize> + Into<usize>
{
  pub fn insert(self, value: V) -> &'a mut V {
    let slot = self.table.slot_mut(self.index);
    *slot = Some(value);
    slot.as_mut().unwrap()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
