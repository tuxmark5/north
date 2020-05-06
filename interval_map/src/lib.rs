#![feature(drain_filter)]
#![feature(nll)]
#![feature(step_trait)]

////////////////////////////////////////////////////////////////////////////////////////////////

use {
  itertools::Itertools,
  num_traits::Bounded,
  std::{
    fmt::{self, Debug},
    iter::{FromIterator, Step},
    mem,
    ops::{Bound::*, Range, RangeBounds, RangeFrom, RangeInclusive},
    slice,
    vec
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Debug)]
pub struct Entry<K, V> {
  pub key: K,
  pub value: Option<V>,
}

impl<K, V> Entry<K, V> {
  pub fn new(key: K, value: Option<V>) -> Self {
    Self { key, value }
  }

  pub fn map_key<F, R>(self, f: F) -> Entry<R, V> where
    K: Clone, F: FnOnce(K) -> R
  {
    Entry {
      key: f(self.key),
      value: self.value,
    }
  }

  pub fn map_value<F, R>(self, f: F) -> Entry<K, R> where
    K: Clone, F: FnOnce(V) -> R
  {
    Entry {
      key: self.key,
      value: self.value.map(f),
    }
  }

  pub fn map_value_ref<F, R>(&self, f: F) -> Entry<K, R> where
    K: Clone, F: FnOnce(&V) -> R
  {
    Entry {
      key: self.key.clone(),
      value: self.value.as_ref().map(f),
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone)]
pub struct IntervalMap<K, V> {
  entries: Vec<Entry<K, V>>,
}

impl<K, V> IntervalMap<K, V> where
  K: Ord, V: Clone
{
  pub fn new() -> Self {
    Self { entries: Vec::new() }
  }

  pub fn entries(&self) -> slice::Iter<Entry<K, V>> {
    self.entries.iter()
  }

  pub fn extend<T>(&mut self, other: T) where
    T: IntoIterator<Item=Entry<K, V>>
  {
    let entries = mem::replace(&mut self.entries, Vec::new());
    let result = entries.into_iter().merge_by(other, |a, b| a.key < b.key);
    self.entries.extend(result);
  }

  pub fn get(&self, key: &K) -> Option<&V> {
    let idx = self.entries.binary_search_by(|e| e.key.cmp(key));
    let idx = match idx {
      Ok(idx) => idx,
      Err(idx) if idx >= 1 => idx - 1,
      Err(_) => return None,
    };

    self.entries.get(idx)
      .and_then(|e| e.value.as_ref())
  }

  pub fn insert<R>(&mut self, key: R, value: Option<V>) where
    K: Clone + Step, R: RangeBounds<K>
  {
    let (start_k, start_v) = match key.start_bound() {
      Excluded(_) => panic!("unsupported start bound"),
      Included(start_k) => (start_k, value),
      Unbounded => panic!("unsupported start bound"),
    };

    let end_pair = match key.end_bound() {
      Excluded(end_k) => {
        let value = self.get(end_k).cloned();
        Some((end_k.clone(), value))
      },
      Included(end_k) => end_k.add_usize(1).map(|end_k| {
        let value = self.get(&end_k).cloned();
        (end_k, value)
      }),
      Unbounded => None
    };

    for _ in self.entries.drain_filter(|e| key.contains(&e.key)) { }

    self.insert_one(start_k.clone(), start_v);

    if let Some((end_k, end_v)) = end_pair {
      self.insert_one(end_k.clone(), end_v);
    }
  }

  pub fn insert_from(&mut self, key: RangeFrom<K>, value: Option<V>) {
    let _ = self.entries.drain_filter(|e| key.contains(&e.key)).count();

    self.insert_one(key.start, value);
  }

  pub fn insert_range(&mut self, key: Range<K>, value: Option<V>) {
    let end_value = self.get(&key.end).cloned();

    let _ = self.entries.drain_filter(|e| key.contains(&e.key)).count();

    self.insert_one(key.start, value);
    self.insert_one(key.end, end_value);
  }

  pub fn insert_one(&mut self, key: K, value: Option<V>) {
    let idx = self.entries.binary_search_by(|e| e.key.cmp(&key));
    match idx {
      Ok(idx) => { self.entries[idx].value = value },
      Err(idx) => { self.entries.insert(idx, Entry::new(key, value)) },
    }
  }

  pub fn iter(&self) -> Iter<K, V> {
    let inner = self.entries.iter();
    Iter { inner, prev: None }
  }

  pub fn iter_raw(&self) -> slice::Iter<Entry<K, V>> {
    self.entries.iter()
  }

  pub fn into_entries(self) -> vec::IntoIter<Entry<K, V>> {
    self.entries.into_iter()
  }

  pub fn len(&self) -> usize {
    self.entries.len()
  }

  pub fn merge<T, F>(&mut self, other: T, merge_val: F) where
    T: IntoIterator<Item=Entry<K, V>>, 
    F: Fn(&mut V, &V)
  {
    // TODO: add fastpath for when left or right is empty
    // TODO: convert this into iterator?

    let entries = mem::replace(&mut self.entries, Vec::new())
      .into_iter()
      .merge_join_by(other, |a, b| a.key.cmp(&b.key));

    let mut value_a = None;
    let mut value_b = None;

    for entry in entries {
      use itertools::EitherOrBoth::*;

      let key = match entry {
        Left(a) => { 
          value_a = a.value; 
          a.key
        },
        Right(b) => { 
          value_b = b.value; 
          b.key
        },
        Both(a, b) => {
          value_a = a.value;
          value_b = b.value;
          a.key
        },
      };

      let value = match (&value_a, &value_b) {
        (Some(a), Some(b)) => {
          let mut a = a.clone();
          merge_val(&mut a, b); 
          Some(a)
        },
        (Some(a), None) => {
          Some(a.clone())
        },
        (None, Some(b)) => {
          Some(b.clone())
        },
        (None, None) => {
          None
        },
      };

      let entry = Entry::new(key, value);
      self.entries.push(entry);
    }
  }
}

impl<K, V> IntervalMap<K, V> where
  K: Bounded + Ord, V: Clone
{
  /*pub fn new_point(key: K, V: value, ) -> Self {
    Self { entries: Vec::new() }
  }*/

  pub fn add_negative(&mut self, value: V) {
    if !self.has_first() {
      let entry = Entry::new(K::min_value(), Some(value.clone()));
      self.entries.insert(0, entry);
    }

    for entry in &mut self.entries {
      if entry.value.is_none() {
        entry.value = Some(value.clone());
      }
    }
  }

  pub fn has_first(&self) -> bool {
    match self.entries.first() {
      Some(first) => first.key == K::min_value(),
      None => false,
    }
  }

  pub fn invert(&mut self, value: V) {
    if !self.has_first() {
      let entry = Entry::new(K::min_value(), None);
      self.entries.insert(0, entry);
    }

    for entry in &mut self.entries {
      if entry.value.is_none() {
        entry.value = Some(value.clone());
      } else {
        entry.value = None;
      }
    }
  }
}

impl<K, V> IntervalMap<K, V> where
  K: Ord, V: Clone + Eq
{
  pub fn dedup(&mut self) {
    self.entries.dedup_by(|a, b| a.value == b.value);
  }
}

impl<K, V> AsRef<[Entry<K, V>]> for IntervalMap<K, V> {
  fn as_ref(&self) -> &[Entry<K, V>] {
    self.entries.as_ref()
  }
}

impl<K, V> Debug for IntervalMap<K, V> where
  K: Debug, V: Debug
{
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let mut debug_map = f.debug_map();
    let mut prev: Option<&Entry<K, V>> = None;

    for entry in &self.entries {
      if let Some(prev) = prev.take() {
        let key = Range { start: &prev.key, end: &entry.key };
        let value = prev.value.as_ref().unwrap();
        debug_map.entry(&key, value);
      }

      if entry.value.is_some() {
        prev = Some(entry);
      }
    }

    if let Some(prev) = prev.take() {
      let key = RangeFrom { start: &prev.key };
      let value = prev.value.as_ref().unwrap();
      debug_map.entry(&key, value);
    }

    debug_map.finish()
  }
}

impl<K, V> Default for IntervalMap<K, V> where
  K: Ord, V: Clone
{
  fn default() -> Self {
    Self::new()
  }
}

impl<K, V> FromIterator<Entry<K, V>> for IntervalMap<K, V> where
  K: Ord, V: Clone
{
  fn from_iter<T>(iter: T) -> Self where
    T: IntoIterator<Item=Entry<K, V>>
  {
    Self {
      entries: Vec::from_iter(iter)
    }
  }
}

impl<'a, K, V> IntoIterator for &'a IntervalMap<K, V> where
  K: 'a + Bounded + Clone + Ord + Step, V: 'a + Clone
{
  type Item = (RangeInclusive<K>, &'a V);
  type IntoIter = Iter<'a, K, V>;

  fn into_iter(self) -> Self::IntoIter {
    (*self).iter()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Iter<'a, K, V> where
  K: 'a, V: 'a
{
  inner: slice::Iter<'a, Entry<K, V>>,
  prev: Option<&'a Entry<K, V>>,
}

impl<'a, K, V> Iterator for Iter<'a, K, V> where
  K: 'a + Bounded + Clone + Step, V: 'a
{
  type Item = (RangeInclusive<K>, &'a V);

  fn next(&mut self) -> Option<Self::Item> {
    loop {
      match self.inner.next() {
        Some(entry) => {
          let next_prev = entry.value.as_ref().and(Some(entry));
          if let Some(prev) = self.prev.take() {
            let key = prev.key.clone()..=entry.key.sub_one();
            let value = prev.value.as_ref().unwrap();
            self.prev = next_prev;
            return Some((key, value));
          } else {
            self.prev = next_prev;
          }
        }

        None => {
          return self.prev.take().and_then(|prev| {
            let key = prev.key.clone()..=K::max_value();
            let value = prev.value.as_ref().unwrap();
            Some((key, value))
          });
        }
      }
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[test]
fn test_one() {
  let mut map = IntervalMap::<i32, i32>::new();
  map.insert(10..30, Some(40));
  map.insert(15..25, Some(50));
  map.add_negative(123);
  println!("{:?}", &map);
}

////////////////////////////////////////////////////////////////////////////////////////////////
