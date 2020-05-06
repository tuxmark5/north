use {
  crate::{
    collections::{
      BlockVec, LinearMap, Slot, Table,
      block_vec,
    },
  },
  std::{
    fmt::{self, Debug},
    marker::PhantomData,
    mem::size_of,
    //simd::{u64x4},
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

macro_rules! make_array {
  ($n:expr, $constructor:expr) => { 
    unsafe {
      let mut items: [_; $n] = std::mem::uninitialized();
      for place in items.iter_mut() {
        std::ptr::write(place, $constructor());
      }
      items
    }
  }
}

use self::{
  array_level::ArrayLevel12,
  bottom_level::BottomLevel,
  linear_level::LinearLevel,
  table_level::TableLevel,
};

// pub type PointerTrie<K, V> = FixedTrie<
//   K, V, LinearLevel<ArrayLevel12<ArrayLevel12<BottomLevel<u32>>>>
// >;

pub type PointerTrie<K, V> = FixedTrie<
  K, V, TableLevel<BottomLevel<u32>>
>;

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct FixedTrie<K, V, L> {
  key_type: PhantomData<K>,
  values: BlockVec<V>,
  top_level: L,
}

impl<K, V, L> FixedTrie<K, V, L> where
  K: TrieKey, L: Default + TrieLevel<K::Repr, Value=u32>,
{
  pub fn new() -> Self {
    Self::default()
  }

  pub fn bind_key(&mut self, key: K, index: usize) {
    let key_repr = key.into_repr();
    let index = index as u32;
    self.top_level.get_mut_or(key_repr, || index);
  }

  pub fn clear(&mut self) {
    self.top_level.clear();
    self.values.clear();
  }

  pub fn dump_info(&self) { 
    println!("FIXED_TRIE:");
    self.top_level.dump_info();
  }

  pub fn get(&self, key: K) -> Option<&V> {
    let key_repr = key.into_repr();
    self.top_level.get(key_repr).map(|index| unsafe {
      self.values.get_unchecked(*index as usize)
    })
  }

  // get_by_index
  pub fn get_index(&self, index: usize) -> Option<&V> {
    self.values.get(index)
  }

  pub fn get_index_mut(&mut self, index: usize) -> Option<&mut V> {
    self.values.get_mut(index)
  }

  pub fn get_mut(&mut self, key: K) -> Option<&mut V> {
    let key_repr = key.into_repr();
    let values = &mut self.values;
    self.top_level.get(key_repr).and_then(move |index| {
      values.get_mut(*index as usize)
    })
  }

  pub fn index(&self, key: K) -> Option<usize> {
    let key_repr = key.into_repr();
    let result = self.top_level.get(key_repr);
    result.map(|index| *index as usize)
  }

  pub fn len(&self) -> usize {
    self.values.len()
  }

  pub fn slot(&mut self, key: K) -> (usize, Slot<V>) {
    let key_repr = key.into_repr();
    let new_index = self.values.len() as u32;
    let index = self.top_level.get_mut_or(key_repr, || new_index);
    let index = *index as usize;
    (index, self.values.slot(index))
  }

  pub fn unbind_key(&mut self, key: K) {
    let key_repr = key.into_repr();
    self.top_level.remove(key_repr);
  }
}

impl<K, V, L> Debug for FixedTrie<K, V, L> where
  K: TrieKey, L: Default + TrieLevel<K::Repr, Value=u32>,
{
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "FixedTrie")
  }
}

impl<K, V, L> Default for FixedTrie<K, V, L> where
  K: TrieKey, L: Default + TrieLevel<K::Repr, Value=u32>,
{
  fn default() -> Self {
    Self {
      key_type: PhantomData,
      values: <_>::default(),
      top_level: <_>::default(),
    }
  }
}

impl<'a, K, V, L> IntoIterator for &'a FixedTrie<K, V, L> {
  type IntoIter = block_vec::Iter<'a, V>;
  type Item = &'a V;

  fn into_iter(self) -> Self::IntoIter {
    self.values.iter()
  }
}

impl<'a, K, V, L> IntoIterator for &'a mut FixedTrie<K, V, L> {
  type IntoIter = block_vec::IterMut<'a, V>;
  type Item = &'a mut V;

  fn into_iter(self) -> Self::IntoIter {
    self.values.iter_mut()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait TrieKey {
  type Repr: Copy;
  fn into_repr(self) -> Self::Repr;
}

impl TrieKey for usize {
  type Repr = usize;

  fn into_repr(self) -> usize {
    self
  }
}

impl<T> TrieKey for *const T {
  type Repr = usize;

  fn into_repr(self) -> usize {
    (self as usize)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait TrieKeyRepr {
  fn take_bits(self, count: u32) -> (usize, Self);
}

impl TrieKeyRepr for u32 {
  fn take_bits(self, count: u32) -> (usize, Self) {
    let head_len = (size_of::<u32>() * 8) as u32 - count;
    let (head, _) = self.overflowing_shr(head_len);
    let (tail, _) = self.overflowing_shl(count);
    (head as usize, tail)
  }
}

impl TrieKeyRepr for usize {
  fn take_bits(self, count: u32) -> (usize, Self) {
    let head_len = (size_of::<usize>() * 8) as u32 - count;
    let (head, _) = self.overflowing_shr(head_len);
    let (tail, _) = self.overflowing_shl(count);
    (head, tail)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait TrieLevel<K> {
  type Value;

  fn clear(&mut self) { }
  fn dump_info(&self) { }
  fn get(&self, key: K) -> Option<&Self::Value>;
  fn get_mut(&mut self, key: K) -> Option<&mut Self::Value>;
  fn get_mut_or<F>(&mut self, key: K, ctor: F) -> &mut Self::Value where
    F: Fn() -> Self::Value;
  fn make<F>(ctor: &F) -> Self where
    F: Fn() -> Self::Value;
  fn remove(&mut self, _key: K) { }
  fn slot(&mut self, key: K) -> Slot<Self::Value>;
}

////////////////////////////////////////////////////////////////////////////////////////////////

mod array_level {
  use super::*;

  pub struct ArrayLevel12<L> {
    entries: [Option<Box<L>>; 4096]
  }

  impl<K, L> TrieLevel<K> for ArrayLevel12<L> where
    K: TrieKeyRepr, L: TrieLevel<K>
  {
    type Value = L::Value;

    fn get(&self, key: K) -> Option<&Self::Value> {
      let (index, rest) = key.take_bits(12);
      let inner = self.entries[index].as_ref()?;
      inner.get(rest)
    }

    fn get_mut(&mut self, key: K) -> Option<&mut Self::Value> {
      let (index, rest) = key.take_bits(12);
      let inner = self.entries[index].as_mut()?;
      inner.get_mut(rest)
    }

    fn get_mut_or<F>(&mut self, key: K, ctor: F) -> &mut Self::Value where
      F: Fn() -> Self::Value
    {
      let (index, rest) = key.take_bits(12);
      let slot = &mut self.entries[index];
      if slot.is_none() { *slot = Some(box L::make(&ctor)); }
      let inner = &mut **slot.as_mut().unwrap();
      inner.get_mut_or(rest, ctor)
    }

    fn make<F>(_ctor: &F) -> Self where
      F: Fn() -> Self::Value
    {
      Self::default()
    }

    fn slot(&mut self, key: K) -> Slot<Self::Value> {
      /*let (index, rest) = key.take_bits(12);
      let slot = &mut self.entries[index];
      if slot.is_none() { *slot = Some(box L::make(&ctor)); }
      let inner = &mut **slot.as_mut().unwrap();
      inner.get_mut_or(rest, ctor)*/
      unimplemented!()
    }
  }

  impl<L> Default for ArrayLevel12<L> {
    fn default() -> Self {
      Self { entries: make_array!(4096, || None) }
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

mod bit_level {
  use super::*;

  pub struct BitLevel8<L> {
    bitmap: [u64; 4],
    entries: Vec<L>,
  }

  impl<L> BitLevel8<L> {
    fn find_index(&self, num: usize) -> Result<usize, usize> {
      Ok(0)
    }
  }

  impl<K, L> TrieLevel<K> for BitLevel8<L> where
    K: TrieKeyRepr, L: TrieLevel<K>
  {
    type Value = L::Value;

    fn get(&self, key: K) -> Option<&Self::Value> {
      let (index, rest) = key.take_bits(8);
      match self.find_index(index) {
        Ok(index) => self.entries[index].get(rest),
        Err(_) => None,
      }
    }

    fn get_mut(&mut self, key: K) -> Option<&mut Self::Value> {
      let (index, rest) = key.take_bits(8);
      match self.find_index(index) {
        Ok(index) => self.entries[index].get_mut(rest),
        Err(_) => None,
      }
    }

    fn get_mut_or<F>(&mut self, key: K, ctor: F) -> &mut Self::Value where
      F: Fn() -> Self::Value
    {
      let (index, rest) = key.take_bits(8);
      let index = match self.find_index(index) { 
        Ok(index) => index,
        Err(index) => {
          let level = L::make(&ctor);
          self.entries.insert(index, level);
          index
        },
      };

      let inner = &mut self.entries[index];
      inner.get_mut_or(rest, ctor)
    }

    fn make<F>(_ctor: &F) -> Self where
      F: Fn() -> Self::Value
    {
      Self { 
        bitmap: <_>::default(),
        entries: <_>::default(),
      }
    }

    fn slot(&mut self, key: K) -> Slot<Self::Value> {
      unimplemented!()
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub mod bottom_level {
  use super::*;

  pub struct BottomLevel<V> {
    value: V
  }

  impl<K, V> TrieLevel<K> for BottomLevel<V> {
    type Value = V;

    fn get(&self, _key: K) -> Option<&Self::Value> {
      Some(&self.value)
    }

    fn get_mut(&mut self, _key: K) -> Option<&mut Self::Value> {
      Some(&mut self.value)
    }

    #[inline]
    fn get_mut_or<F>(&mut self, _key: K, _ctor: F) -> &mut Self::Value where
      F: FnOnce() -> Self::Value
    {
      &mut self.value
    }

    fn make<F>(ctor: &F) -> Self where
      F: Fn() -> Self::Value
    {
      Self { value: (*ctor)() }
    }

    fn slot(&mut self, key: K) -> Slot<Self::Value> {
      unimplemented!()
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub mod linear_level {
  use super::*;

  const MASK_SIZE: u32 = 40;

  pub struct LinearLevel<L> {
    pairs: LinearMap<usize, Box<L>>
  }

  impl<K, L> TrieLevel<K> for LinearLevel<L> where
    K: TrieKeyRepr, L: Default + TrieLevel<K>
  {
    type Value = L::Value;

    fn dump_info(&self) { 
      println!("LINEAR_LEVEL:");
      println!("  len: {}", self.pairs.len());
    }

    fn get(&self, key: K) -> Option<&Self::Value> {
      let (index, rest) = key.take_bits(MASK_SIZE);
      self.pairs.get(&index).and_then(|inner| {
        inner.get(rest)
      })
    }

    fn get_mut(&mut self, key: K) -> Option<&mut Self::Value> {
      let (index, rest) = key.take_bits(MASK_SIZE);
      self.pairs.get_mut(&index).and_then(|inner| {
        inner.get_mut(rest)
      })
    }

    fn get_mut_or<F>(&mut self, key: K, ctor: F) -> &mut Self::Value where
      F: Fn() -> Self::Value
    {
      let (index, rest) = key.take_bits(MASK_SIZE);
      let inner = self.pairs.get_mut_or(index, <_>::default);
      inner.get_mut_or(rest, ctor)
    }

    fn make<F>(_ctor: &F) -> Self where
      F: Fn() -> Self::Value
    {
      Self::default()
    }

    fn slot(&mut self, key: K) -> Slot<Self::Value> {
      unimplemented!()
    }
  }

  impl<L> Default for LinearLevel<L> {
    fn default() -> Self {
      Self { pairs: <_>::default() }
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub mod table_level {
  use super::*;

  const MASK_SIZE: u32 = 64;

  pub struct TableLevel<L> {
    pairs: Table<usize, L>,
  }

  impl<L> TableLevel<L> {
    fn split_key<K>(&self, key: K) -> (usize, K) where
      K: TrieKeyRepr
    {
      key.take_bits(MASK_SIZE)
    }
  }

  impl<K, L> TrieLevel<K> for TableLevel<L> where
    K: Copy + TrieKeyRepr, L: TrieLevel<K>
  {
    type Value = L::Value;

    fn clear(&mut self) { 
      self.pairs.clear();
    }

    fn dump_info(&self) { 
      println!("TABLE_LEVEL:");
      println!("  len: {}", self.pairs.len());
    }

    #[inline]
    fn get(&self, key: K) -> Option<&Self::Value> {
      let (index, rest) = self.split_key(key);
      self.pairs.get(index).and_then(|inner| {
        inner.get(rest)
      })
    }

    #[inline]
    fn get_mut(&mut self, key: K) -> Option<&mut Self::Value> {
      let (index, rest) = self.split_key(key);
      self.pairs.get_mut(index).and_then(|inner| {
        inner.get_mut(rest)
      })
    }

    #[inline]
    fn get_mut_or<F>(&mut self, key: K, ctor: F) -> &mut Self::Value where
      F: Fn() -> Self::Value
    {
      let (index, rest) = self.split_key(key);
      let entry = self.pairs.entry(index);
      let inner = entry.or_insert_with(|| L::make(&ctor));
      inner.get_mut_or(rest, ctor)
    }

    #[inline]
    fn make<F>(_ctor: &F) -> Self where
      F: Fn() -> Self::Value
    {
      Self::default()
    }

    fn remove(&mut self, key: K) { 
      let (index, _rest) = self.split_key(key);
      self.pairs.remove(index);
    }

    fn slot(&mut self, key: K) -> Slot<Self::Value> {
      unimplemented!()
    }
  }

  impl<L> Default for TableLevel<L> {
    fn default() -> Self {
      Self {
        //pairs: <_>::default() 
        pairs: Table::with_capacity(16_000_000) 
      }
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod test {
  use super::{PointerTrie};
  use ::test::{Bencher, black_box};

  #[bench]
  fn bench_lookup(b: &mut Bencher) {
    let mut trie = PointerTrie::<usize, u32>::new();
    trie.get_mut_or(0x9999_9999_9999, || 4567);
    trie.get_mut_or(0x1234567890, || 456);

    let trie = black_box(trie);
    let key = black_box(0x1234567890);
    b.iter(|| {
      assert_eq!(trie.get(key), Some(&456));
    })
  }

  #[test]
  fn test_lookup() {
    let mut trie = PointerTrie::<usize, u32>::new();

    trie.get_mut_or(123, || 456);
    trie.get_mut_or(1234, || 4567);

    assert_eq!(trie.get(123), Some(&456));
    assert_eq!(trie.get(1234), Some(&4567));
    assert_eq!(trie.get(777), None);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
