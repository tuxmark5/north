use {
  fnv::FnvHasher,
  std::{
    collections::{HashMap as StdHashMap, HashSet as StdHashSet},
    hash::BuildHasherDefault,
    marker::PhantomData,
    ptr::{self},
  }
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub mod bit_mask;
pub mod block_vec;
pub mod fixed_trie;
pub mod histogram;
pub mod linear_map;
pub mod linked_list;
pub mod linked_map;
pub mod small_map;
pub mod table;

pub use self::{
  bit_mask::{BitMask, BitMaskBuilder},
  block_vec::{BlockVec},
  fixed_trie::{PointerTrie},
  histogram::{Histogram},
  linear_map::{LinearMap},
  linked_list::{LinkedList},
  linked_map::{LinkedMap},
  small_map::{SmallMap},
  table::{Table},
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub use {
  indexmap::{
    map as index_map,
    set as index_set,
  },
  ::interval_map::{
    self, IntervalMap
  },
  std::{
    collections::{hash_map, hash_set},
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub type HashMap<K, V> = StdHashMap<K, V, BuildHasherDefault<FnvHasher>>;
pub type HashSet<V> = StdHashSet<V, BuildHasherDefault<FnvHasher>>;
pub type IndexMap<K, V> = indexmap::IndexMap<K, V, BuildHasherDefault<FnvHasher>>;
pub type IndexSet<K> = indexmap::IndexSet<K, BuildHasherDefault<FnvHasher>>;

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Bucket<K, V> {
  pub key: K,
  pub value: V,
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub enum Slot<'a, V: 'a> {
  Occupied(OccupiedSlot<'a, V>),
  Vacant(VacantSlot<'a, V>),
}

impl<'a, V> Slot<'a, V> {
  pub fn new(value: &'a mut V) -> Self {
    Slot::Occupied(OccupiedSlot { slot: value })
  }

  pub fn new_uninit(value: *mut V) -> Self {
    Slot::Vacant(VacantSlot { slot: value, lifetime: PhantomData })
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct OccupiedSlot<'a, V: 'a>  {
  slot: &'a mut V
}

impl<'a, V> OccupiedSlot<'a, V> {
  pub fn into_value(self) -> &'a mut V {
    self.slot
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct VacantSlot<'a, V: 'a>  {
  slot: *mut V,
  lifetime: PhantomData<&'a mut ()>,
}

impl<'a, V> VacantSlot<'a, V> {
  pub fn insert(self, value: V) -> &'a mut V {
    unsafe {
      ptr::write(self.slot, value);
      &mut *self.slot
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
