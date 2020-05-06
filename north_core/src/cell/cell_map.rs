use {
  crate::NodeId,
  std::{
    any::{Any, TypeId},
    collections::{HashMap, hash_map::Entry},
    mem::transmute_copy,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Eq, Hash, PartialEq)]
pub struct Key {
  pub key_id: TypeId,
  pub scope: Scope,
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Eq, Hash, PartialEq)]
pub enum Scope {
  Global,
  Node(NodeId)
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait ToScope {
  fn to_scope(&self) -> Scope;
}

impl ToScope for () {
  fn to_scope(&self) -> Scope { Scope::Global }
}

impl ToScope for NodeId {
  fn to_scope(&self) -> Scope { Scope::Node(*self) }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Default)]
pub struct CellMap {
  map: HashMap<Key, Box<dyn Any>>
}

impl CellMap {
  pub fn new() -> Self {
    Self {
      map: HashMap::new(),
    }
  }

  pub unsafe fn get<T: Clone>(&self, key: Key) -> Option<T> {
    let value = &**self.map.get(&key)?;
    let value = transmute_copy::<_, &T>(&value);
    Some((*value).clone())
  }

  pub unsafe fn get_or_default<T>(&mut self, key: Key) -> T where
    T: 'static + Clone + Default
  {
    let value = match self.map.entry(key) {
      Entry::Occupied(occ) => {
        let value = &**occ.get();
        transmute_copy::<_, &T>(&value)
      },
      Entry::Vacant(occ) => {
        let value = &**occ.insert(Box::new(T::default()));
        transmute_copy::<_, &T>(&value)
      },
    };
    (*value).clone()
  }

  pub fn set<T: 'static>(&mut self, key: Key, value: T) {
    let value: Box<dyn Any> = Box::new(value);
    self.map.insert(key, value);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
