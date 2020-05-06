use {
  std::{
    borrow::Borrow,
    collections::HashMap,
    hash::Hash,
  }
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Trie<K, V> {
  children: HashMap<K, Box<Trie<K, V>>>,
  value: Option<V>,
}

impl<K, V> Trie<K, V> where
  K: Eq + Hash + PartialEq
{
  pub fn new() -> Self {
    Self {
      children: HashMap::new(),
      value: None,
    }
  }

  pub fn insert<I>(&mut self, mut key: I, value: V) where
    I: Iterator<Item=K>
  {
    match key.next() {
      Some(key_part) => self.children
        .entry(key_part)
        .or_default()
        .insert(key, value),
      None => {
        self.value = Some(value);
      }
    }
  }

  pub fn subtrie<Q>(&self, key: &Q) -> Option<&Trie<K, V>> where
    K: Borrow<Q>, Q: Eq + Hash
  {
    self.children.get(key).map(|v| &**v)
  }

  pub fn value(&self) -> &Option<V> {
    &self.value
  }
}

impl<K, V> Default for Trie<K, V> where
  K: Eq + Hash + PartialEq
{
  fn default() -> Self {
    Self::new()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
