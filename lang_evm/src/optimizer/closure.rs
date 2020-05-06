use {
  itertools::Itertools,
  north_core::{
    node_id::{NodeId, ToNodeId},
  },
  std::{
    cmp::Ordering,
    slice
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Debug, Default, Eq, Hash, PartialEq)]
pub struct Closure {
  crate entries: Vec<NodeId>
}

impl Closure {
  pub fn new(entries: Vec<NodeId>) -> Self {
    Self { entries }
  }

  pub fn is_empty(&self) -> bool {
    self.entries.is_empty()
  }

  pub fn iter(&self) -> slice::Iter<NodeId> {
    self.entries.iter()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ClosureId(pub usize);

impl ClosureId {
  pub const EMPTY: ClosureId = ClosureId(0);

  pub fn is_empty(&self) -> bool {
    self.0 == 0
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Debug, Default, Eq, Hash, PartialEq)]
pub struct ClosureSeed {
  crate follow_calls: bool,
  crate priority: u32,
  crate entries: Vec<NodeId>  
}

impl ClosureSeed {
  pub fn new(entries: Vec<NodeId>) -> Self {
    Self { follow_calls: false, priority: 0, entries }
  }

  pub fn new_pair(a: &ClosureSeed, b: &ClosureSeed) -> Self {
    let a = a.entries.iter();
    let b = b.entries.iter();
    Self::new(a.merge(b).cloned().collect())
  }

  pub fn new_single<I: ToNodeId>(id: I) -> Self {
    Self::new(vec![id.to_top()])
  }

  pub fn new_single_opt<I: ToNodeId>(id: &Option<I>) -> Self {
    let entries = id.iter().map(|i| i.to_top()).collect();
    Self::new(entries)
  }

  pub fn new_single_with_priority<I: ToNodeId>(id: I, priority: u32) -> Self {
    let entries = vec![id.to_top()];
    Self { follow_calls: false, priority, entries }
  }

  pub fn insert(&mut self, id: NodeId) {
    if !self.entries.contains(&id) {
      self.entries.push(id);
    }
  }

  pub fn into_closure(self) -> Closure {
    Closure::new(self.entries)
  }

  pub fn is_empty(&self) -> bool {
    self.entries.is_empty()
  }

  pub fn merge(&mut self, other: &ClosureSeed) {
    match other.priority.cmp(&self.priority) {
      Ordering::Less => {}
      Ordering::Equal => {
        for e in &other.entries {
          self.insert(*e);
        }
      }
      Ordering::Greater => {
        self.entries.clear();
        self.entries.extend(other.entries.iter());
        self.priority = other.priority;
      }
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
