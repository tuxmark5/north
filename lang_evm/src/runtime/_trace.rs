use {
  crate::{
    collections::IndexSet,
    runtime::TaskData,
  },
  /*std::{
    collections::HashSet,
  },*/
};

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Eq, Hash, PartialEq)]
pub struct TraceKey {
  pub state: u32,
  pub origin_idx: u32,
  pub position: *const u8
}

impl TraceKey {
  pub fn new(data: &TaskData) -> Self {
    Self { 
      state: data.state, 
      origin_idx: data.origin_idx,
      position: data.cursor.start 
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct TraceSet {
  set: IndexSet<TraceKey>
}

impl TraceSet {
  pub fn new() -> Self {
    Self { set: IndexSet::default() }
  }

  pub fn dump_info(&self) {
    println!("TRACE SET:");
    println!("  num entries: {}", self.set.len());
  }

  pub fn trace(&mut self, key: TraceKey) -> bool {
    self.set.insert(key)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
