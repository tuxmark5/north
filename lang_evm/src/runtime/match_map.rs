use {
  crate::{
    collections::IndexMap,
    runtime::{MatchId, ReduceId},
  },
  std::{
    ops::RangeInclusive,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub struct MatchKey {
  pub match_id: MatchId,
  pub reduce_id: ReduceId,
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Default)]
pub struct MatchMap {
  crate map: IndexMap<MatchKey, u16>,
}

impl MatchMap {
  pub fn new() -> Self {
    Self { map: IndexMap::default() }
  }

  pub fn clear(&mut self) {
    self.map.clear();
  }

  pub fn get(&self, match_id: MatchId, reduce_id: ReduceId) -> Option<u16> {
    let key = MatchKey { match_id, reduce_id };
    self.map.get(&key).cloned()
  }

  pub fn insert(&mut self, match_id: MatchId, reduce_id: ReduceId, prec: u16) {
    let key = MatchKey { match_id, reduce_id };
    self.map.insert(key, prec);
  }

  pub fn len(&self) -> usize {
    self.map.len()
  }

  pub fn matches(
    &self, 
    match_id: MatchId, 
    reduce_id: ReduceId, 
    range: RangeInclusive<u16>
  ) -> bool {
    let key = MatchKey { match_id, reduce_id };
    match self.map.get(&key) {
      Some(prec) => range.contains(prec),
      None => false,
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
