use {
  crate::{
    collections::fixed_trie::{TrieKey},
    mir::ReduceKind,
    reduction_name,
    runtime::{ParserCtx},
    util::{Index, Interner},
  },
  north_core::{
    node_id::{NodeId, ToNodeId},
    quote::{QuoteType, RustQuoter, rust_type},
  },
  std::{
    fmt::{self, Debug},
    ops::{RangeInclusive},
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////
pub type CallSpec = &'static [CallElem];
pub type CallSpecId = Index<u32, CallSpecIdTag>;
pub enum CallSpecIdTag { }
pub type CallSpecInterner = Interner<Vec<CallElem>, CallSpecId>;

pub type GrammarId = Index<u16, GrammarIdTag>;
pub enum GrammarIdTag { }

pub type MatchSpec = &'static [MatchElem];
pub type MatchSpecId = Index<u32, MatchSpecIdTag>;
pub enum MatchSpecIdTag { }
pub type MatchSpecInterner = Interner<Vec<MatchElem>, MatchSpecId>;

pub type StateId = Index<u32, StateIdTag>;
pub enum StateIdTag { }
////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize)]
#[repr(C)]
pub struct CallElem {
  pub match_id: MatchId,
  pub min_prec: u16,
}

impl QuoteType for CallElem {
  fn quote(q: &mut RustQuoter) -> rust_type::Type {
    rust_type::Type::make_struct(vec![
      q.quote::<MatchId>(),
      q.quote::<u16>(),
    ])
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub type ChartId = Index<u32, ChartIdTag>;
pub enum ChartIdTag { }

impl Debug for ChartId {
  fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
    write!(fmt, "C[{}]", self.idx())
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize)]
#[repr(C)]
pub struct MatchElem {
  pub match_id: MatchId,
  pub prec_range: RangeInclusive<u16>,
  pub state_id: StateId, // this should be separated probably
}

// &[MatchElem], &[StateId]

impl QuoteType for MatchElem {
  fn quote(q: &mut RustQuoter) -> rust_type::Type {
    rust_type::Type::make_struct(vec![
      q.quote::<MatchId>(),
      q.quote::<(u16, u16)>(),
      q.quote::<StateId>(),
    ])
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub type MatchId = Index<u16, MatchIdTag>;
pub type MatchIdInterner = Interner<NodeId, MatchId>;
pub enum MatchIdTag { }

impl MatchId {
  pub fn new<I: ToNodeId>(node_id: I) -> Self {
    let ctx = ParserCtx::get();
    let node_id = node_id.to_top();
    ctx.match_id_interner.intern(node_id)
  }
}

impl Debug for MatchId {
  fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
    let ctx = ParserCtx::get();
    let node_id = ctx.match_id_interner.get(*self);
    let node_idx = node_id.idx() as u32;
    let name = reduction_name(node_idx);
    write!(fmt, "<{}:{}>", name, node_idx)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub type PosId = Index<u32, PosIdTag>;
pub enum PosIdTag { }

impl PosId {
  pub fn advance(&self, size: u32) -> PosId {
    PosId::from_id(self.idx() + size)
  }

  pub fn size(from: PosId, to: PosId) -> u32 {
    to.idx() - from.idx()
  }
}

impl TrieKey for PosId {
  type Repr = usize;

  fn into_repr(self) -> Self::Repr {
    self.into()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub type ReduceId = Index<u16, ReduceIdTag>;
pub type ReduceIdInterner = Interner<NodeId, ReduceId>;
pub enum ReduceIdTag { }

impl ReduceId {
  pub fn new<I: ToNodeId>(node_id: I) -> Self {
    let ctx = ParserCtx::get();
    let node_id = node_id.to_top();
    ctx.reduce_id_interner.intern(node_id)
  }
}

impl Debug for ReduceId {
  fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
    let ctx = ParserCtx::get();
    let node_id = ctx.reduce_id_interner.get(*self);
    let node_idx = node_id.idx() as u32;
    let name = reduction_name(node_idx);
    let text = format!("<{}:{}>", name, node_idx);
    fmt.pad(text.as_ref())
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Copy)]
pub struct ReduceSpec {
  pub reduce_kind: ReduceKind, 
  pub reduce_id: ReduceId,
}

////////////////////////////////////////////////////////////////////////////////////////////////
