use {
  crate::{
    optimizer::DFA,
    runtime::{MatchId, ReduceId},
  },
  interval_map::IntervalMap,
  north_core::{
    model::{Child, Link, Model},
    prelude::*,
    quote::{QuoteType, RustQuoter, rust_type},
    structure::ForestExt,
  },
  std::{
    collections::BTreeMap,
    ops::{RangeInclusive},
    rc::Rc,
  }
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub use lang_mir::{
  mir::*,
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait Call: Node { 
  fn init(&mut self, min_prec: &mut u16, args: Vec<RValLink>) -> MatchId;
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
pub struct GrammarEntry {
  pub match_id: MatchId,
  pub reduce_id: ReduceId,
  pub prec: u16,
  pub rule: NodeId<ItemRule>
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[repr(u8)]
pub enum ReduceKind {
  Reject = 0,
  Avoid  = 1,
  Normal = 2,
  Prefer = 3,
}

impl QuoteType for ReduceKind {
  fn quote(q: &mut RustQuoter) -> rust_type::Type {
    u8::quote(q)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct CtlExecDFA {
  pub dfa: Rc<DFA>,
  pub fail: BlockRef,
  pub next: Vec<BlockRef>,
}

impl Ctl for CtlExecDFA { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct CtlFail {
}

impl Ctl for CtlFail { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct CtlFork {
  pub blocks: Vec<BlockRef>,
}

impl Ctl for CtlFork { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct CtlLoopBegin {
  pub var: LValLink,
  pub next: BlockRef,
}

impl Ctl for CtlLoopBegin { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct CtlLoopEnd {
  pub var: LValLink,
  pub ok: IntervalMap<u32, BlockRef>,
  pub fail: BlockRef,
}

impl Ctl for CtlLoopEnd { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct CtlLoopNext {
  pub var: LValLink,
  pub ok: BlockRef,
  pub max_iter: Option<u32>,
  pub complete: Option<BlockRef>,
}

impl Ctl for CtlLoopNext { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct CtlMatchChar {
  pub char: u32,
  pub dom_group: u32,
  pub prefer_shift: bool,
  pub ok: BlockRef,
  pub fail: BlockRef,
}

impl Ctl for CtlMatchChar { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct CtlMatchClass {
  pub ok: IntervalMap<u8, BlockRef>,
  pub fail: BlockRef,
  pub dom_group: u32,
  pub prefer_shift: bool,
}

impl Ctl for CtlMatchClass { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct CtlMatchSym {
  pub pos: Vec<CtlMatchSymEntry>,
  pub neg: Vec<CtlMatchSymEntry>,
}

impl Ctl for CtlMatchSym { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
pub struct CtlMatchSymEntry {
  pub match_id: MatchId,
  pub prec_range: RangeInclusive<u16>,
  pub block: BlockRef,
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct CtlReduce {
  pub kind: ReduceKind, 
  pub reduce_id: ReduceId,
  pub short: bool,
  pub next: BlockRef,
  pub fail: BlockRef,
}

impl Ctl for CtlReduce { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct ItemGrammar {
  pub impls: Vec<GrammarEntry>,
}

impl Item for ItemGrammar { }
impl Value for ItemGrammar { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct ItemRule {
  pub name: String,
  pub rule_ty: Link<TypeFn>,
  pub params: Vec<Child<RuleParam>>,
  pub locals: Vec<Child<RuleLocal>>,
  pub blocks: Vec<Child<Block>>
}

impl ItemRule {
  pub fn new(name: String, rule_ty: Link<TypeFn>) -> Self {
    Self { 
      name,
      rule_ty,
      params: Vec::new(),
      locals: Vec::new(),
      blocks: Vec::new(),
    }
  }
}

impl Fn for ItemRule {
  fn add_block(&mut self, model: &mut Model) -> BlockId {
    let block_id = model.new_node::<Block>();
    self.blocks.push(block_id.into());
    block_id
  }

  fn add_local(&mut self, model: &mut Model, ty: Link<dyn Type>) -> RValLink {
    let local_node = RuleLocal { ty, index: self.locals.len() };
    let local_id = model.build_node(local_node);
    self.locals.push(local_id.into());
    local_id.up().into()
  }

  fn add_param(&mut self, model: &mut Model, ty: Link<dyn Type>) -> RValLink {
    let param_node = RuleParam { ty, index: self.params.len() };
    let param_id = model.build_node(param_node);
    self.params.push(param_id.into());
    param_id.up().into()
  }
}

impl Item for ItemRule { }
impl Value for ItemRule { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct RuleLocal {
  pub ty: Link<dyn Type>,
  pub index: usize,
}

impl Value for RuleLocal { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct RuleParam {
  pub ty: Link<dyn Type>,
  pub index: usize,
}

impl Value for RuleParam { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct StmtCallRule {
  pub target: RValLink,
  pub match_id: MatchId,
  pub min_prec: u16,
  pub args: Vec<RValLink>,
}

impl Call for StmtCallRule { 
  fn init(&mut self, min_prec: &mut u16, args: Vec<RValLink>) -> MatchId {
    self.min_prec = *min_prec;
    self.args = args;
    self.match_id
  }
}

impl Stmt for StmtCallRule { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct StmtCallRuleDyn {
  pub call_spec: BTreeMap<MatchId, u16>,
  pub args: Vec<RValLink>,
}

impl Call for StmtCallRuleDyn { 
  fn init(&mut self, min_prec: &mut u16, args: Vec<RValLink>) -> MatchId {
    let (match_id, prec) = self.call_spec.iter_mut().next().unwrap();
    if *prec == 0 { *prec = *min_prec; } else { *min_prec = *prec; }
    self.args = args;
    *match_id
  }
}

impl Stmt for StmtCallRuleDyn { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct StmtLoopBegin {
  pub var: LValLink,
  pub depth: usize,
}

impl Stmt for StmtLoopBegin { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct StmtReduce {
  pub kind: ReduceKind, 
  pub reduce_id: ReduceId,
}

impl Stmt for StmtReduce { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Eq, Hash, Node, PartialEq)]
pub struct StmtRewind {
  pub delta: isize,
}

impl Stmt for StmtRewind { }

////////////////////////////////////////////////////////////////////////////////////////////////

concept_map_part! {
  ConceptMapPart {
    CtlExecDFA      => {dyn Ctl},
    CtlFail         => {dyn Ctl},
    CtlFork         => {dyn Ctl},
    CtlLoopBegin    => {dyn Ctl},
    CtlLoopEnd      => {dyn Ctl},
    CtlLoopNext     => {dyn Ctl},
    CtlMatchChar    => {dyn Ctl},
    CtlMatchClass   => {dyn Ctl},
    CtlMatchSym     => {dyn Ctl},
    CtlReduce       => {dyn Ctl},
    ItemGrammar     => {dyn Item, dyn Value},
    ItemRule        => {dyn Fn, dyn Item, dyn Value},
    RuleLocal       => {dyn Value},
    RuleParam       => {dyn Value},
    StmtCallRule    => {dyn Call, dyn Stmt},
    StmtCallRuleDyn => {dyn Call, dyn Stmt},
    StmtLoopBegin   => {dyn Stmt},
    StmtReduce      => {dyn Stmt},
    StmtRewind      => {dyn Stmt},
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
