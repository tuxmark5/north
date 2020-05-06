use {
  crate::{
    collections::{IndexMap, IntervalMap},
    runtime::MatchId,
  },
  north_core::{
    visitor::Visitor,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub mod closure;
pub mod closure_builder;
pub mod closure_interner;
pub mod dfa;
pub mod dfa_builder;
pub mod group;
pub mod group_builder;
pub mod optimizer_aspect;
pub mod rule_builder;
pub mod succ_builder;

pub use self::{
  closure::{Closure, ClosureId, ClosureSeed},
  closure_builder::{ClosureBuilder, ClosureBuilderState},
  dfa::{DFA},
  dfa_builder::{DFABuilderState},
  group::{Group, GroupDyn, UniqueGroup},
  group_builder::{GroupBuilder, GroupSet},
  optimizer_aspect::{OptimizerAspect},
  rule_builder::{RuleBuilder},
  succ_builder::{SuccBuilder},
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub mod prelude {
  pub use super::{
    BuildClosure, BuildDFA, BuildGroups, BuildSuccs,
    closure::{ClosureSeed},
    group::{UniqueGroup},
    optimizer_aspect::{OptimizerAspect},
  };

  pub use north_core::{
    compiler::aspect::aspect_cast,
    context::{
      ModelCellCtxExt,
      ModelNodeIdCtxExt,
      NodeCellCtxExt,
      NodeIdCtxExt,
    },
  };
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct BuildClosure;

impl Visitor for BuildClosure {
  type ImplicitArgs = ();
  type ExplicitArgs = ClosureBuilderState;
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct BuildDFA;

impl Visitor for BuildDFA {
  type ImplicitArgs = ();
  type ExplicitArgs = DFABuilderState;
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct BuildGroups;

impl Visitor for BuildGroups {
  type ImplicitArgs = RuleBuilder;
  type ExplicitArgs = GroupBuilder;
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct BuildSuccs;

impl Visitor for BuildSuccs {
  type ImplicitArgs = ();
  type ExplicitArgs = SuccBuilder;
}

////////////////////////////////////////////////////////////////////////////////////////////////
pub type MatchMap = IndexMap<MatchId, IntervalMap<u16, ClosureSeed>>;
////////////////////////////////////////////////////////////////////////////////////////////////

// pub fn optimize_rules(rules: Vec<NodeId>) -> NodeId<mir::ItemRule> {
//   COMPILER.with(|comp| {
//     let mut rule_builder = RuleBuilder::new(comp);
//     rule_builder.build(&rules).1
//   })
// }

////////////////////////////////////////////////////////////////////////////////////////////////
