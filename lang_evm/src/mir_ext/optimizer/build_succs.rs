use {
  crate::{
    mir,
    optimizer::prelude::*,
  },
  north_core::{
    Node, 
    visitor::prelude::*,
  },
  north_derive::{
    aspect_rules
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

struct DefaultRule;
struct HasSuccRule;

aspect_rules! {
  <N: Node + ?Sized> visit_r![BuildSuccs, N => DefaultRule] {
    fn visit_ok(self, _ctx, _node, _sb) { }
  }

  <N: Node + ?Sized> visit_r![BuildSuccs, N => HasSuccRule] {
    fn visit_ok(self, ctx, _node, sb) {
      if let Some(id) = ctx.next_sibling_id() {
        sb.add_succ(id);
      }
    }
  }
}

lang_part! {
  BuildSuccsPart {
    visit_r![BuildSuccs, _                     => HasSuccRule],
    visit_r![BuildSuccs, mir::Block            => DefaultRule],
    visit_r![BuildSuccs, mir::CtlBr            => DefaultRule],
    visit_r![BuildSuccs, mir::CtlFork          => DefaultRule],
    visit_r![BuildSuccs, mir::CtlLoopBegin     => DefaultRule],
    visit_r![BuildSuccs, mir::CtlReduce        => DefaultRule],
    visit_r![BuildSuccs, mir::CtlRet           => DefaultRule],
    visit_r![BuildSuccs, mir::StmtCallRule     => DefaultRule],
    visit_r![BuildSuccs, mir::StmtCallRuleDyn  => DefaultRule],
    visit_r![BuildSuccs, mir::StmtLoopBegin    => DefaultRule],
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
