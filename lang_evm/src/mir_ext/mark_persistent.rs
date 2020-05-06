use {
  crate::{
    mir,
    base_ext::gen_mir::PERSISTENT_BLOCK_ID,
    optimizer::prelude::*,
  },
  north_core::{
    Node,
    node_id::{ToNodeId},
    visitor::prelude::*,
  },
  north_derive::{
    aspect_rules
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct MarkPersistent;

impl Visitor for MarkPersistent {
  type ImplicitArgs = ();
  type ExplicitArgs = ();
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub fn mark_persistent_block<A>(block: A) where
  A: ToNodeId<Node=mir::Block>
{
  let block = block.to_node_id();
  if let None = cell_try_get!(PERSISTENT_BLOCK_ID(block)) {
    let block_pid = block.into(): usize;
    cell_set!(PERSISTENT_BLOCK_ID(block), block_pid);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub fn mark_persistent_recursive<I: ToNodeId>(aspect: &VisitorAspect, id: I) {
  let node_id = id.to_top();
  let core = VisitCtxCore::<MarkPersistent> { aspect, node_id, imp_args: &mut () };
  let _ = aspect.visit(core, &mut ());
}

////////////////////////////////////////////////////////////////////////////////////////////////

struct DefaultRule;
struct MarkNothingRule;

aspect_rules! {
  <N: Node + ?Sized> visit_r![MarkPersistent, N => MarkNothingRule] {
    fn visit_ok(self, _ctx, _node, _sb) { }
  }

  visit_r![MarkPersistent, mir::CtlFork => DefaultRule] {
    fn visit_ok(self, _ctx, node, _arg) {
      for block in &node.blocks[1..] { 
        mark_persistent_block(block); 
      }
    }
  }

  visit_r![MarkPersistent, mir::CtlMatchSym => DefaultRule] {
    fn visit_ok(self, _ctx, node, _arg) {
      for entry in &node.pos { mark_persistent_block(&entry.block); }
      for entry in &node.neg { mark_persistent_block(&entry.block); }
    }
  }

  visit_r![MarkPersistent, mir::ItemRule => DefaultRule] {
    fn visit_ok(self, ctx, node, arg) {
      mark_persistent_block(&node.blocks[0]);
      for block in &node.blocks { 
        let tail = { ctx.model().node(block).tail.to_top() };
        let _ = ctx.visit(tail, arg);
      }
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

lang_part! {
  MarkPersistentPart {
    visit_r![MarkPersistent, _ => MarkNothingRule],
    visit_r![MarkPersistent, mir::CtlFork => DefaultRule],
    visit_r![MarkPersistent, mir::CtlMatchSym => DefaultRule],
    visit_r![MarkPersistent, mir::ItemRule => DefaultRule],
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
