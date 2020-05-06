use {
  crate::{
    mir,
    optimizer::{
      closure_builder::{ClosureFlags as CF},
      prelude::*,
    },
  },
  north_core::{
    Node, 
    iter::ModelIterator,
    visitor::prelude::*,
  },
  north_derive::{
    aspect_rules
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

struct AsyncRule;
struct DefaultRule;
struct SyncRule;

aspect_rules! {
  <N: Node + ?Sized> visit_r![BuildClosure, N => AsyncRule] {
    fn visit_ok(self, ctx, _node, cb) {
      cb.visit(CF::RELEVANT, ctx.node_id());
      if let Some(id) = ctx.next_sibling_id() {
        cb.visit(CF::ASYNC_SUCC, id);
      }
    }
  }

  <N: Node + ?Sized> visit_r![BuildClosure, N => SyncRule] {
    fn visit_ok(self, ctx, _node, cb) {
      cb.visit(CF::RELEVANT, ctx.node_id());
      if let Some(id) = ctx.next_sibling_id() {
        cb.visit(CF::SYNC_SUCC, id);
      }
    }
  }

  visit_r![BuildClosure, mir::Block => DefaultRule] {
    fn visit_ok(self, _ctx, node, cb) {
      cb.visit(CF::BLOCK_FIRST, node.first_node());
    }
  }

  visit_r![BuildClosure, mir::CtlBr => DefaultRule] {
    fn visit_ok(self, _ctx, node, cb) {
      cb.visit(CF::BLOCK, node.block.to_top());
    }
  }

  visit_r![BuildClosure, mir::CtlFork => DefaultRule] {
    fn visit_ok(self, _ctx, node, cb) {
      for block in &node.blocks {
        cb.visit(CF::BLOCK, block.to_top());
      }
    }
  }

  visit_r![BuildClosure, mir::CtlLoopBegin => DefaultRule] {
    fn visit_ok(self, ctx, node, cb) {
      cb.visit(CF::RELEVANT, ctx.node_id());
      cb.visit(CF::BLOCK, node.next.to_top());
    }
  }

  visit_r![BuildClosure, mir::CtlReduce => DefaultRule] {
    fn visit_ok(self, ctx, _node, cb) {
      cb.visit(CF::REDUCE | CF::RELEVANT, ctx.node_id());

      // if cb.visit_reduce { }
    }
  }

  visit_r![BuildClosure, mir::CtlRet => DefaultRule] {
    fn visit_ok(self, _ctx, _node, _cb) { }
  }

  visit_r![BuildClosure, mir::ItemRule => DefaultRule] {
    fn visit_ok(self, _ctx, node, cb) {
      cb.visit(CF::BLOCK, &node.blocks[0]);
    }
  }

  visit_r![BuildClosure, mir::StmtCallRule => DefaultRule] {
    fn visit_ok(self, ctx, node, cb) {
      cb.visit(CF::CALL, ctx.node_id());
      cb.visit(CF::CALL_TARGET, &node.target);
      if let Some(id) = ctx.next_sibling_id() {
        cb.visit(CF::ASYNC_SUCC, id);
      }
    }
  }

  visit_r![BuildClosure, mir::StmtCallRuleDyn => DefaultRule] {
    fn visit_ok(self, ctx, node, cb) {
      cb.visit(CF::CALL, ctx.node_id());
      
      if cb.visit_calls {
        let grammar = cb.grammar_cell();
        for (match_id, min_prec) in &node.call_spec {
          for node_id in grammar.matching_iter(*match_id, *min_prec) {
            cb.visit(CF::CALL_TARGET, node_id);
          }
        }
      }
      
      if let Some(id) = ctx.next_sibling_id() {
        cb.visit(CF::ASYNC_SUCC, id);
      }
    }
  }
}

lang_part! {
  BuildClosurePart {
    visit_r![BuildClosure, _                     => SyncRule],
    visit_r![BuildClosure, mir::Block            => DefaultRule],
    visit_r![BuildClosure, mir::CtlBr            => DefaultRule],
    visit_r![BuildClosure, mir::CtlFork          => DefaultRule],
    visit_r![BuildClosure, mir::CtlLoopBegin     => DefaultRule],
    visit_r![BuildClosure, mir::CtlReduce        => DefaultRule],
    visit_r![BuildClosure, mir::CtlRet           => DefaultRule],
    visit_r![BuildClosure, mir::ItemRule         => DefaultRule],
    visit_r![BuildClosure, mir::StmtCallRule     => DefaultRule],
    visit_r![BuildClosure, mir::StmtCallRuleDyn  => DefaultRule],
    visit_r![BuildClosure, mir::StmtLoopBegin    => AsyncRule],
    visit_r![BuildClosure, mir::StmtReduce       => AsyncRule],
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
