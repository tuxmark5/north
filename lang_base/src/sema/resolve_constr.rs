use {
  crate::ast,
  north_core::{
    cell::prelude::*,
    prelude::*,
    visitor::prelude::*,
  },
  north_derive::{
    aspect_rules, cell
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

#[cell(scope(compiler, node))]
pub static P_FN_ANCESTOR: NodeId<ast::ItemFn> = ();

//impl_prop!(P_FN_ANCESTOR, NodeId<ast::ItemFn>);
impl_prop!(P_LOOP_ANCESTOR, NodeId<dyn ast::Loop>);

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct ResolveConstrs;

impl Visitor for ResolveConstrs {
  type ImplicitArgs = ();
  type ExplicitArgs = ();
}

////////////////////////////////////////////////////////////////////////////////////////////////

struct DefaultRule;

aspect_rules! {
  visit_r![ResolveConstrs, ast::ExprBreak => DefaultRule] {
    fn visit(self, ctx, _node) {
      test_loop_ancestor(ctx.model_cell(), ctx.node_id())
    }
  }

  visit_r![ResolveConstrs, ast::ExprContinue => DefaultRule] {
    fn visit(self, ctx, _node) {
      test_loop_ancestor(ctx.model_cell(), ctx.node_id())
    }
  }

  visit_r![ResolveConstrs, ast::ExprReturn => DefaultRule] {
    fn visit(self, ctx, _node) {
      let ancestor = find_ancestor::<ast::ItemFn>(&*ctx.model(), ctx.node_id());
      match ancestor {
        Some(outer_fn) => {
          cell_set!(P_FN_ANCESTOR(ctx.node_id()), outer_fn);
          Ok(())
        }

        None => {
          println!("BAD STUFF");
          Err(())
        }
      }
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

lang_part! {
  ResolveConstrsPart {
    visit_r![ResolveConstrs, ast::ExprBreak => DefaultRule],
    visit_r![ResolveConstrs, ast::ExprContinue => DefaultRule],
    visit_r![ResolveConstrs, ast::ExprReturn => DefaultRule],
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

fn find_ancestor<N>(model: &Model, node_id: NodeId) -> Option<NodeId<N>> where
  N: 'static + Node
{
  model.ancestor_ids(node_id)
    .find_map(|id| model.cast_id::<_, N>(id))
}

fn find_loop_ancestor(model: &Model, node_id: NodeId) -> Option<NodeId<dyn ast::Loop>> {
  let mut prev_id = node_id;

  for ancestor_id in model.ancestor_ids(node_id) {
    let ancestor = &*model.data(ancestor_id);
    if let Ok(_) = model.cast::<_, &dyn ast::Item>(ancestor) { break; }
    if let Ok(loop_node) = model.cast::<_, &dyn ast::Loop>(ancestor) {
      let link_id = model.parent_link_id(prev_id);
      if loop_node.is_loop_ctl_allowed(link_id) {
        return Some(ancestor_id.cast());
      }
    }
    prev_id = ancestor_id;
  }

  None
}

////////////////////////////////////////////////////////////////////////////////////////////////

// TODO: link_id!(Self, field_name)

pub fn test_loop_ancestor(model_cell: &ModelCell, node_id: NodeId) -> Result<(), ()> {
  let ancestor = find_loop_ancestor(&*model_cell.borrow(), node_id);

  match ancestor {
    Some(loop_expr) => {
      let mut model = model_cell.borrow_mut();
      model.prop_set(node_id, P_LOOP_ANCESTOR, loop_expr);
      Ok(())
    }

    None => {
      panic!("BAD STUFF: node must be a descendant of a loop");
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
