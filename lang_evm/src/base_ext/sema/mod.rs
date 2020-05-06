use {
  crate::ast,
  north_core::{
    context::ModelNodeIdCtxExt,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub mod collect_impls;
pub mod derive_scope;
pub mod infer_types;
pub mod resolve;
pub mod unify_types;

////////////////////////////////////////////////////////////////////////////////////////////////

pub fn get_prefer_shift<C: ModelNodeIdCtxExt>(ctx: &C) -> (u32, bool) {
  let mut dom_group = 0;
  let mut prefer_shift = false;

  let model = ctx.model();
  for ancestor in model.ancestor_ids(ctx.node_id()) {
    if let Some(dom) = model.try_get::<_, ast::GExprDom>(ancestor) {
      dom_group = 1;
    } else if let Some(_) = model.try_get::<_, ast::GExprPrefShift>(ancestor) {
      prefer_shift = true;
    }
  }

  (dom_group, prefer_shift)
}

////////////////////////////////////////////////////////////////////////////////////////////////
