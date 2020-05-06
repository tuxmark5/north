use {
  crate::ast,
  lang_mir::{
    codegen_ctx::{CodegenCtxExt},
    mir,
    reductions::TypeRed,
  },
  north_core::{NodeId},
  north_derive::aspect_rules,
  north_gen::prelude::*,
};

////////////////////////////////////////////////////////////////////////////////////////////////

struct DefaultRule;

aspect_rules! {
  reduce_r![TypeRed, ast::TypeRule => DefaultRule] {
    fn reduce(self, ctx, node, arg) -> NodeId<dyn mir::Type> {
      ctx.make_node_up(mir::TypeFn {
        param_tys: ctx.reduce_link_n(&node.param_types, arg),
        result_ty: ctx.reduce_link(node.result_type, arg).into(),
      })
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

lang_part! {
  ReduceTypePart {
    reduce_r![TypeRed, ast::TypeRule => DefaultRule],
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
