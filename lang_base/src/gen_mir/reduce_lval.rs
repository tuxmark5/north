use {
  crate::{
    ast,
    gen_mir::P_VAR_MUTABLE,
  },
  lang_mir::{
    mir::{LValLink},
    reductions::{LValRed, ValueRed},
  },
  north_core::prelude::*,
  north_derive::aspect_rules,
  north_gen::prelude::*,
};

////////////////////////////////////////////////////////////////////////////////////////////////

struct DefaultRule;

aspect_rules! {
  reduce_r![LValRed, ast::ExprVar => DefaultRule] {
    fn reduce(self, ctx, node, cur) -> LValLink {
      let target_node = node.var.target_node.unwrap();
      let lvalue = ctx.reduce_ref_with(ValueRed, target_node, cur).unwrap();

      let mutable = ctx.gen_prop(target_node, P_VAR_MUTABLE);
      match mutable {
        Some(true) => lvalue.cast(),
        _ => panic!("BAD lvalue"),
      }
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

lang_part! {
  ReduceLValPart {
    reduce_r![LValRed, ast::ExprVar => DefaultRule],
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
