use {
  crate::ast,
  lang_mir::{
    CodegenCtxExt,
    Cursor,
    ModRed,
    ValueRed,
    mir::{self},
  },
  north_core::prelude::*,
  north_derive::aspect_rules,
  north_gen::prelude::*,
};

////////////////////////////////////////////////////////////////////////////////////////////////

struct DefaultRule;

aspect_rules! {
  reduce_r![ModRed, ast::Items => DefaultRule] {
    fn reduce(self, ctx, _node, _arg) -> NodeId<mir::Mod> {
      ctx.make_node(mir::Mod { items: Vec::new() })
    }

    fn reduce_late(self, ctx, node, _arg, mod_id) {
      let output_model = ctx.output_model().clone();
      let block_id = NodeId::from_id(0xFFFF_FFFF);
      let mut cur = Cursor::new(output_model, block_id);

      // descendant items here
      ctx.set_mod_id(mod_id);
      for item in &node.items {
        let _ = ctx.reduce_cld_with(ValueRed, item, &mut cur);
      }
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

lang_part! {
  ReduceModPart {
    reduce_r![ModRed, ast::Items => DefaultRule],
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
