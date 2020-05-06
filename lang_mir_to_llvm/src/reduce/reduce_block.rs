use {
  lang_llvm_sys::{BasicBlockRed, ContextExt, ValueRed},
  lang_mir::mir,
  llvm_sys::{prelude::*},
  north_gen::prelude::*,
};

////////////////////////////////////////////////////////////////////////////////////////////////

struct DefaultRule;

aspect_rules! {
  reduce_r![BasicBlockRed, mir::Block => DefaultRule] {
    fn reduce(self, ctx, _node, _builder) -> LLVMBasicBlockRef {
      ctx.make_bb()
    }

    fn reduce_late(self, ctx, node, builder, block) {
      builder.position_at_end(block);
      for stmt in &node.head { ctx.reduce_cld_with(ValueRed, stmt, builder); }
      ctx.reduce_cld_with(ValueRed, &node.tail, builder);
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

lang_part! {
  ReduceBlockPart {
    reduce_r![BasicBlockRed, mir::Block => DefaultRule],
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
