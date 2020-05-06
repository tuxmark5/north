use {
  lang_llvm_sys::{Builder, ContextExt, ModRed, ValueRed},
  lang_mir::mir,
  llvm_sys::{prelude::*},
  north_gen::prelude::*,
};

////////////////////////////////////////////////////////////////////////////////////////////////

struct DefaultRule;

aspect_rules! {
  reduce_r![ModRed, mir::Mod => DefaultRule] {
    fn reduce(self, ctx, _node, _arg) -> LLVMModuleRef {
      ctx.llvm_ctx().create_module("unnamed_module")
    }

    fn reduce_late(self, ctx, node, _arg, curr_mod) {
      ctx.inner.curr_mod = Some(curr_mod);

      let mut builder = Builder::new(ctx.inner.llvm_ctx.clone());
      for item in &node.items {
        ctx.reduce_cld_with(ValueRed, item, &mut builder);
      }
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

lang_part! {
  ReduceModPart {
    reduce_r![ModRed, mir::Mod => DefaultRule],
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
