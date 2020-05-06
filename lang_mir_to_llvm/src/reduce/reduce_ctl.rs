use {
  lang_llvm_sys::{
    BasicBlockRed, ValueRed
  },
  lang_mir::mir,
  llvm_sys::{
    prelude::*,
  },
  north_gen::prelude::*,
};

////////////////////////////////////////////////////////////////////////////////////////////////

struct DefaultRule;

aspect_rules! {
  reduce_r![ValueRed, mir::CtlBr => DefaultRule] {
    fn reduce(self, ctx, node, builder) -> LLVMValueRef {
      let dest = ctx.reduce_link_with(BasicBlockRed, &node.block, builder);
      builder.build_br(dest)
    }
  }

  reduce_r![ValueRed, mir::CtlIf => DefaultRule] {
    fn reduce(self, ctx, node, builder) -> LLVMValueRef {
      let cond = ctx.reduce_link(&node.cond, builder);
      let then_b = ctx.reduce_link_with(BasicBlockRed, &node.block1, builder);
      let else_b = ctx.reduce_link_with(BasicBlockRed, &node.block0, builder);
      builder.build_cond_br(cond, then_b, else_b)
    }
  }

  reduce_r![ValueRed, mir::CtlRet => DefaultRule] {
    fn reduce(self, ctx, node, builder) -> LLVMValueRef {
      if let Some(ref value) = node.value {
        let value = ctx.reduce_link(value, builder);
        builder.build_ret(value)
      } else {
        builder.build_ret_void()
      }
    }
  }

  reduce_r![ValueRed, mir::CtlSwitch => DefaultRule] {
    fn reduce(self, ctx, node, builder) -> LLVMValueRef {
      let value = ctx.reduce_link(&node.value, builder);
      let default = ctx.reduce_link_with(BasicBlockRed, &node.default_case, builder);

      let cases = node.cases.iter()
        .map(|(val, block)| {
          let val = ctx.reduce_link(val, builder);
          let block = ctx.reduce_link_with(BasicBlockRed, block, builder);
          (val, block)
        })
        .collect::<Vec<_>>();

      builder.build_switch(value, default, cases.as_ref())
    }
  }

  reduce_r![ValueRed, mir::CtlSwitchRange => DefaultRule] {
    fn reduce(self, _ctx, _node, _builder) -> LLVMValueRef {

      unimplemented!()
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

lang_part! {
  ReduceCtlPart {
    reduce_r![ValueRed, mir::CtlBr          => DefaultRule],
    reduce_r![ValueRed, mir::CtlIf          => DefaultRule],
    reduce_r![ValueRed, mir::CtlRet         => DefaultRule],
    reduce_r![ValueRed, mir::CtlSwitch      => DefaultRule],
    reduce_r![ValueRed, mir::CtlSwitchRange => DefaultRule],
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
