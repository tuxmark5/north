use {
  lang_llvm_sys::{
    BasicBlockRed, ContextExt, TypeRed, ValueRed
  },
  lang_mir::mir,
  llvm_sys::{
    LLVMIntPredicate, LLVMOpcode,
    core as llvm_core,
    prelude::*,
  },
  north_gen::prelude::*,
  std::{ptr}
};

////////////////////////////////////////////////////////////////////////////////////////////////

struct DefaultRule;

aspect_rules! {
  reduce_r![ValueRed, mir::ConstInt => DefaultRule] {
    fn reduce(self, ctx, node, builder) -> LLVMValueRef {
      let ty = ctx.reduce_link_with(TypeRed, &node.ty, &mut ());
      builder.const_int(ty, node.value)
    }
  }

  reduce_r![ValueRed, mir::ConstStruct => DefaultRule] {
    fn reduce(self, ctx, node, builder) -> LLVMValueRef {
      let mut values = ctx.reduce_link_n(&node.elements, builder);
      builder.const_struct(values.as_mut())
    }
  }

  reduce_r![ValueRed, mir::ItemFn => DefaultRule] {
    fn reduce(self, ctx, node, _builder) -> LLVMValueRef {
      let curr_mod = ctx.curr_mod();
      let name = node.name.as_ref();
      let fn_ty = ctx.reduce_link_with(TypeRed, &node.fn_ty, &mut ());
      ctx.llvm_ctx().add_function(curr_mod, name, fn_ty)
    }

    fn reduce_late(self, ctx, node, builder, curr_fn) {
      ctx.inner.curr_fn = Some(curr_fn);

      let entry = ctx.make_bb();
      builder.position_at_end(entry);

      for param in &node.params {
        let _ = ctx.reduce_cld(param, builder);
      }

      let block0 = node.blocks.first()
        .expect("mir::ItemFn should have at least 1 block");
      let block0 = ctx.reduce_ref_with(BasicBlockRed, block0, builder);
      builder.build_br(block0);

      for block in &node.blocks {
        let _bb = ctx.reduce_cld_with(BasicBlockRed, block, builder);
        builder.reset();
      }
    }
  }

  reduce_r![ValueRed, mir::ItemGlob => DefaultRule] {
    fn reduce(self, ctx, node, builder) -> LLVMValueRef {
      let curr_mod = ctx.curr_mod();
      let ty = ctx.reduce_link_with(TypeRed, &node.ty, &mut ());
      let init = match node.init {
        Some(ref init) => ctx.reduce_link(init, builder),
        None => ptr::null_mut(),
      };

      ctx.llvm_ctx().add_global(curr_mod, "glob", ty, Some(init))
    }
  }

  reduce_r![ValueRed, mir::Local => DefaultRule] {
    fn reduce(self, ctx, node, builder) -> LLVMValueRef {
      let ty = ctx.reduce_link_with(TypeRed, &node.ty, &mut ());
      builder.build_alloca(ty, "l")
    }
  }

  reduce_r![ValueRed, mir::LValDeref => DefaultRule] {
    fn reduce(self, ctx, node, builder) -> LLVMValueRef {
      ctx.reduce_link(&node.value, builder)
    }
  }

  reduce_r![ValueRed, mir::LValMember => DefaultRule] {
    fn reduce(self, ctx, node, builder) -> LLVMValueRef {
      let base = ctx.reduce_cld(&node.base, builder);
      let mut indices = node.path.iter()
        .map(|e| builder.quote_const(e))
        .collect::<Vec<_>>();
      builder.build_in_bounds_gep(base, indices.as_mut())
    }
  }

  reduce_r![ValueRed, mir::Param => DefaultRule] {
    fn reduce(self, ctx, node, builder) -> LLVMValueRef {
      let curr_fn = ctx.curr_fn();
      let raw_param = unsafe { llvm_core::LLVMGetParam(curr_fn, node.index as u32) };
      if node.is_lval {
        let param_ty = ctx.reduce_link_with(TypeRed, &node.ty, &mut ()); // nicer?
        let value = builder.build_alloca(param_ty, "p");
        builder.build_store(raw_param, value);
        value
      } else {
        raw_param
      }
    }
  }

  reduce_r![ValueRed, mir::RValBuiltinFn => DefaultRule] {
    fn reduce(self, _ctx, node, builder) -> LLVMValueRef {
      builder.quote_fn(&node.rust_fn)
    }
  }

  reduce_r![ValueRed, mir::RValMember => DefaultRule] {
    fn reduce(self, ctx, node, builder) -> LLVMValueRef {
      let base = ctx.reduce_cld(&node.base, builder);
      builder.build_extract_value(base, node.path.as_ref())
    }
  }

  reduce_r![ValueRed, mir::StmtCall => DefaultRule] {
    fn reduce(self, ctx, node, builder) -> LLVMValueRef {
      let target = ctx.reduce_link(&node.target, builder);
      let mut args = ctx.reduce_link_n(&node.args, builder);
      builder.build_call(target, args.as_mut())
    }
  }

  reduce_r![ValueRed, mir::StmtOpBin => DefaultRule] {
    fn reduce(self, ctx, node, builder) -> LLVMValueRef {
      let lhs = ctx.reduce_link(&node.left, builder);
      let rhs = ctx.reduce_link(&node.right, builder);
      match map_stmt_bin_op(node.op) {
        BinaryOp::Binary(op) => builder.build_bin_op(op, lhs, rhs),
        BinaryOp::Cmp(pred) => builder.build_icmp(pred, lhs, rhs),
      }
    }
  }

  reduce_r![ValueRed, mir::StmtSet => DefaultRule] {
    fn reduce(self, ctx, node, builder) -> LLVMValueRef {
      let ptr = ctx.reduce_link(&node.target, builder);
      let value = ctx.reduce_link(&node.value, builder);
      builder.build_store(value, ptr)
    }
  }

  reduce_r![ValueRed, mir::StmtStruct => DefaultRule] {
    fn reduce(self, ctx, node, builder) -> LLVMValueRef {
      let ty = ctx.reduce_link_with(TypeRed, &node.ty, &mut ());
      let mut agg = builder.undef(ty);

      for (i, value) in node.values.iter().enumerate() {
        let value = ctx.reduce_link(value, builder);
        agg = builder.build_insert_value(agg, value, i);
      }

      agg
    }
  }

  reduce_r![ValueRed, mir::StmtUse => DefaultRule] {
    fn reduce(self, ctx, node, builder) -> LLVMValueRef {
      let ptr = ctx.reduce_link(&node.lvalue, builder);
      builder.build_load(ptr)
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

lang_part! {
  ReduceValuePart {
    reduce_r![ValueRed, mir::ConstInt       => DefaultRule],
    reduce_r![ValueRed, mir::ConstStruct    => DefaultRule],
    reduce_r![ValueRed, mir::ItemFn         => DefaultRule],
    reduce_r![ValueRed, mir::ItemGlob       => DefaultRule],
    reduce_r![ValueRed, mir::Local          => DefaultRule],
    reduce_r![ValueRed, mir::LValDeref      => DefaultRule],
    reduce_r![ValueRed, mir::LValMember     => DefaultRule],
    reduce_r![ValueRed, mir::Param          => DefaultRule],
    reduce_r![ValueRed, mir::RValBuiltinFn  => DefaultRule],
    reduce_r![ValueRed, mir::RValMember     => DefaultRule],
    reduce_r![ValueRed, mir::StmtCall       => DefaultRule],
    reduce_r![ValueRed, mir::StmtOpBin      => DefaultRule],
    reduce_r![ValueRed, mir::StmtSet        => DefaultRule],
    reduce_r![ValueRed, mir::StmtStruct     => DefaultRule],
    reduce_r![ValueRed, mir::StmtUse        => DefaultRule],
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

enum BinaryOp {
  Binary(LLVMOpcode),
  Cmp(LLVMIntPredicate),
}

fn map_stmt_bin_op(op: mir::StmtOpBinType) -> BinaryOp {
  use self::BinaryOp::*;
  use self::mir::StmtOpBinType::*;

  match op {
    AriAdd   => Binary(LLVMOpcode::LLVMAdd),
    AriDiv   => Binary(LLVMOpcode::LLVMSDiv), // sign
    AriMod   => Binary(LLVMOpcode::LLVMSRem), // sign
    AriMul   => Binary(LLVMOpcode::LLVMMul),
    AriSub   => Binary(LLVMOpcode::LLVMSub),
    BitAnd   => Binary(LLVMOpcode::LLVMAnd),
    BitOr    => Binary(LLVMOpcode::LLVMOr),
    BitShl   => Binary(LLVMOpcode::LLVMShl), // ari
    BitShr   => Binary(LLVMOpcode::LLVMAShr), // ari
    BitXor   => Binary(LLVMOpcode::LLVMXor),
    CmpEq    => Cmp(LLVMIntPredicate::LLVMIntEQ), // sign
    CmpGt    => Cmp(LLVMIntPredicate::LLVMIntSGT), // sign
    CmpGtEq  => Cmp(LLVMIntPredicate::LLVMIntSGE), // sign
    CmpLt    => Cmp(LLVMIntPredicate::LLVMIntSLT), // sign
    CmpLtEq  => Cmp(LLVMIntPredicate::LLVMIntSLE), // sign
    CmpNotEq => Cmp(LLVMIntPredicate::LLVMIntNE), // sign
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
