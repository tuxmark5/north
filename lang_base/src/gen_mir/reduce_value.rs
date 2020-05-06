use {
  crate::{
    ast,
    gen_mir::{
      P_LOOP_DESCR,
      P_VAR_MUTABLE,
      LoopDescr,
      type_of_reduced
    },
    sema::resolve_constr::P_LOOP_ANCESTOR,
  },
  lang_mir::{
    codegen_ctx::CodegenCtxExt,
    mir::{self, StmtOpBinType, RValLinkOpt},
    reductions::{LValRed, ValueRed},
  },
  north_core::{
    context::{ModelNodeIdCtxExt, NodeIdCtxExt},
    model::Link,
    prelude::*,
  },
  north_derive::aspect_rules,
  north_gen::prelude::*,
  std::fmt::Debug
};

////////////////////////////////////////////////////////////////////////////////////////////////

struct DefaultRule;
struct ExprOpBinRule(StmtOpBinType);
struct ExprOpBinRuleOld(i32);
struct ExprOpUnaryRule(i32);

aspect_rules! {
  reduce_r![ValueRed, ast::DeclLet => DefaultRule] {
    fn reduce(self, ctx, node, cur) -> RValLinkOpt {
      ctx.gen_prop_own_set(P_VAR_MUTABLE, true);

      let ty = type_of_reduced(ctx, ctx.node_id());
      let local = ctx.make_local(ty.into());

      if let Some(init) = node.init {
        let init = ctx.reduce_cld(init, cur).unwrap();
        cur.stmt_set(local.clone().cast(), init);
      }

      Some(local)
    }
  }

  reduce_r![ValueRed, ast::DeclRuleParam => DefaultRule] {
    fn reduce(self, ctx, _node, _cur) -> RValLinkOpt {
      ctx.gen_prop_own_set(P_VAR_MUTABLE, true);

      let ty = type_of_reduced(ctx, ctx.node_id());
      let param = ctx.make_param(ty.into());
      Some(param)
    }
  }

  reduce_r![ValueRed, ast::DeclRuleParams => DefaultRule] {
    fn reduce(self, ctx, node, cur) -> RValLinkOpt {
      for param in &node.params {
        let _ = ctx.reduce_cld(param, cur);
      }
      None
    }
  }

  reduce_r![ValueRed, ast::ExprBlock => DefaultRule] {
    fn reduce(self, ctx, node, cur) -> RValLinkOpt {
      let mut result = None;

      for elem in &node.elems {
        result = ctx.reduce_cld(*elem, cur);
      }

      result
    }
  }

  reduce_r![ValueRed, ast::ExprBreak => DefaultRule] {
    fn reduce(self, ctx, _node, cur) -> RValLinkOpt {
      let target_loop = ctx.prop(P_LOOP_ANCESTOR).unwrap();
      let loop_descr = ctx.gen_prop(target_loop, P_LOOP_DESCR).unwrap();
      cur.ctl_br(loop_descr.loop_end);
      None
    }
  }

  reduce_r![ValueRed, ast::ExprCall => DefaultRule] {
    fn reduce(self, ctx, node, cur) -> RValLinkOpt {
      let target = ctx.reduce_cld(&node.target, cur).unwrap();
      let args = ctx.reduce_cld_n(&node.args, cur).into_iter()
        .map(Option::unwrap).collect::<Vec<_>>();
      Some(cur.stmt_call(target, args))
    }
  }

  reduce_r![ValueRed, ast::ExprContinue => DefaultRule] {
    fn reduce(self, ctx, _node, cur) -> RValLinkOpt {
      let target_loop = ctx.prop(P_LOOP_ANCESTOR).unwrap();
      let loop_descr = ctx.gen_prop(target_loop, P_LOOP_DESCR).unwrap();
      cur.ctl_br(loop_descr.loop_start);
      None
    }
  }

  reduce_r![ValueRed, ast::ExprIf => DefaultRule] {
    fn reduce(self, ctx, node, cur) -> RValLinkOpt {
      let end = ctx.block();
      let cond = ctx.reduce_link(node.cond, cur).unwrap();

      let mut block1 = ctx.block_cur();
      let _value1 = ctx.reduce_cld(node.body1, &mut block1);
      block1.ctl_br(end);

      if let Some(body0) = node.body0 {
        let mut block0 = ctx.block_cur();
        let _value0 = ctx.reduce_cld(body0, &mut block0);
        block0.ctl_br(end);
        cur.ctl_if(cond, &block1, &block0);
      } else {
        cur.ctl_if(cond, &block1, end);
      }

      cur.seek(end);

      None // assign value; phi
    }
  }

  reduce_r![ValueRed, ast::ExprLitInt => DefaultRule] {
    fn reduce(self, ctx, node, cur) -> RValLinkOpt {
      let mir_type = type_of_reduced(ctx, ctx.node_id());
      let value_str = &node.lit.data.value;
      let value = value_str.parse::<i64>().unwrap();
      cur.const_int(mir_type.cast().into(), value)
    }
  }

  reduce_r![ValueRed, ast::ExprLoop => DefaultRule] {
    fn reduce(self, ctx, node, cur) -> RValLinkOpt {
      let mut body = ctx.block_start(cur);
      let body_id = body.orig_block_id();
      let end = ctx.block();

      ctx.gen_prop_own_set(P_LOOP_DESCR, LoopDescr {
        loop_start: body.orig_block_id(),
        loop_end: end,
      });

      let _ = ctx.reduce_cld(node.body, &mut body);
      body.ctl_br(body_id);

      cur.seek(end);

      None
    }
  }

  reduce_r![ValueRed, ast::ExprOpAriSet => DefaultRule] {
    fn reduce(self, ctx, node, cur) -> RValLinkOpt {
      let left = ctx.reduce_cld_with(LValRed, node.left, cur);
      let right = ctx.reduce_cld(node.right, cur).unwrap();
      cur.stmt_set(left, right);
      None
    }
  }

  <O: Debug> reduce_r![ValueRed, ast::ExprOpBin<O> => ExprOpBinRule] {
    fn reduce(self, ctx, node, cur) -> RValLinkOpt {
      let left = ctx.reduce_cld(node.left, cur).unwrap();
      let right = ctx.reduce_cld(node.right, cur).unwrap();
      Some(cur.stmt_op_bin(self.0, left, right))
    }
  }

  <O: Debug> reduce_r![ValueRed, ast::ExprOpBin<O> => ExprOpBinRuleOld] {
    fn reduce(self, ctx, node, cur) -> RValLinkOpt {
      let _left = ctx.reduce_cld(node.left, cur);
      let _right = ctx.reduce_cld(node.right, cur);
      // do the op
      None
    }
  }

  <O: Debug> reduce_r![ValueRed, ast::ExprOpPre<O> => ExprOpUnaryRule] {
    fn reduce(self, ctx, node, cur) -> RValLinkOpt {
      ctx.reduce_cld(node.inner, cur): Option<Link<_>>;
      // do the op
      None
    }
  }

  reduce_r![ValueRed, ast::ExprReturn => DefaultRule] {
    fn reduce(self, ctx, node, cur) -> RValLinkOpt {
      let value = node.value.and_then(|v| ctx.reduce_cld(v, cur));
      cur.ctl_ret(value);
      None
    }
  }

  reduce_r![ValueRed, ast::ExprVar => DefaultRule] {
    fn reduce(self, ctx, node, cur) -> RValLinkOpt {
      let target_node = node.var.target_node.unwrap();
      let lvalue = ctx.reduce_ref(target_node, cur).unwrap();

      let mutable = ctx.gen_prop(target_node, P_VAR_MUTABLE);
      match mutable {
        Some(true) => Some(cur.stmt_use(lvalue.cast())),
        _ => Some(lvalue),
      }
    }
  }

  reduce_r![ValueRed, ast::ExprWhile => DefaultRule] {
    fn reduce(self, ctx, node, cur) -> RValLinkOpt {
      let mut start = ctx.block_start(cur);
      let mut body = ctx.block_cur();
      let end = ctx.block();

      ctx.gen_prop_own_set(P_LOOP_DESCR, LoopDescr {
        loop_start: start.orig_block_id(),
        loop_end: end,
      });

      let cond = ctx.reduce_cld(node.cond, &mut start).unwrap();
      start.ctl_if(cond, &body, end);

      let _ = ctx.reduce_cld(node.body, &mut body);
      body.ctl_br(&start);

      cur.seek(end);

      None
    }
  }

  reduce_r![ValueRed, ast::ItemBuiltins => DefaultRule] {
    fn reduce(self, _ctx, _node, _cur) -> RValLinkOpt {
      None
    }
  }

  reduce_r![ValueRed, ast::ItemFn => DefaultRule] {
    fn reduce(self, ctx, node, _cur) -> RValLinkOpt {
      let name = node.name.data.to_string();
      let fn_ty = type_of_reduced(ctx, ctx.node_id()).cast().into();
      let out = ctx.make_item(mir::ItemFn::new(name, fn_ty));
      Some(out.up().into())
    }

    fn reduce_late(self, ctx, node, cur, fn_id) {
      let fn_id = fn_id.unwrap().to_node_id().cast();
      ctx.set_func_id(fn_id);

      let _ = ctx.reduce_cld(node.params, cur);

      let mut block = ctx.block_cur();
      let ret_value = ctx.reduce_cld(node.body, &mut block);
      if !block.is_completed() {
        block.ctl_ret(ret_value);
      }
    }
  }

  reduce_r![ValueRed, ast::StmtDecl => DefaultRule] {
    fn reduce(self, ctx, node, cur) -> RValLinkOpt {
      ctx.reduce_cld(node.decl, cur)
    }
  }

  reduce_r![ValueRed, ast::StmtExpr => DefaultRule] {
    fn reduce(self, ctx, node, cur) -> RValLinkOpt {
      let result = ctx.reduce_cld(node.expr, cur);
      match node.semi {
        Some(_) => None,
        None => result,
      }
    }
  }

  reduce_r![ValueRed, ast::StmtItem => DefaultRule] {
    fn reduce(self, _ctx, _node, _cur) -> RValLinkOpt {
      None
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

lang_part! {
  ReduceValuePart {
    reduce_r![ValueRed, ast::DeclLet          => DefaultRule],
    reduce_r![ValueRed, ast::DeclRuleParam    => DefaultRule],
    reduce_r![ValueRed, ast::DeclRuleParams   => DefaultRule],
    reduce_r![ValueRed, ast::ExprBlock        => DefaultRule],
    reduce_r![ValueRed, ast::ExprBreak        => DefaultRule],
    reduce_r![ValueRed, ast::ExprCall         => DefaultRule],
    reduce_r![ValueRed, ast::ExprContinue     => DefaultRule],
    reduce_r![ValueRed, ast::ExprIf           => DefaultRule],
    reduce_r![ValueRed, ast::ExprLitInt       => DefaultRule],
    reduce_r![ValueRed, ast::ExprLoop         => DefaultRule],
    reduce_r![ValueRed, ast::ExprOpAriAdd     => ExprOpBinRule(StmtOpBinType::AriAdd)],
    reduce_r![ValueRed, ast::ExprOpAriAddSet  => ExprOpBinRuleOld(0)],
    reduce_r![ValueRed, ast::ExprOpAriDiv     => ExprOpBinRule(StmtOpBinType::AriDiv)],
    reduce_r![ValueRed, ast::ExprOpAriDivSet  => ExprOpBinRuleOld(0)],
    reduce_r![ValueRed, ast::ExprOpAriMod     => ExprOpBinRule(StmtOpBinType::AriMod)],
    reduce_r![ValueRed, ast::ExprOpAriModSet  => ExprOpBinRuleOld(0)],
    reduce_r![ValueRed, ast::ExprOpAriNeg     => ExprOpUnaryRule(0)],
    reduce_r![ValueRed, ast::ExprOpAriMul     => ExprOpBinRule(StmtOpBinType::AriMul)],
    reduce_r![ValueRed, ast::ExprOpAriMulSet  => ExprOpBinRuleOld(0)],
    reduce_r![ValueRed, ast::ExprOpAriSet     => DefaultRule],
    reduce_r![ValueRed, ast::ExprOpAriSub     => ExprOpBinRule(StmtOpBinType::AriSub)],
    reduce_r![ValueRed, ast::ExprOpAriSubSet  => ExprOpBinRuleOld(0)],
    reduce_r![ValueRed, ast::ExprOpBitAnd     => ExprOpBinRule(StmtOpBinType::BitAnd)],
    reduce_r![ValueRed, ast::ExprOpBitOr      => ExprOpBinRule(StmtOpBinType::BitOr)],
    reduce_r![ValueRed, ast::ExprOpBitNot     => ExprOpUnaryRule(0)],
    reduce_r![ValueRed, ast::ExprOpBitShl     => ExprOpBinRule(StmtOpBinType::BitShl)],
    reduce_r![ValueRed, ast::ExprOpBitShr     => ExprOpBinRule(StmtOpBinType::BitShr)],
    reduce_r![ValueRed, ast::ExprOpBitXor     => ExprOpBinRule(StmtOpBinType::BitXor)],
    reduce_r![ValueRed, ast::ExprOpCmpEq      => ExprOpBinRule(StmtOpBinType::CmpEq)],
    reduce_r![ValueRed, ast::ExprOpCmpGt      => ExprOpBinRule(StmtOpBinType::CmpGt)],
    reduce_r![ValueRed, ast::ExprOpCmpGtEq    => ExprOpBinRule(StmtOpBinType::CmpGtEq)],
    reduce_r![ValueRed, ast::ExprOpCmpLt      => ExprOpBinRule(StmtOpBinType::CmpLt)],
    reduce_r![ValueRed, ast::ExprOpCmpLtEq    => ExprOpBinRule(StmtOpBinType::CmpLtEq)],
    reduce_r![ValueRed, ast::ExprOpCmpNotEq   => ExprOpBinRule(StmtOpBinType::CmpNotEq)],
    reduce_r![ValueRed, ast::ExprOpLogAnd     => ExprOpBinRuleOld(0)],
    reduce_r![ValueRed, ast::ExprOpLogNot     => ExprOpUnaryRule(0)],
    reduce_r![ValueRed, ast::ExprOpLogOr      => ExprOpBinRuleOld(0)],
    reduce_r![ValueRed, ast::ExprReturn       => DefaultRule],
    reduce_r![ValueRed, ast::ExprVar          => DefaultRule],
    reduce_r![ValueRed, ast::ExprWhile        => DefaultRule],
    reduce_r![ValueRed, ast::ItemBuiltins     => DefaultRule],
    reduce_r![ValueRed, ast::ItemFn           => DefaultRule],
    reduce_r![ValueRed, ast::StmtDecl         => DefaultRule],
    reduce_r![ValueRed, ast::StmtExpr         => DefaultRule],
    reduce_r![ValueRed, ast::StmtItem         => DefaultRule],
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
