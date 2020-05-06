use {
  crate::{
    ast,
    sema::resolve_constr::P_FN_ANCESTOR,
  },
  north_core::{
    cell::{prelude::*}, 
    prelude::*
  },
  north_derive::aspect_rules,
  north_typesys::{
    InferTypesCtxCore as CtxCore,
    prelude::*,
  },
  std::fmt::Debug,
};

////////////////////////////////////////////////////////////////////////////////////////////////

struct AriOpRule;
struct BitOpRule;
struct CmpOpRule;
struct DefaultRule;
struct LogOpRule;
struct NeverRule;
//struct UnitRule;

////////////////////////////////////////////////////////////////////////////////////////////////

aspect_rules! {
  infer_types_r![ast::DeclLet => DefaultRule] {
    fn infer_types(self, ctx, node) {
      if let Some(node_type) = node.ty {
        let self_type = ctx.type_of_self();
        ctx.type_eq(self_type, node_type.cast());
      }

      if let Some(init) = node.init {
        let self_type = ctx.type_of_self();
        let init_type = ctx.type_of(init);
        ctx.type_eq(self_type, init_type);
      }
    }
  }

  infer_types_r![ast::DeclRuleParam => DefaultRule] {
    fn infer_types(self, ctx, node) {
      let self_type = ctx.type_of_self();
      ctx.type_eq(self_type, node.ty.cast());
    }
  }

  infer_types_r![ast::ExprBlock => DefaultRule] {
    fn infer_types(self, ctx, node) {
      let self_type = ctx.type_of_self();
      let last_node = node.elems.last().cloned();
      let last_type = type_of_opt(&ctx, last_node);
      ctx.type_eq(self_type, last_type);
    }
  }

  infer_types_r![ast::ExprCall => DefaultRule] {
    fn infer_types(self, ctx, node) {
      let type_node = ast::TypeFn {
        param_types: ctx.type_of_n(node.args.iter()),
        result_type: ctx.type_of_self().cast(),
      };

      let target_type = ctx.make_type(type_node);
      let target_type_b = ctx.type_of(node.target);
      ctx.type_eq(target_type, target_type_b);
    }
  }

  infer_types_r![ast::ExprIf => DefaultRule] {
    fn infer_types(self, ctx, node) {
      let cond_type = ctx.type_of(node.cond);
      let bool_type = ctx.make_type(ast::TypeBool { });
      ctx.type_eq(cond_type, bool_type);

      let self_type = ctx.type_of_self();
      let body1_type = ctx.type_of(node.body1);
      ctx.type_eq(self_type, body1_type);

      if let Some(body0) = node.body0 {
        let body0_type = ctx.type_of(body0);
        ctx.type_eq(self_type, body0_type);
      }

      // if B1 is missing, then the type is B0
      // if B0 is never, then type is B1
      // else type is B1
    }
  }

  infer_types_r![ast::ExprLitInt => DefaultRule] {
    fn infer_types(self, ctx, _node) {
      let self_type = ctx.type_of_self();
      let i32_type = ctx.make_type(ast::TypeInt::new(true, 32));
      ctx.type_eq(self_type, i32_type);
    }
  }

  <O: Clone + Debug> infer_types_r![ast::ExprOpBin<O> => AriOpRule] {
    fn infer_types(self, ctx, node) {
      let self_type = ctx.type_of_self();
      let left_type = ctx.type_of(node.left);
      ctx.type_eq(self_type, left_type);

      let right_type = ctx.type_of(node.right);
      ctx.type_eq(self_type, right_type);
    }
  }

  <O: Clone + Debug> infer_types_r![ast::ExprOpBin<O> => BitOpRule] {
    fn infer_types(self, ctx, node) {
      let self_type = ctx.type_of_self();
      let left_type = ctx.type_of(node.left);
      ctx.type_eq(self_type, left_type);

      let right_type = ctx.type_of(node.right);
      let i32_type = ctx.make_type(ast::TypeInt::new(true, 32));
      ctx.type_eq(right_type, i32_type);
    }
  }

  <O: Clone + Debug> infer_types_r![ast::ExprOpBin<O> => CmpOpRule] {
    fn infer_types(self, ctx, node) {
       let self_type = ctx.type_of_self();
      let bool_type = ctx.make_type(ast::TypeBool { });
      ctx.type_eq(self_type, bool_type);

      let left_type = ctx.type_of(node.left);
      let right_type = ctx.type_of(node.right);
      ctx.type_eq(left_type, right_type);
    }
  }

  <O: Clone + Debug> infer_types_r![ast::ExprOpBin<O> => LogOpRule] {
    fn infer_types(self, ctx, node) {
      let self_type = ctx.type_of_self();
      let bool_type = ctx.make_type(ast::TypeBool { });
      ctx.type_eq(self_type, bool_type);

      let left_type = ctx.type_of(node.left);
      ctx.type_eq(left_type, bool_type);

      let right_type = ctx.type_of(node.right);
      ctx.type_eq(right_type, bool_type);
    }
  }

  <O: Clone + Debug> infer_types_r![ast::ExprOpPre<O> => AriOpRule] {
    fn infer_types(self, ctx, node) {
      let self_type = ctx.type_of_self();
      let inner_type = ctx.type_of(node.inner);
      ctx.type_eq(self_type, inner_type);
    }
  }

  <O: Clone + Debug> infer_types_r![ast::ExprOpPre<O> => BitOpRule] {
    fn infer_types(self, ctx, node) {
      let self_type = ctx.type_of_self();
      let inner_type = ctx.type_of(node.inner);
      ctx.type_eq(self_type, inner_type);
    }
  }

  <O: Clone + Debug> infer_types_r![ast::ExprOpPre<O> => LogOpRule] {
    fn infer_types(self, ctx, node) {
      let self_type = ctx.type_of_self();
      let bool_type = ctx.make_type(ast::TypeBool { });
      ctx.type_eq(self_type, bool_type);

      let inner_type = ctx.type_of(node.inner);
      ctx.type_eq(inner_type, bool_type);
    }
  }

  infer_types_r![ast::ExprReturn => DefaultRule] {
    fn infer_types(self, ctx, node) {
      let self_type = ctx.type_of_self();
      let unit_type = ctx.make_type(ast::TypeUnit { });
      ctx.type_eq(self_type, unit_type);

      //if let Some(ancestor_id) = ctx.prop(P_FN_ANCESTOR) {
      if let Some(ancestor_id) = cell_try_get!(P_FN_ANCESTOR(ctx.node_id())) {
        let ret_ty_opt = ctx.model().node(ancestor_id).ret_ty;
        let fn_ret_ty = make_type_opt(&ctx, ret_ty_opt);
        let value_ty = type_of_opt(&ctx, node.value);
        ctx.type_eq(fn_ret_ty, value_ty);
      } else {
        panic!("no function ancestor");
      }
    }
  }

  infer_types_r![ast::ExprVar => DefaultRule] {
    fn infer_types(self, ctx, node) {
      if let Some(target_node) = node.var.target_node {
        let self_type = ctx.type_of_self();
        let target_type = ctx.type_of(target_node);
        ctx.type_eq(self_type, target_type);
      }
    }
  }

  infer_types_r![ast::ExprWhile => DefaultRule] {
    fn infer_types(self, ctx, node) {
      let cond_type = ctx.type_of(node.cond);
      let bool_type = ctx.make_type(ast::TypeBool { });
      ctx.type_eq(cond_type, bool_type);

      let self_type = ctx.type_of_self();
      let unit_type = ctx.make_type(ast::TypeUnit { });
      ctx.type_eq(self_type, unit_type);
    }
  }

  infer_types_r![ast::ItemFn => DefaultRule] {
    fn infer_types(self, ctx, node) {
      let params_node = ctx.fetch_node_owned(node.params);
      let result_type = make_type_opt(&mut ctx, node.ret_ty);

      let type_node = ast::TypeFn {
        param_types: ctx.type_of_n(params_node.params.iter()),
        result_type: result_type.cast(), // DEREF
      };

      let self_type = ctx.type_of_self();
      let fn_type = ctx.make_type(type_node);
      ctx.type_eq(self_type, fn_type);

      // defer this to mir:
      //let body_type = ctx.type_of(node.body);
      //
      //ctx.type_eq(body_type, result_type);
    }
  }

  infer_types_r![ast::StmtExpr => DefaultRule] {
    fn infer_types(self, ctx, node) {
      let self_type = ctx.type_of_self();
      if node.semi.is_some() {
        let unit_type = ctx.make_type(ast::TypeUnit { });
        ctx.type_eq(self_type, unit_type);
      } else {
        let expr_type = ctx.type_of(node.expr);
        ctx.type_eq(self_type, expr_type);
      }
    }
  }

  <N: Node + ?Sized> infer_types_r![N => NeverRule] {
    fn infer_types(self, ctx, _node) {
      let self_type = ctx.type_of_self();
      let unit_type = ctx.make_type(ast::TypeNever { });
      ctx.type_eq(self_type, unit_type);
    }
  }

//  [(N: Node + ?Sized), N => UnitRule] {
//    fn infer_types(self, ctx, _node) {
//      let self_type = ctx.type_of_self();
//      let unit_type = ctx.make_type(ast::TypeUnit { });
//      ctx.type_eq(self_type, unit_type);
//    }
//  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

lang_part! {
  InferTypesPart {
    infer_types_r![ast::DeclLet          => DefaultRule],
    infer_types_r![ast::DeclRuleParam    => DefaultRule],
    infer_types_r![ast::ExprBlock        => DefaultRule],
    infer_types_r![ast::ExprBreak        => NeverRule],
    infer_types_r![ast::ExprCall         => DefaultRule],
    infer_types_r![ast::ExprContinue     => NeverRule],
    infer_types_r![ast::ExprIf           => DefaultRule],
    infer_types_r![ast::ExprLitInt       => DefaultRule],
    infer_types_r![ast::ExprOpAriAdd     => AriOpRule],
    infer_types_r![ast::ExprOpAriAddSet  => AriOpRule],
    infer_types_r![ast::ExprOpAriDiv     => AriOpRule],
    infer_types_r![ast::ExprOpAriDivSet  => AriOpRule],
    infer_types_r![ast::ExprOpAriMod     => AriOpRule],
    infer_types_r![ast::ExprOpAriModSet  => AriOpRule],
    infer_types_r![ast::ExprOpAriNeg     => AriOpRule],
    infer_types_r![ast::ExprOpAriMul     => AriOpRule],
    infer_types_r![ast::ExprOpAriMulSet  => AriOpRule],
    infer_types_r![ast::ExprOpAriSet     => AriOpRule],
    infer_types_r![ast::ExprOpAriSub     => AriOpRule],
    infer_types_r![ast::ExprOpAriSubSet  => AriOpRule],
    infer_types_r![ast::ExprOpBitAnd     => BitOpRule],
    infer_types_r![ast::ExprOpBitOr      => BitOpRule],
    infer_types_r![ast::ExprOpBitNot     => BitOpRule],
    infer_types_r![ast::ExprOpBitShl     => BitOpRule],
    infer_types_r![ast::ExprOpBitShr     => BitOpRule],
    infer_types_r![ast::ExprOpBitXor     => BitOpRule],
    infer_types_r![ast::ExprOpCmpEq      => CmpOpRule],
    infer_types_r![ast::ExprOpCmpGt      => CmpOpRule],
    infer_types_r![ast::ExprOpCmpGtEq    => CmpOpRule],
    infer_types_r![ast::ExprOpCmpLt      => CmpOpRule],
    infer_types_r![ast::ExprOpCmpLtEq    => CmpOpRule],
    infer_types_r![ast::ExprOpCmpNotEq   => CmpOpRule],
    infer_types_r![ast::ExprOpLogAnd     => LogOpRule],
    infer_types_r![ast::ExprOpLogNot     => LogOpRule],
    infer_types_r![ast::ExprOpLogOr      => LogOpRule],
    infer_types_r![ast::ExprReturn       => NeverRule],
    infer_types_r![ast::ExprVar          => DefaultRule],
    infer_types_r![ast::ExprWhile        => DefaultRule],
    infer_types_r![ast::ItemFn           => DefaultRule],
    infer_types_r![ast::StmtExpr         => DefaultRule],
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub fn make_type_opt<B>(ctx: &CtxCore, type_id: Option<NodeId<B>>)
  -> NodeId where B: Node + ?Sized
{
  match type_id {
    Some(type_id) => type_id.cast(),
    None => ctx.make_type(ast::TypeUnit { }),
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub fn type_of_opt<B>(ctx: &CtxCore, node_id: Option<NodeId<B>>)
  -> NodeId where B: Node + ?Sized
{
  match node_id {
    Some(node_id) => ctx.type_of(node_id),
    None => ctx.make_type(ast::TypeUnit { }),
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
