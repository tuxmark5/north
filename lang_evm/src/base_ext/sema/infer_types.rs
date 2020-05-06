use {
  crate::ast,
  lang_base::sema::infer_types::make_type_opt,
  north_derive::aspect_rules,
  north_typesys::prelude::*,
};

////////////////////////////////////////////////////////////////////////////////////////////////

struct DefaultRule;

aspect_rules! {
  infer_types_r![ast::AttrPartOf => DefaultRule] {
    fn infer_types(self, ctx, node) {
      let rule_node = ctx.ancestor_ids().skip(1).next().unwrap();
      let rule_type = ctx.type_of(rule_node);
      let target_type = ctx.type_of(&node.rule);
      ctx.type_eq(rule_type, target_type);
    }
  }

  infer_types_r![ast::GExprCall => DefaultRule] {
    fn infer_types(self, ctx, node) {
      let type_node = ast::TypeRule {
        param_types: vec![], //ctx.type_of_n(node.args.iter()),
        result_type: ctx.type_of_self().cast(),
      };

      let target_type = ctx.make_type(type_node);
      let target_type_b = ctx.type_of(&node.target);
      ctx.type_eq(target_type, target_type_b);
    }
  }

  infer_types_r![ast::ItemRule => DefaultRule] {
    fn infer_types(self, ctx, node) {
      let params_node = ctx.fetch_node_owned(node.params);
      let result_type = make_type_opt(&ctx, node.ret_ty);

      let type_node = ast::TypeRule {
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

  infer_types_r![ast::ItemRuleDyn => DefaultRule] {
    fn infer_types(self, ctx, node) {
      let params_node = ctx.fetch_node_owned(node.params);
      let result_type = make_type_opt(&ctx, node.ret_ty);

      let type_node = ast::TypeRule {
        param_types: ctx.type_of_n(params_node.params.iter()),
        result_type: result_type.cast(), // DEREF
      };

      let self_type = ctx.type_of_self();
      let fn_type = ctx.make_type(type_node);
      ctx.type_eq(self_type, fn_type);
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

lang_part! {
  InferTypesPart {
    infer_types_r![ast::AttrPartOf  => DefaultRule],
    infer_types_r![ast::GExprCall   => DefaultRule],
    infer_types_r![ast::ItemRule    => DefaultRule],
    infer_types_r![ast::ItemRuleDyn => DefaultRule],
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
