use {
  crate::ast,
  north_derive::aspect_rules,
  north_typesys::prelude::*,
};

////////////////////////////////////////////////////////////////////////////////////////////////

struct DefaultRule;

aspect_rules! {
  unify_types_r![ast::TypeRule => DefaultRule] {
    fn unify_types(self, ctx, a, b) {
      if a.param_types.len() != b.param_types.len() {
        panic!("param number mismatch");
      }

      let params_a = a.param_types.iter();
      let params_b = b.param_types.iter();
      for (param_a, param_b) in params_a.zip(params_b) {
        ctx.unify(*param_a, *param_b);
      }

      ctx.unify(a.result_type, b.result_type);
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

lang_part! {
  UnifyTypesPart {
    unify_types_r![ast::TypeRule => DefaultRule],
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
