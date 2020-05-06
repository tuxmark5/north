use {
  crate::{ast, sema::derive_scope::Down},
  north_core::prelude::*,
  north_derive::aspect_rules,
  north_scope::prelude::*,
};

////////////////////////////////////////////////////////////////////////////////////////////////

struct DefaultRule;

////////////////////////////////////////////////////////////////////////////////////////////////

aspect_rules! {
  resolve_r![ast::ExprVar => DefaultRule] {
    fn resolve(self, ctx) {
      let scope = ctx.own_scope::<Down>();
      let mut node = ctx.node_mut();
      ctx.resolve_ref(&mut node.var, scope);
    }
  }

  resolve_r![ast::TypeVar => DefaultRule] {
    fn resolve(self, ctx) {
      let scope = ctx.own_scope::<Down>();
      let mut node = ctx.node_mut();
      ctx.resolve_ref(&mut node.var, scope);
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

lang_part! {
  ResolvePart {
    resolve_r![ast::ExprVar => DefaultRule],
    resolve_r![ast::TypeVar => DefaultRule],
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
