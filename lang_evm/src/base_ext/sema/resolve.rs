use {
  crate::ast,
  lang_base::sema::derive_scope::Down,
  north_derive::aspect_rules,
  north_scope::prelude::*,
};

////////////////////////////////////////////////////////////////////////////////////////////////

struct DefaultRule;

aspect_rules! {
  resolve_r![ast::AttrPartOf => DefaultRule] {
    fn resolve(self, ctx) {
      let scope = ctx.own_scope::<Down>();
      let mut node = ctx.node_mut();
      ctx.resolve_ref(&mut node.rule, scope);
    }
  }

  resolve_r![ast::GExprCall => DefaultRule] {
    fn resolve(self, ctx) {
      let scope = ctx.own_scope::<Down>();
      let mut node = ctx.node_mut();
      ctx.resolve_ref(&mut node.target, scope);
    }
  }

  resolve_r![ast::ItemGroup => DefaultRule] {
    fn resolve(self, ctx) {
      let scope = ctx.own_scope::<Down>();
      let mut node = ctx.node_mut();
      ctx.resolve_ref(&mut node.rule, scope);
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

lang_part! {
  ResolvePart {
    resolve_r![ast::AttrPartOf => DefaultRule],
    resolve_r![ast::GExprCall  => DefaultRule],
    resolve_r![ast::ItemGroup => DefaultRule],
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
