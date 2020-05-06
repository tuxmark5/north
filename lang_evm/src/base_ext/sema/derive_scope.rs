use {
  crate::ast,
  lang_base::sema::derive_scope::{Down, Up},
  north_core::prelude::*,
  north_derive::aspect_rules,
  north_scope::prelude::*,
};

////////////////////////////////////////////////////////////////////////////////////////////////

struct DefaultRule;

aspect_rules! {
  derive_scope_r![Up, ast::ItemBuiltins => DefaultRule] {
    fn derive_scope(self, ctx, node) {
      ctx.scope_composite(Up, &node.elems)
    }
  }

  derive_scope_r![Up, ast::ItemGroup => DefaultRule] {
    fn derive_scope(self, ctx, node) {
      let ident     = &node.name.data;
      let own_scope = scope::Single::make(&ident.name, ctx.node_id());
      let cld_scope = ctx.scope_composite(Up, &node.items);
      scope::Pair::make(own_scope, cld_scope)
    }
  }

  derive_scope_r![Down, ast::ItemRule => DefaultRule] {
    fn derive_scope(self, ctx, node) {
      let parent_scope = ctx.parent_scope(Down); // TODO: take only statics
      let params_scope = ctx.scope_of(Up, node.params);
      scope::Pair::make(parent_scope, params_scope)
    }
  }

  derive_scope_r![Up, ast::ItemRule => DefaultRule] {
    fn derive_scope(self, ctx, node) {
      let ident = &node.name.data;
      scope::Single::make(&ident.name, ctx.node_id())
    }
  }

  derive_scope_r![Up, ast::ItemRuleDyn => DefaultRule] {
    fn derive_scope(self, ctx, node) {
      let ident = &node.name.data;
      scope::Single::make(&ident.name, ctx.node_id())
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

lang_part! {
  DeriveScopePart {
    derive_scope_r![Up   , ast::ItemBuiltins => DefaultRule],
    derive_scope_r![Up   , ast::ItemGroup    => DefaultRule],
    derive_scope_r![Down , ast::ItemRule     => DefaultRule],
    derive_scope_r![Up   , ast::ItemRule     => DefaultRule],
    derive_scope_r![Up   , ast::ItemRuleDyn  => DefaultRule],
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
