use {
  crate::ast,
  north_core::{
    context::{ModelNodeIdCtxExt, NodeIdCtxExt},
    prelude::*,
    structure::ForestExt,
  },
  north_scope::{
    DeriveScope, 
    DeriveScopeCtx as Ctx, 
    ScopeAspect,
    ScopeKind,
    prelude::*,
    scope::{self, ScopePtrOpt},
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Copy)]
pub struct Down;
impl ScopeKind for Down { }

#[derive(Clone, Copy)]
pub struct Up;
impl ScopeKind for Up { }

////////////////////////////////////////////////////////////////////////////////////////////////

struct DefaultRule;

////////////////////////////////////////////////////////////////////////////////////////////////

impl<N: Node + ?Sized> DeriveScope<N, Down> for DefaultRule {
  default const RELATIVE: bool = false;
  
  default fn derive_scope(&self, (mut ctx, _): Ctx<N>) -> ScopePtrOpt {
    ctx.parent_scope(Down)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl<N: Node + ?Sized> DeriveScope<N, Up> for DefaultRule {
  default fn derive_scope(&self, (_, _): Ctx<N>) -> ScopePtrOpt {
    None
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl DeriveScope<ast::DeclLet, Up> for DefaultRule {
  fn derive_scope(&self, (ctx, node): Ctx<ast::DeclLet>) -> ScopePtrOpt {
    let ident = &node.var.data;
    scope::Single::make(&ident.name, ctx.node_id())
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl DeriveScope<ast::DeclRuleParam, Up> for DefaultRule {
  fn derive_scope(&self, (ctx, node): Ctx<ast::DeclRuleParam>) -> ScopePtrOpt {
    let ident = &node.var.data;
    scope::Single::make(&ident.name, ctx.node_id())
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl DeriveScope<ast::DeclRuleParams, Up> for DefaultRule {
  fn derive_scope(&self, (mut ctx, node): Ctx<ast::DeclRuleParams>) -> ScopePtrOpt {
    ctx.scope_composite(Up, &node.params)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl DeriveScope<ast::ExprBlock, Down> for DefaultRule {
  const RELATIVE: bool = true;

  fn derive_scope(&self, (mut ctx, _node): Ctx<ast::ExprBlock>) -> ScopePtrOpt {
    // unordered static + ordered instance
    // composite: ((parent.down + own) + sibling[0].up) + sibling[1].up
    //               STA+IST      STA      INST              INST

    // TODO: unordered items + ordered locals

    let ref_node = ctx.ref_node_id;
    let new_ref_node = ctx.model().prev_sibling_id(ref_node);

    let scope_a = new_ref_node
      .and_then(|target| ctx.self_scope(Down, target))
      .or_else(|| ctx.parent_scope(Down));
    let scope_b = ctx.scope_of(Up, ref_node);
    scope::Pair::make(scope_a, scope_b)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl DeriveScope<ast::Items, Down> for DefaultRule {
  fn derive_scope(&self, (mut ctx, node): Ctx<ast::Items>) -> ScopePtrOpt {
    ctx.scope_composite(Up, &node.items)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl DeriveScope<ast::ItemBuiltins, Up> for DefaultRule {
  fn derive_scope(&self, (mut ctx, node): Ctx<ast::ItemBuiltins>) -> ScopePtrOpt {
    ctx.scope_composite(Up, &node.nodes)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl DeriveScope<ast::ItemFn, Down> for DefaultRule {
  fn derive_scope(&self, (mut ctx, node): Ctx<ast::ItemFn>) -> ScopePtrOpt {
    let parent_scope = ctx.parent_scope(Down); // TODO: take only statics
    let params_scope = ctx.scope_of(Up, node.params);
    scope::Pair::make(parent_scope, params_scope)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl DeriveScope<ast::ItemFn, Up> for DefaultRule {
  fn derive_scope(&self, (ctx, node): Ctx<ast::ItemFn>) -> ScopePtrOpt {
    let ident = &node.name.data;
    scope::Single::make(&ident.name, ctx.node_id())
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl DeriveScope<ast::ItemType, Up> for DefaultRule {
  fn derive_scope(&self, (_ctx, node): Ctx<ast::ItemType>) -> ScopePtrOpt {
    scope::Single::make(node.name.as_ref(), node.body.cast())
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl DeriveScope<ast::StmtDecl, Up> for DefaultRule {
  fn derive_scope(&self, (mut ctx, node): Ctx<ast::StmtDecl>) -> ScopePtrOpt {
    ctx.scope_of(Up, node.decl)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

// LocalUp, GlobalUp

lang_part! {
  DeriveScopePart {
    derive_scope_r![Down, _                   => DefaultRule],
    derive_scope_r![Down, ast::ExprBlock      => DefaultRule],
    derive_scope_r![Down, ast::Items          => DefaultRule],
    derive_scope_r![Down, ast::ItemFn         => DefaultRule],

    derive_scope_r![Up, _                     => DefaultRule],
    derive_scope_r![Up, ast::DeclLet          => DefaultRule],
    derive_scope_r![Up, ast::DeclRuleParam    => DefaultRule],
    derive_scope_r![Up, ast::DeclRuleParams   => DefaultRule],
    derive_scope_r![Up, ast::ItemBuiltins     => DefaultRule],
    derive_scope_r![Up, ast::ItemFn           => DefaultRule],
    derive_scope_r![Up, ast::ItemType         => DefaultRule],
    derive_scope_r![Up, ast::StmtDecl         => DefaultRule],
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
