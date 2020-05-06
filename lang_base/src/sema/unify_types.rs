use {
  crate::ast,
  north_core::{
    compiler::{aspect::AspectPart},
  },
  north_typesys::{
    TypeSysAspect, UnifyTypes, UnifyTypesCtx,
    unify_types_part,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

struct UnifyTypesRule;

////////////////////////////////////////////////////////////////////////////////////////////////

impl UnifyTypes<ast::TypeBool, ast::TypeBool> for UnifyTypesRule {
  fn unify_types(&self, _ctx: UnifyTypesCtx, _a: &ast::TypeBool, _b: &ast::TypeBool) { }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl UnifyTypes<ast::TypeFn, ast::TypeFn> for UnifyTypesRule {
  fn unify_types(&self, ctx: UnifyTypesCtx, a: &ast::TypeFn, b: &ast::TypeFn) {
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

////////////////////////////////////////////////////////////////////////////////////////////////

impl UnifyTypes<ast::TypeInt, ast::TypeInt> for UnifyTypesRule {
  fn unify_types(&self, _ctx: UnifyTypesCtx, a: &ast::TypeInt, b: &ast::TypeInt) {
    if a.signed != b.signed || a.size != b.size {
      panic!("int mismatch {} {}; {} {}", a.signed, b.signed, a.size, b.size);
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl UnifyTypes<ast::TypeUnit, ast::TypeUnit> for UnifyTypesRule {
  fn unify_types(&self, _ctx: UnifyTypesCtx, _a: &ast::TypeUnit, _b: &ast::TypeUnit) { }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl UnifyTypes<ast::TypeVar, ast::TypeVar> for UnifyTypesRule {
  fn unify_types(&self, ctx: UnifyTypesCtx, a: &ast::TypeVar, b: &ast::TypeVar) {
    match (a.var.target_node, b.var.target_node) {
      (Some(target_a), Some(target_b)) => ctx.unify(target_a, target_b),
      _ => panic!("invalid reference"),
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

unify_types_part! {
  UnifyTypesPart {
    (ast::TypeBool,  ast::TypeBool  ) => UnifyTypesRule,
    (ast::TypeFn,    ast::TypeFn    ) => UnifyTypesRule,
    (ast::TypeInt,   ast::TypeInt   ) => UnifyTypesRule,
    (ast::TypeUnit,  ast::TypeUnit  ) => UnifyTypesRule,
    (ast::TypeVar,   ast::TypeVar   ) => UnifyTypesRule
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
