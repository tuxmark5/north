use {
  crate::ast,
  lang_mir::{
    codegen_ctx::{CodegenCtx, CodegenCtxExt},
    mir,
    reductions::TypeRed,
  },
  north_core::prelude::*,
  north_derive::aspect_rules,
  north_gen::prelude::*,
  north_typesys::type_of,
};

////////////////////////////////////////////////////////////////////////////////////////////////

struct DefaultRule;

aspect_rules! {
  reduce_r![TypeRed, ast::TypeBool => DefaultRule] {
    fn reduce(self, ctx, _node, _arg) -> NodeId<dyn mir::Type> {
      ctx.make_node_up(mir::TypeInt { bit_width: 8 })
    }
  }

  reduce_r![TypeRed, ast::TypeFn => DefaultRule] {
    fn reduce(self, ctx, node, arg) -> NodeId<dyn mir::Type> {
      ctx.make_node_up(mir::TypeFn {
        param_tys: ctx.reduce_link_n(&node.param_types, arg),
        result_ty: ctx.reduce_link(node.result_type, arg).into(),
      })
    }
  }

  reduce_r![TypeRed, ast::TypeInt => DefaultRule] {
    fn reduce(self, ctx, node, _arg) -> NodeId<dyn mir::Type> {
      ctx.make_node_up(mir::TypeInt { bit_width: node.size as u32 })
    }
  }

  reduce_r![TypeRed, ast::TypeUnit => DefaultRule] {
    fn reduce(self, ctx, _node, _arg) -> NodeId<dyn mir::Type> {
      ctx.make_node_up(mir::TypeUnit { })
    }
  }

  reduce_r![TypeRed, ast::TypeVar => DefaultRule] {
    fn reduce(self, ctx, node, arg) -> NodeId<dyn mir::Type> {
      let target_type = node.var.target_node.unwrap();
      ctx.reduce_link(target_type, arg)
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub fn type_of_reduced<R>(ctx: &ReduceCtxCore<R>, node_id: NodeId)
  -> NodeId<dyn mir::Type> where
  R: Reduction<InnerCtx = CodegenCtx>
{
  let src_type = type_of(node_id).unwrap();
  let mir_type = ctx.reduce_link_with(TypeRed, src_type, &mut ());
  mir_type
}

////////////////////////////////////////////////////////////////////////////////////////////////

lang_part! {
  ReduceTypePart {
    reduce_r![TypeRed, ast::TypeBool  => DefaultRule],
    reduce_r![TypeRed, ast::TypeFn    => DefaultRule],
    reduce_r![TypeRed, ast::TypeInt   => DefaultRule],
    reduce_r![TypeRed, ast::TypeUnit  => DefaultRule],
    reduce_r![TypeRed, ast::TypeVar   => DefaultRule],
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
