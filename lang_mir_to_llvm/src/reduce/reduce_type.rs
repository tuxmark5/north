use {
  lang_llvm_sys::{
    ContextExt, TypeRed
  },
  lang_mir::mir,
  llvm_sys::{
    prelude::*,
  },
  north_gen::prelude::*,
};

////////////////////////////////////////////////////////////////////////////////////////////////

struct DefaultRule;

aspect_rules! {
  reduce_r![TypeRed, mir::TypeArray => DefaultRule] {
    fn reduce(self, ctx, node, _arg) -> LLVMTypeRef {
      let element_type = ctx.reduce_link(&node.value_ty, _arg);
      ctx.llvm_ctx().type_array(element_type, node.elem_count)
    }
  }

  reduce_r![TypeRed, mir::TypeInt => DefaultRule] {
    fn reduce(self, ctx, node, _arg) -> LLVMTypeRef {
      ctx.llvm_ctx().type_int(node.bit_width)
    }
  }

  reduce_r![TypeRed, mir::TypeFn => DefaultRule] {
    fn reduce(self, ctx, node, _arg) -> LLVMTypeRef {
      let return_type = ctx.reduce_link(&node.result_ty, _arg);
      let mut param_types = ctx.reduce_link_n(&node.param_tys, _arg);
      ctx.llvm_ctx().type_function(return_type, param_types.as_mut())
    }
  }

  reduce_r![TypeRed, mir::TypePtr => DefaultRule] {
    fn reduce(self, ctx, node, _arg) -> LLVMTypeRef {
      let element_type = ctx.reduce_link(&node.target_ty, _arg);
      ctx.llvm_ctx().type_pointer(element_type)
    }
  }

  reduce_r![TypeRed, mir::TypeStruct => DefaultRule] {
    fn reduce(self, ctx, node, _arg) -> LLVMTypeRef {
      let mut element_types = ctx.reduce_link_n(&node.elements, _arg);
      ctx.llvm_ctx().type_struct(element_types.as_mut())
    }
  }

  reduce_r![TypeRed, mir::TypeUnit => DefaultRule] {
    fn reduce(self, ctx, _node, _arg) -> LLVMTypeRef {
      ctx.llvm_ctx().type_void()
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

lang_part! {
  ReduceTypePart {
    reduce_r![TypeRed, mir::TypeArray   => DefaultRule],
    reduce_r![TypeRed, mir::TypeInt     => DefaultRule],
    reduce_r![TypeRed, mir::TypeFn      => DefaultRule],
    reduce_r![TypeRed, mir::TypePtr     => DefaultRule],
    reduce_r![TypeRed, mir::TypeStruct  => DefaultRule],
    reduce_r![TypeRed, mir::TypeUnit    => DefaultRule],
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
