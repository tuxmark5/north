use {
  crate::{
    Reduction,
    reduce::{ReduceDyn},
  },
  north_core::NodeId,
  std::{
    marker::PhantomData,
    rc::Rc,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

// reduce_cld     reduce_now
// reduce_link    reduce_opt
// reduce_ref     lookup

pub trait Reducer {
  fn reduce_to<B>(&self, node_id: NodeId) -> B;

  fn reduce_with<R>(&self, node_id: NodeId) -> R::Output where
    R: Reduction;
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct NodeReducer {

}

impl Reducer for NodeReducer {
  fn reduce_to<B>(&self, node_id: NodeId) -> B {
    unimplemented!()
  }

  fn reduce_with<R>(&self, node_id: NodeId) -> R::Output where
    R: Reduction
  {
    unimplemented!()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////


/*

open questions:
1. how to reduce ValueRef (aka not nodes)
  > ports are needed for function arguments and projections
2.

codegen:
  types -> NodeId to NodeId<dyn mir::Type>
  exprs -> ValueRefOpt   ; basic blocks aren't generated explicitly by anything
  ast::Items -> NodeId<mir::Mod>
  ast::ItemFn ->
    1. NodeId<dyn Item> for Mod
    2. ValueRefOpt ?? for The rest

llvm:
  <ValueRed>
  mir::ItemFn -> LLVMValueRef
  mir::Stmt* -> LLVMValueRef
  mir::Const* -> LLVMValueRef

  <TypeRed>
  mir::Type -> LLVMTypeRef

  <BasicBlockRed>
  mir::Block -> LLVMBasicBlockRef

  <ModuleRed>
  mir::Mod -> LLVMModuleRef


  ExprDeref, ExprVar, ExprMember <- LValues

  ((a.0).1).2 = b;



*/
