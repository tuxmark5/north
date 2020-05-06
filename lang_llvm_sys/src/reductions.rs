use {
  crate::{
    builder::Builder,
    context::ContextInner,
  },
  llvm_sys::{
    prelude::*,
  },
  north_gen::{
    Reduction
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Copy, Default)]
pub struct BasicBlockRed;

impl Reduction for BasicBlockRed {
  type Arg = Builder;
  type InnerCtx = ContextInner;
  type Output = LLVMBasicBlockRef;
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Copy, Default)]
pub struct ModRed;

impl Reduction for ModRed {
  type Arg = ();
  type InnerCtx = ContextInner;
  type Output = LLVMModuleRef;
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Copy, Default)]
pub struct TypeRed;

impl Reduction for TypeRed {
  type Arg = ();
  type InnerCtx = ContextInner;
  type Output = LLVMTypeRef;
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Copy, Default)]
pub struct ValueRed;

impl Reduction for ValueRed {
  type Arg = Builder;
  type InnerCtx = ContextInner;
  type Output = LLVMValueRef;
}

////////////////////////////////////////////////////////////////////////////////////////////////
