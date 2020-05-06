use {
  crate::{
    codegen_ctx::CodegenCtx,
    cursor::Cursor,
    mir::{self},
  },
  north_core::NodeId,
  north_gen::{Reduction}
};

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Copy, Default)]
pub struct LValRed;

impl Reduction for LValRed {
  type Arg = Cursor;
  type InnerCtx = CodegenCtx;
  type Output = mir::LValLink;
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Copy, Default)]
pub struct ModRed;

impl Reduction for ModRed {
  type Arg = ();
  type InnerCtx = CodegenCtx;
  type Output = NodeId<mir::Mod>;
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Copy, Default)]
pub struct TypeRed;

impl Reduction for TypeRed {
  type Arg = ();
  type InnerCtx = CodegenCtx;
  type Output = NodeId<dyn mir::Type>;
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Copy, Default)]
pub struct ValueRed;

impl Reduction for ValueRed {
  type Arg = Cursor;
  type InnerCtx = CodegenCtx;
  type Output = mir::RValLinkOpt;
}

////////////////////////////////////////////////////////////////////////////////////////////////
