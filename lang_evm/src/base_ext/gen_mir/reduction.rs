use {
  crate::{
    base_ext::gen_mir::reduce_call::CallInfo,
    mir_ext::mir,
  },
  lang_mir::{CodegenCtx, Cursor},
  north_gen::Reduction,
};

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Copy, Default)]
pub struct CallRed;

impl Reduction for CallRed {
  const CACHE_RESULT: bool = false;
  type Arg = Cursor;
  type InnerCtx = CodegenCtx;
  type Output = Option<CallInfo>;
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone)]
pub struct CallRedCtx {
  pub dom_group: u32,
  pub prefer_shift: bool,
}

scoped_thread_local! {
  pub static CALL_RED_PARAMS: CallRedCtx
}

pub fn call_red_params() -> CallRedCtx {
  CALL_RED_PARAMS.with(|x| x.clone())
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Copy, Default)]
pub struct GreedyRed;

impl Reduction for GreedyRed {
  type Arg = Cursor;
  type InnerCtx = CodegenCtx;
  type Output = mir::RValLinkOpt;
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Copy, Default)]
pub struct LazyRed;

impl Reduction for LazyRed {
  type Arg = Cursor;
  type InnerCtx = CodegenCtx;
  type Output = mir::RValLinkOpt;
}

////////////////////////////////////////////////////////////////////////////////////////////////
