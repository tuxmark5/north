pub mod reduce_call;
pub mod reduce_expr;
pub mod reduce_gexpr;
pub mod reduce_item;
pub mod reduce_type;
pub mod reduction;

////////////////////////////////////////////////////////////////////////////////////////////////

use {
  crate::{
    ast::{self},
    mir::{BlockId, BlockRef},
  },
  north_core::{
    cell::prelude::*,
    context::ModelNodeIdCtxExt,
  },
  north_derive::cell,
  north_gen::{ReduceCtxCore, Reduction},
};

////////////////////////////////////////////////////////////////////////////////////////////////

scoped_thread_local!(pub static FAIL_BLOCK_ID: FailBlock);

// #[cell(scope(compiler, node))]
// pub static FAIL_BLOCK_ID: FailBlock = ();

#[derive(Clone)]
pub enum FailBlock {
  Partial(BlockId),
  Total(BlockId),
}

impl FailBlock {
  pub fn get<R: Reduction>(_ctx: &ReduceCtxCore<R>) -> Self {
    FAIL_BLOCK_ID.with(|b| b.clone())

    // ctx.ancestor_ids()
    //   .find_map(|n| cell_try_get!(FAIL_BLOCK_ID(n)))
    //   .expect("no failure block found")
  }

  pub fn get_any<R: Reduction>(ctx: &ReduceCtxCore<R>) -> BlockRef {
    match Self::get(ctx) {
      FailBlock::Partial(id) => id.into(),
      FailBlock::Total(id) => id.into(),
    }
  }

  pub fn get_partial<R: Reduction>(ctx: &ReduceCtxCore<R>) -> Option<BlockRef> {
    match Self::get(ctx) {
      FailBlock::Partial(id) => Some(id.into()),
      FailBlock::Total(_id) => None,
    }
  }

  pub fn set<F: FnOnce() -> R, R>(block: Self, scope: F) -> R {
    FAIL_BLOCK_ID.set(&block, scope)
  }

  pub fn to_ref(&self) -> BlockRef {
    match self {
      FailBlock::Partial(id) => (*id).into(),
      FailBlock::Total(id) => (*id).into(),
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[cell(scope(compiler, node))]
pub static ITER_DEPTH: usize = ();

pub fn get_iter_depth<R: Reduction>(ctx: &ReduceCtxCore<R>) -> usize {
  ctx.ancestor_ids()
    .find_map(|n| cell_try_get!(ITER_DEPTH(n)))
    .unwrap_or(0)
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[cell(scope(compiler, node))]
pub static PERSISTENT_BLOCK_ID: usize = ();

////////////////////////////////////////////////////////////////////////////////////////////////
