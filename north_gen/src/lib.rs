#![feature(box_syntax)]
#![feature(specialization)]

////////////////////////////////////////////////////////////////////////////////////////////////

extern crate north_derive;

////////////////////////////////////////////////////////////////////////////////////////////////

pub mod gen_aspect;
pub mod reduce;

pub use self::{
  gen_aspect::GenAspect,
  reduce::{Reduce, ReduceCtx, ReduceCtxCore, ReduceDyn},
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub mod prelude {
  pub use {
    crate::{
      Reduction, reduce_r,
      gen_aspect::GenAspect,
      reduce::{Reduce, ReduceCtx, ReduceCtxCore},
    },
    north_core::{
      compiler::{AspectPart, aspect::aspect_cast},
      context::{
        ModelCellCtxExt,
        ModelNodeIdCtxExt,
        NodeIdCtxExt,
      },
      prelude::*,
    },
    north_derive::{
      aspect_rules,
    },
  };
}

////////////////////////////////////////////////////////////////////////////////////////////////

use {
  north_core::{
    NodeId,
    compiler::COMPILER
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait Reduction: 'static + Copy + Default + Sized {
  const CACHE_RESULT: bool = true;
  type Arg;
  type InnerCtx: Clone;
  type Output: Clone;
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub fn reduce_node<R>(
  node_id : NodeId,
  inner   : R::InnerCtx,
  arg     : &mut R::Arg
) -> R::Output where
  R: Reduction
{
  COMPILER.with(|comp| {
    let aspect = comp.aspect_mut::<GenAspect>();
    let core = aspect.core_ctx(node_id, inner);
    aspect.reduce_cld::<R>(core, arg)
  })
}

////////////////////////////////////////////////////////////////////////////////////////////////
