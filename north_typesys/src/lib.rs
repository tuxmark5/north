#![feature(arbitrary_self_types)]
#![feature(box_syntax)]
#![feature(generators)]
#![feature(generator_trait)]
#![feature(specialization)]
#![feature(type_ascription)]
#![feature(untagged_unions)]

////////////////////////////////////////////////////////////////////////////////////////////////

#[macro_use]
extern crate north_derive;

////////////////////////////////////////////////////////////////////////////////////////////////

pub mod deref_types_pass;
pub mod infer_types;
pub mod infer_types_pass;
pub mod type_sys_aspect;
pub mod unify_types;

////////////////////////////////////////////////////////////////////////////////////////////////

pub use self::{
  deref_types_pass::DerefTypesPass,
  infer_types::{InferTypes, InferTypesCtx, InferTypesCtxCore, InferTypesFuture},
  infer_types_pass::InferTypesPass,
  type_sys_aspect::{TypeRef, TypeSysAspect},
  unify_types::{UnifyTypes, UnifyTypesCtx},
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub mod prelude {
  pub use {
    crate::{
      infer_types::{InferTypes, InferTypesCtx, InferTypesCtxCore, InferTypesFuture},
      infer_types_r,
      type_sys_aspect::{TypeSysAspect},
      unify_types::{UnifyTypes, UnifyTypesCtx},
      unify_types_part, unify_types_r,
    },
    north_core::{
      compiler::{AspectPart, aspect::aspect_cast},
      context::{ModelCellCtxExt, ModelNodeIdCtxExt, NodeIdCtxExt},
      futures::prelude::*,
      prelude::*,
    },
  };
}

////////////////////////////////////////////////////////////////////////////////////////////////

use {
  north_core::{
    NodeId,
    compiler::COMPILER,
  }
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub fn type_of(node_id: NodeId) -> Option<NodeId> {
  COMPILER.with(|comp| {
    let aspect = comp.aspect::<TypeSysAspect>();
    aspect.type_of_concrete(node_id)
  })
}

////////////////////////////////////////////////////////////////////////////////////////////////
