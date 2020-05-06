#![feature(arbitrary_self_types)]
#![feature(specialization)]

////////////////////////////////////////////////////////////////////////////////////////////////

extern crate north_derive;

////////////////////////////////////////////////////////////////////////////////////////////////

pub mod derive_scope;
pub mod resolve;
pub mod resolve_pass;
pub mod scope;
pub mod scope_aspect;

pub use self::{
  derive_scope::{DeriveScope, DeriveScopeCtx},
  resolve::{Resolve, ResolveCtx, ResolveFuture},
  resolve_pass::ResolvePass,
  scope::Scope,
  scope_aspect::ScopeAspect,
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub mod prelude {
  pub use {
    crate::{
      derive_scope_r, resolve_r,
      derive_scope::{DeriveScope, DeriveScopeCtx},
      resolve::{Resolve, ResolveCtx, ResolveFuture},
      scope::{self, ScopePtrOpt},
      scope_aspect::ScopeAspect,
    },
    north_core::{
      compiler::{AspectPart, aspect::aspect_cast},
      context::NodeIdCtxExt,
      futures::prelude::*,
      prelude::*,
    },
  };
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait ScopeKind: 'static + Copy { }

////////////////////////////////////////////////////////////////////////////////////////////////

/*
  scopes:
  * how to deal with multiple scopes in one construct:
    types vs vars vs macros vs etc...
  * overhead reduction:
    * is it meaningless?
    > largest scopes are glob-imported or single imported
    > 2nd largest is global scope
    > local scopes are small by comparison
    > both 1 and 2 scopes are unordrered, as such

    > scope caching should be controlled by scope generator
    > that way scope generator can produce the key only with relevant data
    > largest scopes will act be adaptors over node children
    > RefCell

    categories:
      Static - one instance total. item decls, consts and statics are this way
      Instance - local lets, generic params and regular params

      Type - types
      Var - variable

*/
////////////////////////////////////////////////////////////////////////////////////////////////
