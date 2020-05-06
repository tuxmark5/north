#![feature(box_syntax)]
#![feature(specialization)]

////////////////////////////////////////////////////////////////////////////////////////////////

pub use ::llvm_sys;

pub mod builder;
pub mod context;
pub mod dsl;
pub mod jit_engine;
pub mod pass_manager;
pub mod reductions;
pub mod serializer;
pub mod types;

pub use self::{
  builder::Builder,
  context::{Context, ContextExt, ContextInner},
  jit_engine::JITEngine,
  reductions::*,
  serializer::{Native, serialize},
};

////////////////////////////////////////////////////////////////////////////////////////////////

use {
  ::llvm_sys::{
    analysis::{self, LLVMVerifierFailureAction},
    core,
    prelude::*,
  },
  std::{
    ptr,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub fn verify_module(mod_ref: LLVMModuleRef) {
  let mut message = ptr::null_mut();
  let action = LLVMVerifierFailureAction::LLVMReturnStatusAction;
  unsafe {
    analysis::LLVMVerifyModule(mod_ref, action, &mut message);
    core::LLVMDisposeMessage(message);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
