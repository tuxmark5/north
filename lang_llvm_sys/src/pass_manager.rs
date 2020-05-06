use {
  llvm_sys::{
    core,
    prelude::*,
    transforms
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct PassManager {
  raw: LLVMPassManagerRef,
}

impl PassManager {
  pub fn new() -> Self {
    let raw = unsafe { core::LLVMCreatePassManager() };
    Self { raw }
  }

  pub fn add_scalar_gvn_pass(&self) {
    unsafe {
      //transforms::scalar::LLVMAddGVNPass(self.raw);
      // transforms::scalar::LLVMAddVerifierPass(self.raw);
      transforms::scalar::LLVMAddNewGVNPass(self.raw);
      //transforms::scalar::LLVMAddPromoteMemoryToRegisterPass(self.raw);
    }
  }

  pub fn run(&self, module: LLVMModuleRef) {
    unsafe {
      core::LLVMRunPassManager(self.raw,  module);
    }
  }
}

impl Drop for PassManager {
  fn drop(&mut self) {
    unsafe { core::LLVMDisposePassManager(self.raw); }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
