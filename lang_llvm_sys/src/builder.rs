use {
  crate::{
    context::Context,
  },
  llvm_sys::{
    LLVMIntPredicate, LLVMOpcode,
    core as llvm_core,
    prelude::*,
  },
  north_core::quote::{QuoteType, RustFn},
  serde::Serialize,
  std::{
    ffi::CString,
    ptr,
    rc::Rc,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Builder {
  context: Rc<Context>,
  raw: LLVMBuilderRef,
}

impl Builder {
  pub fn new(context: Rc<Context>) -> Self {
    let llvm_ctx = context.raw();
    let raw = unsafe { llvm_core::LLVMCreateBuilderInContext(llvm_ctx) };
    Self { context, raw }
  }

  pub fn build_add(&self, lhs: LLVMValueRef, rhs: LLVMValueRef) -> LLVMValueRef {
    let name = "\0".as_ptr() as *const i8;
    unsafe { llvm_core::LLVMBuildAdd(self.raw, lhs, rhs, name) }
  }

  pub fn build_alloca(&self, ty: LLVMTypeRef, _name: &str) -> LLVMValueRef {
    let name = "\0".as_ptr() as *const i8;
    unsafe { llvm_core::LLVMBuildAlloca(self.raw, ty, name) }
  }

  pub fn build_bin_op(&self, op: LLVMOpcode, lhs: LLVMValueRef, rhs: LLVMValueRef)
    -> LLVMValueRef
  {
    let name = "\0".as_ptr() as *const i8;
    unsafe { llvm_core::LLVMBuildBinOp(self.raw, op, lhs, rhs, name) }
  }

  pub fn build_bit_cast(&self, val: LLVMValueRef, dest_ty: LLVMTypeRef)
    -> LLVMValueRef
  {
    let name = "\0".as_ptr() as *const i8;
    unsafe { llvm_core::LLVMBuildBitCast(self.raw, val, dest_ty, name) }
  }

  pub fn build_br(&self, dest: LLVMBasicBlockRef) -> LLVMValueRef {
    unsafe { llvm_core::LLVMBuildBr(self.raw, dest) }
  }

  pub fn build_call(&self, target: LLVMValueRef, args: &mut [LLVMValueRef])
    -> LLVMValueRef
  {
    let args_ptr = args.as_mut_ptr();
    let num_args = args.len() as u32;
    let name = CString::new("").unwrap();
    unsafe {
      llvm_core::LLVMBuildCall(self.raw, target, args_ptr, num_args, name.as_ptr())
    }
  }

  pub fn build_call_native(&self, target: &RustFn, args: &mut [LLVMValueRef])
    -> LLVMValueRef
  {
    let target = self.quote_fn(target);
    self.build_call(target, args)
  }

  pub fn build_cond_br(
    &self,
    cond    : LLVMValueRef,
    then_b  : LLVMBasicBlockRef,
    else_b  : LLVMBasicBlockRef,
  ) -> LLVMValueRef {
    unsafe { llvm_core::LLVMBuildCondBr(self.raw, cond, then_b, else_b) }
  }

  pub fn build_extract_value(
    &self,
    mut agg : LLVMValueRef,
    indices : &[usize],
  ) -> LLVMValueRef {
    let name = "\0".as_ptr() as *const i8;
    for index in indices {
      let index = *index as u32;
      agg = unsafe { llvm_core::LLVMBuildExtractValue(self.raw, agg, index, name) };
    }
    agg
  }

  pub fn build_icmp(
    &self,
    pred    : LLVMIntPredicate,
    lhs     : LLVMValueRef,
    rhs     : LLVMValueRef
  ) -> LLVMValueRef {
    let name = "\0".as_ptr() as *const i8;
    unsafe { llvm_core::LLVMBuildICmp(self.raw, pred, lhs, rhs, name) }
  }

  pub fn build_in_bounds_gep(
    &self,
    pointer : LLVMValueRef,
    indices : &mut [LLVMValueRef],
  ) -> LLVMValueRef {
    let num_indices = indices.len() as u32;
    let indices = indices.as_mut_ptr();
    let name = "\0".as_ptr() as *const i8;
    unsafe {
      llvm_core::LLVMBuildInBoundsGEP(self.raw, pointer, indices, num_indices, name)
    }
  }

  pub fn build_insert_value(
    &self,
    agg   : LLVMValueRef,
    elem  : LLVMValueRef,
    index : usize
  ) -> LLVMValueRef {
    let name = CString::new("").unwrap();
    unsafe {
      llvm_core::LLVMBuildInsertValue(self.raw, agg, elem, index as u32, name.as_ptr())
    }
  }

  pub fn build_load(&self, ptr: LLVMValueRef) -> LLVMValueRef {
    let name = "\0".as_ptr() as *const i8;
    unsafe { llvm_core::LLVMBuildLoad(self.raw, ptr, name) }
  }

  pub fn build_ret(&self, value: LLVMValueRef) -> LLVMValueRef {
    unsafe { llvm_core::LLVMBuildRet(self.raw, value) }
  }

  pub fn build_ret_void(&self) -> LLVMValueRef {
    unsafe { llvm_core::LLVMBuildRetVoid(self.raw) }
  }

  pub fn build_store(&self, value: LLVMValueRef, ptr: LLVMValueRef) -> LLVMValueRef {
    unsafe { llvm_core::LLVMBuildStore(self.raw, value, ptr) }
  }

  pub fn build_struct(&self, ty: LLVMTypeRef, values: &[LLVMValueRef]) -> LLVMValueRef {
    let mut agg = self.undef(ty);

    for (i, value) in values.iter().enumerate() {
      agg = self.build_insert_value(agg, *value, i);
    }

    agg
  }

  pub fn build_struct_gep(&self, ptr: LLVMValueRef, index: usize) -> LLVMValueRef {
    unsafe {
      let name = "\0".as_ptr() as *const i8;
      llvm_core::LLVMBuildStructGEP(self.raw, ptr, index as u32, name)
    }
  }

  pub fn build_switch(
    &self,
    value   : LLVMValueRef,
    default : LLVMBasicBlockRef,
    cases   : &[(LLVMValueRef, LLVMBasicBlockRef)]
  ) -> LLVMValueRef where {
    let num_cases = cases.len() as u32;
    unsafe {
      let switch = llvm_core::LLVMBuildSwitch(self.raw, value, default, num_cases);
      for (val, block) in cases { llvm_core::LLVMAddCase(switch, *val, *block); }
      switch
    }
  }

  pub fn const_int(&self, ty: LLVMTypeRef, value: i64) -> LLVMValueRef {
    unsafe { llvm_core::LLVMConstInt(ty, value as u64, 0) }
  }

  pub fn const_struct(&self, values: &mut [LLVMValueRef]) -> LLVMValueRef {
    unsafe {
      llvm_core::LLVMConstStructInContext(
        self.context.raw(),
        values.as_mut_ptr(),
        values.len() as u32,
        0
      )
    }
  }

  pub fn context(&self) -> &Context {
    &*self.context
  }

  // LLVMConstStructInContext

  pub fn current_block(&self) -> Option<LLVMBasicBlockRef> {
    let block = unsafe { llvm_core::LLVMGetInsertBlock(self.raw) };
    if block != ptr::null_mut() {
      Some(block)
    } else {
      None
    }
  }

  pub fn is_valid(&self) -> bool {
    unsafe { llvm_core::LLVMGetInsertBlock(self.raw) != ptr::null_mut() }
  }

  pub fn position_at_end(&self, block: LLVMBasicBlockRef) {
    unsafe { llvm_core::LLVMPositionBuilderAtEnd(self.raw, block) }
  }

  pub fn quote_const<T: Serialize>(&self, value: &T) -> LLVMValueRef {
    crate::serialize(&*self.context, value).unwrap()
  }

  pub fn quote_fn(&self, target: &RustFn) -> LLVMValueRef {
    let fn_ty = target.fn_type;
    let fn_ty = self.context.quote_type_id(fn_ty);
    let fn_ty = self.context.type_pointer(fn_ty);
    let ptr = target.raw_ptr as usize;
    let ptr = self.quote_const(&ptr);
    self.build_bit_cast(ptr, fn_ty)
  }

  pub fn quote_ptr<T: QuoteType>(&self, ptr: *const T) -> LLVMValueRef {
    self.context.quote_ptr::<T>(ptr)
  }

  pub fn quote_ty<T: QuoteType>(&self) -> LLVMTypeRef {
    self.context.quote_type::<T>()
  }

  pub fn reset(&self) {
    unsafe { llvm_core::LLVMPositionBuilderAtEnd(self.raw, ptr::null_mut()) }
  }

  pub fn undef(&self, ty: LLVMTypeRef) -> LLVMValueRef {
    unsafe { llvm_core::LLVMGetUndef(ty) }
  }

  pub fn with_block<F>(&self, f: F) where
    F: FnOnce(LLVMBasicBlockRef) -> LLVMBasicBlockRef
  {
    let curr_block = self.current_block().unwrap();
    let new_block = f(curr_block);
    self.position_at_end(new_block);
  }
}

impl Drop for Builder {
  fn drop(&mut self) {
    unsafe { llvm_core::LLVMDisposeBuilder(self.raw); }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
