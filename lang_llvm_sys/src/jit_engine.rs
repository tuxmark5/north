use {
  crate::{
    pass_manager::PassManager,
  },
  libc::{c_char, c_void},
  llvm_sys::{
    core,
    error::{
      LLVMErrorRef,
      LLVMErrorSuccess,
    },
    orc::{
      self,
      LLVMOrcJITStackRef
    },
    prelude::*,
    target,
    target_machine::{
      self as tm,
      LLVMCodeGenOptLevel,
      LLVMCodeModel,
      LLVMRelocMode,
      LLVMTargetMachineRef,
    },
  },
  std::{
    ffi::{CString},
    ptr,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct JITEngine {
  target_machine: LLVMTargetMachineRef,
  jit_stack: LLVMOrcJITStackRef,
  pass_manager: PassManager,
}

type ResolveFn = extern fn(*const c_char, *mut c_void) -> u64;

extern "C" fn resolve2(_name: *const c_char, _ctx: *mut c_void) -> u64 {
  panic!("oh noes");
}

impl JITEngine {
  pub fn new() -> Self {
    let target_machine = unsafe { Self::new_target_machine().unwrap() };
    let jit_stack = unsafe { orc::LLVMOrcCreateInstance(target_machine) };
    let pass_manager = PassManager::new();

    pass_manager.add_scalar_gvn_pass();

    Self { target_machine, jit_stack, pass_manager }
  }

  // LLVMOrcSetIndirectStubPointer(JITStack, "foo", MainAddr);
  pub unsafe fn add_module(&self, module: LLVMModuleRef) -> Result<(), ()> {
    //core::LLVMDumpModule(module);

    self.pass_manager.run(module);
    // core::LLVMDumpModule(module);

    /*let triple = tm::LLVMGetTargetMachineTriple(self.target_machine);
    core::LLVMSetTarget(module, triple);

    let layout = tm::LLVMCreateTargetDataLayout(self.target_machine);
    target::LLVMSetModuleDataLayout(module, layout);*/

    //let object_file = LLVMCreateObjectFile(mem_buf);

    //let shared_module = mem::transmute(module);

    /*let mut func_name = ptr::null_mut();
    let name = CString::new("some_func").unwrap();
    orc::LLVMOrcGetMangledSymbol(self.jit_stack, &mut func_name, name.as_ptr());*/

    let symbol_resolver = Some(resolve2 as ResolveFn);
    let symbol_resolver_ctx = ptr::null_mut();

    let mut ret_handle = 0;

    let error = orc::LLVMOrcAddEagerlyCompiledIR(
      self.jit_stack, 
      &mut ret_handle, 
      module, 
      symbol_resolver, 
      symbol_resolver_ctx
    );

    /*let mut ret_handle = 0;
    let error = orc::LLVMOrcAddObjectFile(
      self.jit_stack, &mut ret_handle, object_file, symbol_resolver, symbol_resolver_ctx
    );*/

    Self::make_result((), error)
  }

  pub fn get_symbol_address(&self, name: &str) -> Result<u64, ()> {
    let name = CString::new(name).unwrap();
    let mut ret_addr = 0;
    let error = unsafe {
      orc::LLVMOrcGetSymbolAddress(self.jit_stack, &mut ret_addr, name.as_ptr())
    };

    if ret_addr == 0 {
      Err(())
    } else {
      Self::make_result(ret_addr, error)
    }
  }

  pub fn make_result<T>(value: T, error: LLVMErrorRef) -> Result<T, ()> {
    if error.is_null() {
      Ok(value)
    } else {
      Err(())
    }
  }

  /*pub fn make_result_2<F, R>(func: F) -> Result<R, ()> where
    F: FnOnce() -> (R, LLVMOrcErrorCode)
  {
    let (value, error) = func();
    use self::LLVMOrcErrorCode::*;
    match error {
      LLVMOrcErrSuccess => Ok(value),
      LLVMOrcErrGeneric => Err(())
    }
  }*/

  pub unsafe fn new_target_machine() -> Result<LLVMTargetMachineRef, ()> {
    let result = target::LLVM_InitializeNativeTarget();
    if result != 0 { panic!("can't init native target: {:?}", result); }

    let result = target::LLVM_InitializeNativeAsmParser();
    if result != 0 { panic!("can't init native asm parser: {:?}", result); }

    let result = target::LLVM_InitializeNativeAsmPrinter();
    if result != 0 { panic!("can't init native asm printer: {:?}", result); }

    let triple = tm::LLVMGetDefaultTargetTriple();
    let mut target_ref = ptr::null_mut();
    tm::LLVMGetTargetFromTriple(triple, &mut target_ref, ptr::null_mut());
    if target_ref.is_null() { panic!("no target"); }

    let has_jit = tm::LLVMTargetHasJIT(target_ref);
    if has_jit == 0 { panic!("no jit"); }

    let cpu = CString::new("").unwrap();
    let features = ptr::null();//CString::new("").unwrap();
    //let level = LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault;
    let level = LLVMCodeGenOptLevel::LLVMCodeGenLevelNone;
    let reloc = LLVMRelocMode::LLVMRelocDefault;
    let code_model = LLVMCodeModel::LLVMCodeModelJITDefault;

    let result = tm::LLVMCreateTargetMachine(
      target_ref, triple, cpu.as_ptr(), features, level, reloc, code_model
    );

    core::LLVMDisposeMessage(triple);

    Ok(result)
  }
}

impl Drop for JITEngine {
  fn drop(&mut self) {
    unsafe {
      orc::LLVMOrcDisposeInstance(self.jit_stack);
      tm::LLVMDisposeTargetMachine(self.target_machine);
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
