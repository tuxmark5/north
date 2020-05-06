use {
  crate::{
    dsl,
    serializer::serialize,
    types::{
      reduce_type::Reducer
    }
  },
  dsl_mir::BuildItems,
  north_core::{
    quote::{QuoteType, TypeId},
  },
  north_gen::{
    ReduceCtxCore,
    Reduction,
  },
  llvm_sys::{
    LLVMTypeKind,
    core as llvm_core,
    prelude::*,
  },
  serde::Serialize,
  std::{
    ptr,
    cell::RefCell,
    collections::{HashMap, hash_map::Entry::*},
    ffi::CString,
    rc::Rc
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////
pub type AttrMap = HashMap<&'static str, LLVMAttributeRef>;
////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Context {
  raw_ctx: LLVMContextRef,
  attributes: RefCell<AttrMap>,
  reducer: RefCell<Reducer>,
}

impl Context {
  pub fn new() -> Self {
    let raw_ctx = unsafe { llvm_core::LLVMContextCreate() };
    Self {
      raw_ctx,
      attributes: RefCell::new(AttrMap::new()),
      reducer: RefCell::new(Reducer::new()),
    }
  }

  pub fn add_global(
    &self,
    mod_ref : LLVMModuleRef,
    name    : &str,
    ty      : LLVMTypeRef,
    init    : Option<LLVMValueRef>,
  ) -> LLVMValueRef {
    let name = CString::new(name).unwrap();
    let ty = self.type_not_empty(ty);
    unsafe {
      let global = llvm_core::LLVMAddGlobal(mod_ref, ty, name.as_ptr());
      if let Some(init) = init {
        llvm_core::LLVMSetInitializer(global, init);
      }
      global
    }
  }

  pub fn add_function(
    &self,
    mod_ref : LLVMModuleRef,
    name    : &str,
    fn_ty   : LLVMTypeRef
  ) -> LLVMValueRef {
    let name = CString::new(name).unwrap();
    unsafe { llvm_core::LLVMAddFunction(mod_ref, name.as_ptr(), fn_ty) }
  }

  pub fn append_basic_block(&self, fn_: LLVMValueRef) -> LLVMBasicBlockRef {
    let name = "\0".as_ptr() as *const i8;
    unsafe { llvm_core::LLVMAppendBasicBlockInContext(self.raw_ctx, fn_, name) }
  }

  pub fn attribute(&self, attr: &'static str) -> LLVMAttributeRef {
    let mut attrs = self.attributes.borrow_mut();

    match attrs.entry(attr) {
      Occupied(occupied) => *occupied.get(),
      Vacant(vacant) => *vacant.insert(self.create_attribute(attr)),
    }
  }

  pub fn create_module(&self, _name: &str) -> LLVMModuleRef {
    let name = "mod\0".as_ptr() as *const i8;
    unsafe { llvm_core::LLVMModuleCreateWithNameInContext(name, self.raw_ctx) }
    //unsafe { llvm_core::LLVMModuleCreateWithName(name) }
  }

  pub fn const_array(
    &self, 
    elem_ty: LLVMTypeRef, 
    values: &mut [LLVMValueRef]
  ) -> LLVMValueRef {
    if values.is_empty() {
      return ptr::null_mut();
    }

    unsafe {
      llvm_core::LLVMConstArray(
        elem_ty,
        values.as_mut_ptr(), 
        values.len() as u32,
      )
    }
  }

  pub fn const_bit_cast(&self, value: LLVMValueRef, ty: LLVMTypeRef) -> LLVMValueRef {
    unsafe {
      llvm_core::LLVMConstBitCast(value, ty)
    }
  }

  pub fn const_int_to_ptr(&self, value: LLVMValueRef, ty: LLVMTypeRef) -> LLVMValueRef {
    unsafe {
      llvm_core::LLVMConstIntToPtr(value, ty)
    }
  }

  pub fn const_struct(&self, values: &mut [LLVMValueRef]) -> LLVMValueRef {
    unsafe {
      llvm_core::LLVMConstStructInContext(
        self.raw_ctx,
        values.as_mut_ptr(), 
        values.len() as u32,
        0
      )
    }
  }

  pub fn create_attribute(&self, attr: &'static str) -> LLVMAttributeRef {
    let c_attr = CString::new(attr).unwrap();

    unsafe {
      let kind = llvm_core::LLVMGetEnumAttributeKindForName(
        c_attr.as_ptr(), attr.len()
      );

      llvm_core::LLVMCreateEnumAttribute(self.raw_ctx, kind, 0)

      // llvm_core::LLVMCreateStringAttribute(
      //   self.raw_ctx, c_attr.as_ptr(), attr.len() as u32, ptr::null(), 0
      // )
    }
  }

  pub fn dump_module(&self, mod_ref: LLVMModuleRef) {
    unsafe {
      let fun = llvm_core::LLVMGetFirstFunction(mod_ref);
      println!("FIRST {:?} {:?}", mod_ref, fun);
      llvm_core::LLVMDumpModule(mod_ref)
    }
  }

  pub fn quote_const<T: Serialize>(&self, value: &T) -> LLVMValueRef {
    serialize(self, value).unwrap()
  }

  pub fn quote_ptr<T: QuoteType>(&self, ptr: *const T) -> LLVMValueRef {
    let elem_ty = self.quote_type::<T>();
    let ptr_ty = self.type_pointer(elem_ty);
    let ptr = self.quote_const(&(ptr as usize));
    self.const_int_to_ptr(ptr, ptr_ty)
  }

  pub fn quote_type<T: QuoteType>(&self) -> LLVMTypeRef {
    let mut reducer = self.reducer.borrow_mut();
    reducer.quote::<T>(self)
  }

  pub fn quote_type_id(&self, type_id: TypeId) -> LLVMTypeRef {
    let mut reducer = self.reducer.borrow_mut();
    reducer.reduce_type(self, type_id)
  }

  pub fn raw(&self) -> LLVMContextRef {
    self.raw_ctx
  }

  pub fn type_array(&self, element: LLVMTypeRef, count: usize) -> LLVMTypeRef {
    unsafe { llvm_core::LLVMArrayType(element, count as u32) }
  }

  pub fn type_function(
    &self,
    return_type: LLVMTypeRef,
    param_types: &mut [LLVMTypeRef]
  ) -> LLVMTypeRef {
    let param_count = param_types.len() as u32;
    let param_types = param_types.as_mut_ptr();
    unsafe {
      llvm_core::LLVMFunctionType(return_type, param_types, param_count, 0)
    }
  }

  pub fn type_int(&self, bit_width: u32) -> LLVMTypeRef {
    unsafe { llvm_core::LLVMIntTypeInContext(self.raw_ctx, bit_width) }
  }

  pub fn type_not_empty(&self, ty: LLVMTypeRef) -> LLVMTypeRef {
    let is_zero = unsafe {
      match llvm_core::LLVMGetTypeKind(ty) {
        LLVMTypeKind::LLVMArrayTypeKind => { llvm_core::LLVMGetArrayLength(ty) == 0 },
        _ => false,
      }
    };

    match is_zero {
      true => self.type_int(8),
      false => ty,
    }
  }

  pub fn type_of(&self, value: LLVMValueRef) -> LLVMTypeRef {
    unsafe { llvm_core::LLVMTypeOf(value) }
  }

  pub fn type_pointer(&self, element_type: LLVMTypeRef) -> LLVMTypeRef {
    unsafe { llvm_core::LLVMPointerType(element_type, 0) }
  }

  pub fn type_struct(&self, element_types: &mut [LLVMTypeRef]) -> LLVMTypeRef {
    let element_count = element_types.len() as u32;
    let element_types = element_types.as_mut_ptr();
    unsafe { llvm_core::LLVMStructTypeInContext(self.raw_ctx, element_types, element_count, 0) }
  }

  pub fn type_struct_named(
    &self, name: &str, element_types: &mut [LLVMTypeRef],
  ) -> LLVMTypeRef {
    let name = CString::new(name).unwrap();
    let element_count = element_types.len() as u32;
    let element_types = element_types.as_mut_ptr();
    unsafe { 
      let ty = llvm_core::LLVMStructCreateNamed(self.raw_ctx, name.as_ptr());
      llvm_core::LLVMStructSetBody(ty, element_types, element_count, 0);
      ty
    }
  }

  pub fn type_void(&self) -> LLVMTypeRef {
    unsafe { llvm_core::LLVMVoidTypeInContext(self.raw_ctx) }
  } 
}

impl Drop for Context {
  fn drop(&mut self) {
    unsafe { llvm_core::LLVMContextDispose(self.raw_ctx); }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone)]
pub struct ContextInner {
  pub curr_fn: Option<LLVMValueRef>,
  pub curr_mod: Option<LLVMModuleRef>,
  pub llvm_ctx: Rc<Context>,
}

impl ContextInner {
  pub fn new(llvm_ctx: Rc<Context>) -> Self {
    Self {
      curr_fn: None,
      curr_mod: None,
      llvm_ctx,
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait ContextExt<'a, R> where
  R: Reduction<InnerCtx = ContextInner>
{
  fn build_global(&self, init: LLVMValueRef) -> LLVMValueRef {
    let llvm_ctx = self.llvm_ctx();
    let curr_mod = self.curr_mod();
    let ty = unsafe { llvm_core::LLVMTypeOf(init) };
    llvm_ctx.add_global(curr_mod, "global", ty, Some(init))
  }

  fn context(&self) -> &ReduceCtxCore<R>;

  fn curr_fn(&self) -> LLVMValueRef {
    self.inner().curr_fn.unwrap()
  }

  fn curr_mod(&self) -> LLVMModuleRef {
    self.inner().curr_mod.unwrap()
  }

  fn inner(&self) -> &ContextInner {
    &self.context().inner
  }

  fn make_bb(&self) -> LLVMBasicBlockRef {
    let curr_fn = self.curr_fn();
    let block = self.llvm_ctx().append_basic_block(curr_fn);
    block
  }

  fn llvm_ctx(&self) -> &Context {
    &*self.inner().llvm_ctx
  }

  fn resume_fn(&self) -> (dsl::BuildItems, dsl::BuildItemFnLate) {
    let context = self.inner().llvm_ctx.clone();
    let b = dsl::BuildItems::new(context, self.curr_mod());
    let fb = b.item_fn_resume(self.curr_fn().into());
    (b, fb)
  }

  /*fn set_curr_fn(&self, func: LLVMValueRef) {
    self.inner()
  }*/
}

impl<'a, R> ContextExt<'a, R> for ReduceCtxCore<'a, R> where
  R: Reduction<InnerCtx = ContextInner>
{
  fn context(&self) -> &ReduceCtxCore<R> { self }
}

////////////////////////////////////////////////////////////////////////////////////////////////
