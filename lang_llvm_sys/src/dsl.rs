use {
  crate::{
    builder::Builder,
    context::Context,
  },
  dsl_mir::{self as dsl, prelude::*},
  indexmap::IndexMap,
  interval_map::{Entry, IntervalMap},
  llvm_sys::{
    LLVMIntPredicate::{self, LLVMIntULT},
    LLVMOpcode,
    core as llvm_core,
    prelude::*,
  },
  std::{
    cell::Cell,
    ops::Range,
    rc::Rc,
  }
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub type Block = LLVMBasicBlockRef;
pub type LVal = LLVMValueRef;
pub type Type = LLVMTypeRef;

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone)]
pub enum RVal {
  Ready(LLVMValueRef),
  Pending(Rc<Cell<Option<LLVMValueRef>>>)
}

impl RVal {
  pub fn new(value: LLVMValueRef) -> Self {
    RVal::Ready(value)
  }

  pub fn new_pending() -> Self {
    let cell = Rc::new(Cell::new(None));
    RVal::Pending(cell)
  }

  pub fn complete(&self, value: LLVMValueRef) -> Self {
    match self {
      RVal::Ready(_) => panic!("RVal already completed"),
      RVal::Pending(cell) => {
        cell.set(Some(value));
        RVal::Ready(value)
      }
    }
  }

  pub fn raw(&self) -> LLVMValueRef {
    match self {
      RVal::Ready(value) => *value,
      RVal::Pending(cell) => cell.get()
        .expect("invalid unwrap on incomplete RVal"),
    }
  }
}

impl From<LLVMValueRef> for RVal {
  fn from(value: LLVMValueRef) -> Self {
    Self::new(value)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Config;

impl dsl::Config for Config {
  type Block  = Block;
  type LVal   = LVal;
  type RVal   = RVal;
  type Type   = Type;

  type BuildBlock           = BuildBlock;
  type BuildConstArray      = BuildConstArray;
  type BuildConstTuple      = BuildConstTuple;
  type BuildCtlSwitch       = BuildCtlSwitch;
  type BuildCtlSwitchRange  = BuildCtlSwitchRange;
  type BuildItemFn          = BuildItemFn;
  type BuildItemFnLate      = BuildItemFnLate;
  type BuildItemStatic      = BuildItemStatic;
  type BuildItems           = BuildItems;
  type BuildStmtCall        = BuildStmtCall;
  type BuildStmtStruct      = BuildStmtStruct;
  type BuildTypeStruct      = BuildTypeStruct;
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct BuildBlock {
  pub builder: Builder,
}

impl BuildBlock {
  pub fn new(context: Rc<Context>, block: Block) -> Self {
    let builder = Builder::new(context);
    builder.position_at_end(block);
    Self { builder }
  }
}

impl dsl::BuildBlock<Config> for BuildBlock {
  fn ctl_br(&mut self, next: Block) {
    self.builder.build_br(next);
  }

  fn ctl_if(&mut self, cond: RVal, then_b: Block, else_b: Block) {
    self.builder.build_cond_br(cond.raw(), then_b, else_b);
  }

  fn ctl_ret(&mut self, value: Option<RVal>) { 
    match value {
      Some(value) => { self.builder.build_ret(value.raw()); },
      None => { self.builder.build_ret_void(); },
    }
  }

  fn ctl_switch(&mut self, value: RVal, def_case: Block) -> BuildCtlSwitch {
    BuildCtlSwitch {
      value, def_case, cases: Vec::new(),
    }
  }

  fn ctl_switch_range(&mut self, value: RVal, def_case: Block) -> BuildCtlSwitchRange {
    BuildCtlSwitchRange {
      value, def_case, cases: IntervalMap::new(),
    }
  }

  fn lval_field(&mut self, base: LVal, field: usize) -> LVal {
    // assert base is ptr
    self.builder.build_struct_gep(base, field)
  }

  fn stmt_bin_op(&mut self, op: BinOp, lhs: RVal, rhs: RVal) -> RVal {
    let b = &mut self.builder;
    let (lhs, rhs) = (lhs.raw(), rhs.raw());
    match op {
      BinOp::Add => { b.build_bin_op(LLVMOpcode::LLVMAdd, lhs, rhs).into() },
      BinOp::CmpEq => { b.build_icmp(LLVMIntPredicate::LLVMIntEQ, lhs, rhs).into() },
      BinOp::Sub => { b.build_bin_op(LLVMOpcode::LLVMSub, lhs, rhs).into() },
    }
  }

  fn stmt_call(&mut self, target: RVal) -> BuildStmtCall { 
    BuildStmtCall {
      target,
      args: Vec::new(),
    }
  }

  fn stmt_field(&mut self, base: RVal, field: usize) -> RVal { 
    self.builder.build_extract_value(base.raw(), &[field]).into()
  }

  fn stmt_lval(&mut self, value: RVal) -> LVal { 
    value.raw()
  }

  fn stmt_lval_cast(&mut self, value: LVal, ty: Type) -> LVal {
    let context = self.builder.context();
    let ptr_ty = context.type_pointer(ty);
    self.builder.build_bit_cast(value, ptr_ty)
  }

  fn stmt_rval_cast(&mut self, value: RVal, ty: Type) -> RVal {
    self.builder.build_bit_cast(value.raw(), ty).into()
  }

  fn stmt_set(&mut self, dst: LVal, src: RVal) { 
    self.builder.build_store(src.raw(), dst);
  }

  fn stmt_struct(&mut self, ty: Option<Type>) -> BuildStmtStruct { 
    BuildStmtStruct {
      elems: Vec::new(),
      ty,
    }
  }

  fn stmt_use(&mut self, value: LVal) -> RVal { 
    self.builder.build_load(value).into()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct BuildConstArray {
  context: Rc<Context>,
  elem_ty: Type,
  values: Vec<RVal>,
}

impl dsl::BuildConstArray<Config> for BuildConstArray {
  fn add_element(&mut self, value: RVal) { 
    self.values.push(value);
  }

  fn complete(self) -> RVal { 
    let mut values = self.values.into_iter()
      .map(|v| v.raw())
      .collect::<Vec<_>>();
    let raw = self.context.const_array(self.elem_ty, values.as_mut());
    RVal::new(raw)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct BuildConstTuple {
  context: Rc<Context>,
  values: Vec<RVal>,
}

impl BuildConstTuple {
  pub fn new(context: Rc<Context>) -> Self {
    let values = Vec::new();
    Self { context, values }
  }
}

impl dsl::BuildConstTuple<Config> for BuildConstTuple {
  fn add_element(&mut self, value: RVal) { 
    self.values.push(value);
  }

  fn complete(self) -> RVal { 
    let mut values = self.values.into_iter()
      .map(|v| v.raw())
      .collect::<Vec<_>>();
    let raw = self.context.const_struct(values.as_mut());
    RVal::new(raw)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct BuildCtlSwitch {
  value: RVal,
  def_case: Block,
  cases: Vec<(LLVMValueRef, Block)>,
}

impl dsl::BuildCtlSwitch<Config> for BuildCtlSwitch {
  fn add_case(&mut self, value: RVal, target: Block) {
    self.cases.push((value.raw(), target));
  }

  fn add_cases<I>(&mut self, iter: I) where
    I: IntoIterator<Item=(RVal, Block)>
  {
    let iter = iter.into_iter()
      .map(|(l, b)| (l.raw(), b));
    self.cases.extend(iter);
  }

  fn complete(mut self, _fb: &mut BuildItemFnLate, bb: &mut BuildBlock) {
    let cases = self.cases.as_mut();
    bb.builder.build_switch(self.value.raw(), self.def_case, cases);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct BuildCtlSwitchRange {
  value: RVal,
  def_case: Block,
  cases: IntervalMap<u32, Block>,
}

impl BuildCtlSwitchRange {
  pub fn gen_binary_search(
    b     : &mut Builder,
    fb    : &mut BuildItemFnLate,
    key   : LLVMValueRef,
    fail  : Block,
    curr  : &Option<Block>,
    slice : &[Entry<u32, Block>],
  ) -> LLVMBasicBlockRef
  {
    if slice.is_empty() {
      return curr.unwrap_or(fail);
    }

    let middle = slice.len() / 2;
    let left = &slice[0..middle];
    let entry = &slice[middle];
    let right = &slice[(middle + 1)..];

    let block_left = Self::gen_binary_search(b, fb, key, fail, curr, left);
    let block_right = Self::gen_binary_search(b, fb, key, fail, &entry.value, right);

    let block = fb.context.append_basic_block(fb.raw_fn);
    b.position_at_end(block);

    let pivot = b.quote_const(&(entry.key as u32));
    let result = b.build_icmp(LLVMIntULT, key, pivot);
    b.build_cond_br(result, block_left, block_right);

    block
  }

  pub fn gen_switch(
    b       : &mut Builder,
    key     : LLVMValueRef,
    else_b  : LLVMBasicBlockRef,
    cases   : &IntervalMap<u32, Block>,
  ) {
    let mut switch_cases = Vec::with_capacity(256);

    for k in 0..256 {
      if let Some(v) = cases.get(&k) {
        let k = b.quote_const(&(k as u8));
        switch_cases.push((k, *v));
      } else {
        //break;
      }
    }

    b.build_switch(
      key,
      else_b,
      switch_cases.as_ref()
    );
  }

  pub fn gen_switch_2(
    b       : &mut Builder,
    key     : LLVMValueRef,
    else_b  : LLVMBasicBlockRef,
    cases   : &IntervalMap<u32, Block>,
  ) {
    let (table, blocks) = Self::gen_table_pair(cases);
    let table = Box::leak(box table); // DEDUP
    let table = b.quote_ptr(table.as_ptr());

    let index_ptr = b.build_in_bounds_gep(table, &mut [key]);
    let index = b.build_load(index_ptr);

    let switch_cases = blocks.into_iter()
      .enumerate()
      .map(|(i, block)| (b.quote_const(&(i as u8)), block))
      .collect::<Vec<_>>();

    b.build_switch(
      index,
      else_b,
      switch_cases.as_ref()
    );
  }

  pub fn gen_table_pair(cases: &IntervalMap<u32, Block>) -> (Vec<u8>, Vec<Block>) {
    let mut table = Vec::with_capacity(256);
    let mut values = IndexMap::new();
    table.resize(256, 0xFFu8);

    for (range, value) in cases.iter() {
      let entry = values.entry(*value);
      let index = entry.index() as u8;
      entry.or_insert(());

      for i in range {
        table[i as usize] = index;
      }
    }

    let values = values.keys().map(|b| *b).collect();
    (table, values)
  }
}

impl dsl::BuildCtlSwitchRange<Config> for BuildCtlSwitchRange {
  fn add_case(&mut self, value: Range<u32>, target: Block) {
    self.cases.insert(value, Some(target));
  }

  fn add_cases<I>(&mut self, iter: I) where
    I: IntoIterator<Item=Entry<u32, Block>>
  {
    self.cases.extend(iter);
  }

  fn complete(self, fb: &mut BuildItemFnLate, bb: &mut BuildBlock) {
    // Self::gen_switch(
    //   &mut bb.builder, 
    //   self.value.raw(),
    //   self.def_case,
    //   &self.cases,
    // );

    let curr_block = bb.builder.current_block().unwrap();

    let entry_block = Self::gen_binary_search(
      &mut bb.builder, 
      fb, 
      self.value.raw(), 
      self.def_case, 
      &Some(self.def_case),
      self.cases.as_ref()
    );

    bb.builder.position_at_end(curr_block);
    bb.builder.build_br(entry_block);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct BuildItemFn {
  context : Rc<Context>,
  module  : LLVMModuleRef,
  name    : String,
  params  : Vec<(String, Type)>,
  ret_ty  : Type,
}

impl BuildItemFn {
  fn new(context: Rc<Context>, module: LLVMModuleRef, name: String, ret_ty: Type) -> Self {
    let params = Vec::new();
    Self { context, module, name, params, ret_ty }
  }
}

impl dsl::BuildItemFn<Config> for BuildItemFn {
  fn add_param(&mut self, name: &str, ty: Type) { 
    let name = String::from(name);
    self.params.push((name, ty));
  }

  fn complete(self) -> (RVal, BuildItemFnLate) { 
    let mut param_tys = self.params.iter().map(|t| t.1).collect::<Vec<_>>();
    let context = self.context;
    let fn_ty = context.type_function(self.ret_ty, param_tys.as_mut());

    let raw_fn = context.add_function(self.module, self.name.as_str(), fn_ty);
    let params = self.params.iter()
      .enumerate()
      .map(|(i, _)| unsafe { llvm_core::LLVMGetParam(raw_fn, i as u32) })
      .collect::<Vec<_>>();

    let builder = BuildItemFnLate { context, raw_fn, params };
    (RVal::new(raw_fn), builder)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct BuildItemFnLate {
  context: Rc<Context>,
  raw_fn: LLVMValueRef,
  params: Vec<LLVMValueRef>,
}

impl dsl::BuildItemFnLate<Config> for BuildItemFnLate {
  fn build_block(&mut self, _name: &str) -> (Block, BuildBlock) { 
    let block = self.context.append_basic_block(self.raw_fn);
    let builder = BuildBlock::new(self.context.clone(), block);
    (block, builder)
  }

  fn build_block_resume(&mut self, block: Block) -> (Block, BuildBlock) {
    let builder = BuildBlock::new(self.context.clone(), block);
    (block, builder)
  }

  fn get_param(&mut self, idx: usize) -> RVal {
    RVal::new(self.params[idx])
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct BuildItemStatic {
  global: RVal,
}

impl BuildItemStatic {
  pub fn new(global: RVal) -> Self {
    Self { global }
  }
}

impl dsl::BuildItemStatic<Config> for BuildItemStatic {
  fn set_init(self, init: RVal) { 
    let global_raw = self.global.raw();
    let init_raw = init.raw();
    unsafe { 
      llvm_core::LLVMSetInitializer(global_raw, init_raw); 
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct BuildItems {
  context: Rc<Context>,
  module: LLVMModuleRef,
}

impl BuildItems {
  pub fn new(context: Rc<Context>, module: LLVMModuleRef) -> Self {
    Self { context, module }
  }
}

impl dsl::BuildItems<Config> for BuildItems {
  fn const_align_of(&self, ty: Type) -> RVal {
    unsafe { llvm_core::LLVMAlignOf(ty).into() }
  }

  fn const_array(&self, elem_ty: Type) -> BuildConstArray { 
    BuildConstArray {
      context: self.context.clone(),
      elem_ty,
      values: Vec::new(),
    }
  }

  fn const_null(&self, ty: Type) -> RVal {
    unsafe { llvm_core::LLVMConstNull(ty).into() }
  }
  
  fn const_tuple(&self) -> BuildConstTuple { 
    BuildConstTuple::new(self.context.clone())
  }

  fn const_rval_cast(&mut self, value: RVal, ty: Type) -> RVal {
    unsafe { llvm_core::LLVMConstBitCast(value.raw(), ty).into() }
  }

  fn const_size_of(&self, ty: Type) -> RVal {
    unsafe { llvm_core::LLVMSizeOf(ty).into() }
  }

  fn item_fn(&self, name: &str, ret_ty: Type) -> BuildItemFn { 
    let context = self.context.clone();
    let name = String::from(name);
    BuildItemFn::new(context, self.module, name, ret_ty)
  }

  fn item_fn_resume(&self, fn_val: RVal) -> BuildItemFnLate {
    let raw_fn = fn_val.raw();

    let mut params = Vec::new();
    let num_params = unsafe { llvm_core::LLVMCountParams(raw_fn) };
    for i in 0..num_params {
      let param = unsafe { llvm_core::LLVMGetParam(raw_fn, i) };
      params.push(param);
    }

    BuildItemFnLate { 
      context: self.context.clone(), 
      raw_fn,
      params 
    }
  }

  fn item_static(&self, name: &str, ty: Type) -> (RVal, BuildItemStatic) { 
    let raw = self.context.add_global(self.module, name, ty, None);
    let rval = RVal::new(raw);
    let builder = BuildItemStatic::new(rval.clone());
    (rval, builder)
  }

  fn item_type(&self, _name: &str, ty: Type) -> Type {
    ty
  }

  fn quote_const<T: Serialize>(&self, value: T) -> RVal { 
    let raw = self.context.quote_const(&value);
    RVal::new(raw)
  }

  fn quote_fn<F: FnOnce() -> RustFn>(&self, value: F) -> RVal { 
    let rust_fn = value();
    let fn_ty = self.context.quote_type_id(rust_fn.fn_type);
    let fn_ty = self.context.type_pointer(fn_ty);
    let raw_fn = self.context.quote_const(&(rust_fn.raw_ptr as usize));
    let raw_fn = self.context.const_int_to_ptr(raw_fn, fn_ty);
    RVal::new(raw_fn)
  }

  fn quote_type<T: QuoteType>(&self) -> Type { 
    self.context.quote_type::<T>()
  }

  fn type_array(&self, elem_ty: Type, size: usize) -> Type {
    self.context.type_array(elem_ty, size)
  }

  fn type_ref(&self, _mutable: bool, elem: Type) -> Type { 
    self.context.type_pointer(elem)
  }

  fn type_struct(&self, name: Option<&str>) -> BuildTypeStruct {
    let name = name.map(|n| String::from(n));
    BuildTypeStruct { name, elems: Vec::new() }
  }

  fn type_void(&self) -> Type {
    self.context.type_void()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct BuildStmtCall {
  target: RVal,
  args: Vec<LLVMValueRef>,
}

impl dsl::BuildStmtCall<Config> for BuildStmtCall {
  fn add_arg(&mut self, value: RVal) { 
    self.args.push(value.raw());
  }

  fn complete(mut self, bb: &mut BuildBlock) -> RVal { 
    let target = self.target.raw();
    let args = self.args.as_mut();
    let result = bb.builder.build_call(target, args);
    RVal::new(result)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct BuildStmtStruct {
  elems: Vec<LLVMValueRef>,
  ty: Option<LLVMTypeRef>,
}

impl dsl::BuildStmtStruct<Config> for BuildStmtStruct {
  fn add_element(&mut self, value: RVal) { 
    self.elems.push(value.raw());
  }

  fn add_elements<T>(&mut self, iter: T) where
    T: IntoIterator<Item=RVal>
  {
    let iter = iter.into_iter().map(|e| e.raw());
    self.elems.extend(iter);
  }

  fn complete(mut self, bb: &mut BuildBlock) -> RVal { 
    let context = bb.builder.context();

    let struct_ty = self.ty.unwrap_or_else(|| {
      let mut types = self.elems.iter()
      .map(|e| context.type_of(*e))
      .collect::<Vec<_>>();
      context.type_struct(types.as_mut())
    });

    let elems = self.elems.as_mut();
    let raw = bb.builder.build_struct(struct_ty, elems);
    RVal::new(raw)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct BuildTypeStruct {
  name: Option<String>,
  elems: Vec<LLVMTypeRef>,
}

impl dsl::BuildTypeStruct<Config> for BuildTypeStruct {
  fn add_element(&mut self, ty: Type) { 
    self.elems.push(ty);
  }

  fn add_elements<T>(&mut self, iter: T) where
    T: IntoIterator<Item=Type> 
  {
    self.elems.extend(iter);
  }

  fn complete(mut self, b: &mut BuildItems) -> Type { 
    let elems = self.elems.as_mut();
    match self.name {
      Some(name) => b.context.type_struct_named(name.as_ref(), elems),
      None => b.context.type_struct(elems),
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
