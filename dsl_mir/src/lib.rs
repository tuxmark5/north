use {
  interval_map::Entry,
  north_core::quote::{QuoteType, RustFn},
  serde::Serialize,
  std::ops::Range,
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub mod prelude {
  pub use super::{
    BinOp,
    BuildBlock,
    BuildConstArray,
    BuildConstTuple,
    BuildCtlSwitch,
    BuildCtlSwitchRange,
    BuildItemFn,
    BuildItemFnLate,
    BuildItemStatic,
    BuildItems,
    BuildStmtCall,
    BuildStmtStruct,
    BuildTypeStruct
  };

  pub use {
    north_core::quote::{QuoteType, RustFn},
    serde::Serialize,
  };
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub enum BinOp {
  Add,
  CmpEq,
  Sub,
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait Config: Sized {
  type Block;
  type LVal: Clone;
  type RVal: Clone;
  type Type: Clone;

  type BuildBlock: BuildBlock<Self>;
  type BuildConstArray: BuildConstArray<Self>;
  type BuildConstTuple: BuildConstTuple<Self>;
  type BuildCtlSwitch: BuildCtlSwitch<Self>;
  type BuildCtlSwitchRange: BuildCtlSwitchRange<Self>;
  type BuildItemFn: BuildItemFn<Self>;
  type BuildItemFnLate: BuildItemFnLate<Self>;
  type BuildItemStatic: BuildItemStatic<Self>;
  type BuildItems: BuildItems<Self>;
  type BuildStmtCall: BuildStmtCall<Self>;
  type BuildStmtStruct: BuildStmtStruct<Self>;
  type BuildTypeStruct: BuildTypeStruct<Self>;
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait BuildBlock<C: Config> {
  fn ctl_br(&mut self, next: C::Block);
  fn ctl_if(&mut self, cond: C::RVal, then_b: C::Block, else_b: C::Block);
  fn ctl_ret(&mut self, value: Option<C::RVal>);
  fn ctl_switch(&mut self, value: C::RVal, def_case: C::Block) -> C::BuildCtlSwitch;
  fn ctl_switch_range(&mut self, value: C::RVal, def_case: C::Block) -> C::BuildCtlSwitchRange;
  fn lval_field(&mut self, base: C::LVal, field: usize) -> C::LVal;
  fn stmt_bin_op(&mut self, op: BinOp, lhs: C::RVal, rhs: C::RVal) -> C::RVal;
  fn stmt_call(&mut self, func: C::RVal) -> C::BuildStmtCall;
  fn stmt_field(&mut self, base: C::RVal, field: usize) -> C::RVal;
  fn stmt_lval(&mut self, value: C::RVal) -> C::LVal;
  fn stmt_lval_cast(&mut self, value: C::LVal, ty: C::Type) -> C::LVal;
  fn stmt_rval_cast(&mut self, value: C::RVal, ty: C::Type) -> C::RVal;
  fn stmt_set(&mut self, dst: C::LVal, src: C::RVal);
  fn stmt_struct(&mut self, ty: Option<C::Type>) -> C::BuildStmtStruct;
  fn stmt_use(&mut self, value: C::LVal) -> C::RVal;
}

pub trait BuildConstArray<C: Config> {
  fn add_element(&mut self, value: C::RVal);
  fn complete(self) -> C::RVal;
}

pub trait BuildConstTuple<C: Config> {
  fn add_element(&mut self, value: C::RVal);
  fn complete(self) -> C::RVal;
}

pub trait BuildCtlSwitch<C: Config> {
  fn add_case(&mut self, value: C::RVal, target: C::Block);
  fn add_cases<I>(&mut self, iter: I) where
    I: IntoIterator<Item=(C::RVal, C::Block)>;
  fn complete(self, fb: &mut C::BuildItemFnLate, bb: &mut C::BuildBlock);
}

pub trait BuildCtlSwitchRange<C: Config> {
  fn add_case(&mut self, value: Range<u32>, target: C::Block);
  fn add_cases<I>(&mut self, iter: I) where
    I: IntoIterator<Item=Entry<u32, C::Block>>;
  fn complete(self, fb: &mut C::BuildItemFnLate, bb: &mut C::BuildBlock);
}

pub trait BuildItemFn<C: Config> {
  fn add_param(&mut self, name: &str, ty: C::Type);
  fn complete(self) -> (C::RVal, C::BuildItemFnLate);
}

pub trait BuildItemFnLate<C: Config> {
  fn build_block(&mut self, name: &str) -> (C::Block, C::BuildBlock);
  fn build_block_resume(&mut self, block: C::Block) -> (C::Block, C::BuildBlock);
  fn get_param(&mut self, idx: usize) -> C::RVal;
}

pub trait BuildItemStatic<C: Config> {
  fn set_init(self, init: C::RVal);
}

pub trait BuildItems<C: Config> { 
  fn const_align_of(&self, ty: C::Type) -> C::RVal;
  fn const_array(&self, ty: C::Type) -> C::BuildConstArray;
  fn const_null(&self, ty: C::Type) -> C::RVal;
  fn const_tuple(&self) -> C::BuildConstTuple;
  fn const_rval_cast(&mut self, value: C::RVal, ty: C::Type) -> C::RVal;
  fn const_size_of(&self, ty: C::Type) -> C::RVal;

  fn item_fn(&self, name: &str, ret_ty: C::Type) -> C::BuildItemFn;
  fn item_fn_resume(&self, fn_val: C::RVal) -> C::BuildItemFnLate;
  fn item_static(&self, name: &str, ty: C::Type) -> (C::RVal, C::BuildItemStatic);
  fn item_type(&self, name: &str, ty: C::Type) -> C::Type;

  fn quote_const<T: Serialize>(&self, value: T) -> C::RVal;
  fn quote_fn<F: FnOnce() -> RustFn>(&self, value: F) -> C::RVal;
  fn quote_type<T: QuoteType>(&self) -> C::Type;

  fn type_array(&self, elem_ty: C::Type, size: usize) -> C::Type;
  fn type_ref(&self, mutable: bool, elem: C::Type) -> C::Type;
  fn type_struct(&self, name: Option<&str>) -> C::BuildTypeStruct;
  fn type_void(&self) -> C::Type;
}

pub trait BuildStmtCall<C: Config> {
  fn add_arg(&mut self, value: C::RVal);
  fn complete(self, bb: &mut C::BuildBlock) -> C::RVal;
}

pub trait BuildStmtStruct<C: Config> {
  fn add_element(&mut self, value: C::RVal);
  fn add_elements<T>(&mut self, iter: T) where
    T: IntoIterator<Item=C::RVal>;
  fn complete(self, bb: &mut C::BuildBlock) -> C::RVal;
}

pub trait BuildTypeStruct<C: Config> {
  fn add_element(&mut self, ty: C::Type);
  fn add_elements<T>(&mut self, iter: T) where
    T: IntoIterator<Item=C::Type>;
  fn complete(self, b: &mut C::BuildItems) -> C::Type;
}

////////////////////////////////////////////////////////////////////////////////////////////////
