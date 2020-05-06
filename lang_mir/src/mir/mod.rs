use {
  crate::builder,
  interval_map::IntervalMap,
  north_core::{
    model::{Child, Link, Model, ModelCell, Reference as Ref},
    prelude::*,
    quote::RustFn,
    structure::ForestExt,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub mod ty;
pub use self::ty::*;

////////////////////////////////////////////////////////////////////////////////////////////////

pub type BlockId      = NodeId<Block>;
pub type BlockRef     = Ref<Block>;
pub type ConstLink    = Link<dyn Const>;
pub type LValLink     = Link<dyn LVal>;
pub type RValLink     = Link<dyn Value>;
pub type RValLinkOpt  = Option<RValLink>;
pub type TypeLink     = Link<dyn Type>;

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait Const: Node { }
pub trait Ctl: Node { }

pub trait Fn: Node {
  fn add_block(&mut self, model: &mut Model) -> BlockId;
  fn add_local(&mut self, model: &mut Model, ty: Link<dyn Type>) -> RValLink;
  fn add_param(&mut self, model: &mut Model, ty: Link<dyn Type>) -> RValLink;
}

pub trait Item: Node { }
pub trait LVal: Node { }
pub trait Stmt: Node { }
pub trait Value: Node { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct Block {
  pub head: Vec<Child<dyn Stmt>>,
  pub tail: Child<dyn Ctl>,
}

impl Block {
  pub fn first_node(&self) -> NodeId {
    self.head.first()
      .map(|n| n.to_top())
      .unwrap_or_else(|| self.tail.to_top())
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct ConstInt {
  pub ty: Link<TypeInt>,
  pub value: i64,
}

impl Const for ConstInt { }
impl Value for ConstInt { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct ConstStruct {
  //pub ty: Link<TypeInt>,
  pub elements: Vec<ConstLink>,
}

impl Const for ConstStruct { }
impl Value for ConstStruct { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct CtlBr {
  pub block: BlockRef,
}

impl Ctl for CtlBr { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct CtlIf {
  pub cond: RValLink,
  pub block1: BlockRef,
  pub block0: BlockRef,
}

impl Ctl for CtlIf { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct CtlRet {
  pub value: RValLinkOpt,
}

impl Ctl for CtlRet { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct CtlSwitch {
  pub value: RValLink,
  pub cases: Vec<(ConstLink, BlockRef)>,
  pub default_case: BlockRef,
}

impl Ctl for CtlSwitch { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct CtlSwitchRange {
  pub value: RValLink,
  pub cases: IntervalMap<ConstLink, BlockRef>,
  pub default_case: BlockRef,
}

impl Ctl for CtlSwitchRange { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct ItemBuiltinFn {
  //pub target_fn: fn
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct ItemFn {
  pub name: String,
  pub fn_ty: Link<TypeFn>,
  pub params: Vec<Child<Param>>,
  pub locals: Vec<Child<Local>>,
  pub blocks: Vec<Child<Block>>
}

impl ItemFn {
  pub fn build<T: ToString>(model_cell: ModelCell, name: T) -> builder::ItemFnBuilder {
    builder::ItemFnBuilder::new(model_cell, name)
  }

  pub fn new(name: String, fn_ty: Link<TypeFn>) -> Self {
    Self {
      name,
      fn_ty,
      params: Vec::new(),
      locals: Vec::new(),
      blocks: Vec::new(),
    }
  }
}

impl Const for ItemFn { }

impl Fn for ItemFn {
  fn add_block(&mut self, model: &mut Model) -> BlockId {
    let block_id = model.new_node::<Block>();
    self.blocks.push(block_id.into());
    block_id
  }

  fn add_local(&mut self, model: &mut Model, ty: Link<dyn Type>) -> RValLink {
    let local_node = Local { ty };
    let local_id = model.build_node(local_node);
    self.locals.push(local_id.into());
    local_id.up().into()
  }

  fn add_param(&mut self, model: &mut Model, ty: Link<dyn Type>) -> RValLink {
    let param_idx = self.params.len();
    let param_node = Param { ty, index: param_idx, is_lval: true };
    let param_id = model.build_node(param_node);
    self.params.push(param_id.into());
    param_id.up().into()
  }
}

impl Item for ItemFn { }
impl Value for ItemFn { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct ItemGlob {
  pub ty: TypeLink,
  pub init: Option<ConstLink>,
}

impl Item for ItemGlob { }
impl Value for ItemGlob { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct Local {
  pub ty: Link<dyn Type>
}

impl Value for Local { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct LValDeref {
  pub value: RValLink,
}

impl LVal for LValDeref { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct LValMember {
  pub base: LValLink,
  pub path: Vec<u32>,
}

impl LVal for LValMember { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct Mod {
  pub items: Vec<Child<dyn Item>>
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct Param {
  pub ty: Link<dyn Type>,
  pub index: usize,
  pub is_lval: bool,
}

impl Value for Param { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct RValBuiltinFn {
  pub rust_fn: RustFn,
}

impl Value for RValBuiltinFn { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct RValMember {
  pub base: RValLink,
  pub path: Vec<usize>,
}

impl Value for RValMember { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct RValParam {
  pub ty: Link<dyn Type>,
  pub index: usize,
}

impl Value for RValParam { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct StmtCall {
  pub target: RValLink,
  pub args: Vec<RValLink>,
}

impl Stmt for StmtCall { }
impl Value for StmtCall { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct StmtOpBin {
  pub op: StmtOpBinType,
  pub left: RValLink,
  pub right: RValLink,
}

impl Stmt for StmtOpBin { }
impl Value for StmtOpBin { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Copy, Debug)]
pub enum StmtOpBinType {
  AriAdd,
  AriDiv,
  AriMod,
  AriMul,
  AriSub,
  BitAnd,
  BitOr,
  BitShl,
  BitShr,
  BitXor,
  CmpEq,
  CmpGt,
  CmpGtEq,
  CmpLt,
  CmpLtEq,
  CmpNotEq,
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct StmtSet {
  pub target: LValLink,
  pub value: RValLink,
}

impl Stmt for StmtSet { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct StmtStruct {
  pub ty: Link<TypeStruct>,
  pub values: Vec<RValLink>,
}

impl Stmt for StmtStruct { }
impl Value for StmtStruct { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct StmtUse {
  pub lvalue: LValLink
}

impl Stmt for StmtUse { }
impl Value for StmtUse { }

////////////////////////////////////////////////////////////////////////////////////////////////

concept_map_part! {
  ConceptMapPart {
    Block           => {},
    ConstInt        => {dyn Const, dyn Value},
    ConstStruct     => {dyn Const, dyn Value},
    CtlBr           => {dyn Ctl},
    CtlIf           => {dyn Ctl},
    CtlRet          => {dyn Ctl},
    CtlSwitch       => {dyn Ctl},
    CtlSwitchRange  => {dyn Ctl},
    ItemFn          => {dyn Const, dyn Fn, dyn Item, dyn Value},
    ItemGlob        => {dyn Item, dyn Value},
    Local           => {dyn Value},
    LValDeref       => {dyn LVal},
    LValMember      => {dyn LVal},
    Mod             => {},
    Param           => {dyn Value},
    RValBuiltinFn   => {dyn Value},
    RValMember      => {dyn Value},
    RValParam       => {dyn Value},
    StmtCall        => {dyn Stmt, dyn Value},
    StmtOpBin       => {dyn Stmt, dyn Value},
    StmtSet         => {dyn Stmt},
    StmtStruct      => {dyn Stmt, dyn Value},
    StmtUse         => {dyn Stmt, dyn Value},
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
