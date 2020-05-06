use {
  crate::{
    quote_type,
    mir::{
      self, Block, BlockId, BlockRef, ConstLink, LValLink, 
      StmtOpBinType, RValLink, RValLinkOpt
    },
    quote_value::quote_value,
  },
  north_core::{
    model::{Child, Link, ModelCell},
    node_id::{NodeId, ToNodeId},
  },
  serde::Serialize,
  std::mem,
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait IntoBlockId {
  fn into_block_id(self) -> BlockId;
}

impl IntoBlockId for BlockId {
  fn into_block_id(self) -> BlockId { self }
}

impl<'c> IntoBlockId for &'c Cursor {
  fn into_block_id(self) -> BlockId { self.orig_block_id }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Cursor {
  output_model: ModelCell,
  curr_block_id: Option<BlockId>,
  orig_block_id: BlockId,
  statements: Vec<Child<dyn mir::Stmt>>,
}

impl Cursor {
  pub fn new(output_model: ModelCell, block_id: BlockId) -> Self {
    Self {
      output_model,
      curr_block_id: Some(block_id),
      orig_block_id: block_id,
      statements: Vec::new(),
    }
  }

  pub fn new_move(other: &mut Cursor) -> Self {
    let block_id = other.curr_block_id.take().unwrap();
    Self::new(other.output_model.clone(), block_id)
  }

  pub fn add_stmt(&mut self, stmt: Child<dyn mir::Stmt>) {
    if self.curr_block_id.is_some() {
      self.statements.push(stmt);
    }
  }

  pub fn build_const<N>(&mut self, node: N) -> Link<N> where
    N: mir::Const + mir::Value
  {
    let mut model = self.output_model.borrow_mut();
    let node_id = model.build_node(node);
    Link { node_id, owned: true }
  }

  pub fn build_ctl<N: mir::Ctl>(&mut self, node: N) {
    if let Some(block_id) = self.curr_block_id.take() {
      let mut model = self.output_model.borrow_mut();
      let tail_id = model.build_node(node);

      model.complete_node(block_id, Block {
        head: mem::replace(&mut self.statements, Vec::new()),
        tail: tail_id.up().into(),
      });
    }
  }

  pub fn build_lval<N>(&mut self, node: N) -> LValLink where
    N: mir::LVal
  {
    let node_id = self.output_model.build_node(node).up();
    self.add_stmt(node_id.cast().into());
    Link { node_id, owned: true }
  }

  pub fn build_stmt<N>(&mut self, node: N) -> NodeId<N> where
    N: mir::Stmt
  {
    let node_id = self.output_model.build_node(node);
    self.add_stmt(node_id.up().into());
    node_id
  }

  pub fn build_type<N>(&mut self, node: N) -> Link<N> where
    N: mir::Type
  {
    let mut model = self.output_model.borrow_mut();
    let node_id = model.build_node(node);
    Link { node_id, owned: false }
  }

  pub fn build_rval<N>(&mut self, node: N) -> RValLink where
    N: mir::Value
  {
    let mut model = self.output_model.borrow_mut();
    let node_id = model.build_node(node);
    node_id.up().into()
  }

  pub fn build_value<N>(&mut self, node: N) -> RValLink where
    N: mir::Stmt + mir::Value
  {
    let mut model = self.output_model.borrow_mut();
    let node_id = model.build_node(node);
    if self.curr_block_id.is_some() {
      self.statements.push(node_id.up().into());
    }
    node_id.up().into()
  }

  pub fn is_completed(&self) -> bool {
    self.curr_block_id.is_none()
  }

  pub fn is_empty(&self) -> bool {
    self.statements.is_empty()
  }

  pub fn orig_block_id(&self) -> BlockId {
    self.orig_block_id
  }

  pub fn output_model(&self) -> &ModelCell {
    &self.output_model
  }

  pub fn seek(&mut self, block: BlockId) {
    assert!(self.curr_block_id.is_none());
    assert_eq!(self.statements.len(), 0);
    self.curr_block_id = Some(block);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl Cursor {
  pub fn const_int(&mut self, ty: Link<mir::TypeInt>, value: i64)
    -> RValLinkOpt
  {
    let link = self.build_const(mir::ConstInt { ty, value });
    Some(link.up())
  }

  pub fn ctl_br<B>(&mut self, block: B) where
    B: ToNodeId<Node=Block>
  {
    let block = block.to_node_id().into();
    self.build_ctl(mir::CtlBr { block });
  }

  pub fn ctl_if<B1, B0>(&mut self, cond: RValLink, block1: B1, block0: B0) where
    B1: IntoBlockId, B0: IntoBlockId
  {
    let block1 = block1.into_block_id().into();
    let block0 = block0.into_block_id().into();
    self.build_ctl(mir::CtlIf { cond, block1, block0 });
  }

  pub fn ctl_ret(&mut self, value: RValLinkOpt) {
    self.build_ctl(mir::CtlRet { value });
  }

  pub fn ctl_switch(
    &mut self,
    value         : RValLink,
    cases         : Vec<(ConstLink, BlockRef)>,
    default_case  : BlockRef
  ) {
    self.build_ctl(mir::CtlSwitch { value, cases, default_case });
  }

  pub fn lval_deref(&mut self, value: RValLink) -> LValLink {
    self.build_lval(mir::LValDeref { value })
  }

  pub fn lval_member(&mut self, base: LValLink, path: Vec<u32>) -> LValLink {
    self.build_lval(mir::LValMember { base, path })
  }

  pub fn rval_member(&mut self, base: RValLink, path: Vec<usize>) -> RValLink {
    self.build_rval(mir::RValMember { base, path })
  }

  pub fn stmt_call(&mut self, target: RValLink, args: Vec<RValLink>) -> RValLink {
    // TODO: check target return type
    self.build_value(mir::StmtCall { target, args })
  }

  pub fn stmt_fat_ptr(&mut self, data: RValLink, vtable: RValLink) -> RValLink {
    type FatPtr = (usize, usize);
    let vtable_ty = quote_type::<FatPtr>(&self.output_model);
    let values = vec![data, vtable];
    self.stmt_struct(vtable_ty.cast(), values)
  }

  pub fn stmt_op_bin(
    &mut self,
    op    : StmtOpBinType,
    left  : RValLink,
    right : RValLink,
  ) -> RValLink {
    self.build_value(mir::StmtOpBin { op, left, right })
  }

  pub fn stmt_set(&mut self, target: LValLink, value: RValLink)  {
    self.build_stmt(mir::StmtSet { target, value });
  }

  pub fn stmt_struct(
    &mut self, ty: Link<mir::TypeStruct>, values: Vec<RValLink>
  ) -> RValLink {
    self.build_value(mir::StmtStruct { ty, values })
  }

  pub fn stmt_use(&mut self, lvalue: LValLink) -> RValLink {
    self.build_value(mir::StmtUse { lvalue })
  }

  pub fn quote_const<T: Serialize>(&self, value: &T) -> RValLink {
    let mut model = self.output_model.borrow_mut();
    quote_value(&mut *model, value).unwrap()
  }
}

impl ToNodeId for Cursor {
  type Node = Block;

  fn to_node_id(&self) -> NodeId<Block> {
    self.orig_block_id()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[macro_export]
macro_rules! builtin_fn {
  ($ctx:expr, $fun:expr, $arity:tt) => {
    $ctx.build_rval(mir::RValBuiltinFn {
      rust_fn: quote_fn!($fun, $arity)
    })
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
