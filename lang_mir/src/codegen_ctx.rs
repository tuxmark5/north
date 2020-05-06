use {
  crate::{
    cursor::Cursor,
    mir::{self, BlockId, Type, RValLink},
  },
  north_core::{
    Node, NodeId,
    model::{Link, ModelCell},
    quote::QuoteType,
  },
  north_gen::{
    ReduceCtxCore,
    Reduction,
  },
  std::marker::Unsize
};

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone)]
pub struct CodegenCtx {
  pub output_model: ModelCell,
  pub mod_id: Option<NodeId<mir::Mod>>,
  pub func_id: Option<NodeId<dyn mir::Fn>>,
}

impl CodegenCtx {
  pub fn new(output_model: ModelCell) -> Self {
    Self {
      output_model,
      mod_id: None,
      func_id: None,
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait CodegenCtxExt<'a, R> where
  R: Reduction<InnerCtx = CodegenCtx>
{
  fn context(&self) -> &ReduceCtxCore<R>;
  fn context_mut(&mut self) -> &mut ReduceCtxCore<'a, R>;

  fn block(&self) -> BlockId {
    let curr_fn_id = self.curr_fn_id();
    let mut out_model = self.output_model().borrow_mut();
    let curr_fn_cell = out_model.node_cell(curr_fn_id);
    let mut curr_fn = curr_fn_cell.borrow_mut();
    curr_fn.add_block(&mut *out_model)
  }

  fn block_cur(&self) -> Cursor {
    let block_id = self.block();
    Cursor::new(self.output_model().clone(), block_id)
  }

  fn block_start(&self, cur: &mut Cursor) -> Cursor {
    if !cur.is_completed() && cur.is_empty() {
      Cursor::new_move(cur)
    } else {
      let start = self.block_cur();
      cur.ctl_br(&start);
      start
    }
  }

  fn curr_fn_id(&self) -> NodeId<dyn mir::Fn> {
    self.inner().func_id.expect("context must have func_id during block creation")
  }

  /*fn curr_mod_cell(&self) -> NodeCell<mir::Mod> {
    let curr_mod_id = self.curr_mod_id();
    self.output_model().node_cell(curr_mod_id)
  }*/

  fn curr_mod_id(&self) -> NodeId<mir::Mod> {
    self.inner().mod_id.expect("context must have mod_id during item creation")
  }

  fn inner(&self) -> &CodegenCtx {
    &self.context().inner
  }

  /*fn inner_mut(&mut self) -> &mut CodegenCtx {
    &mut self.context_mut().inner
  }*/

  fn make_item<N: mir::Item>(&self, node: N) -> NodeId<N> {
    let mut out_model = self.output_model().borrow_mut();
    let item_id = out_model.build_node(node);

    let current_mod_id = self.curr_mod_id();
    let mut current_mod = out_model.node_mut(current_mod_id);
    current_mod.items.push(item_id.up().into());

    item_id
  }

  fn make_local(&self, ty: Link<dyn Type>) -> RValLink { // LValLink?
    let curr_fn_id = self.curr_fn_id();
    let mut out_model = self.output_model().borrow_mut();
    let curr_fn_cell = out_model.node_cell(curr_fn_id);
    let mut curr_fn = curr_fn_cell.borrow_mut();
    curr_fn.add_local(&mut *out_model, ty)
  }

  fn make_node<N: Node>(&self, node: N) -> NodeId<N> {
    let mut out_model = self.output_model().borrow_mut();
    out_model.build_node(node)
  }

  fn make_node_up<A, B>(&self, node: A) -> NodeId<B> where
    A: Node + Unsize<B>, B: Node + ?Sized
  {
    self.make_node::<A>(node).up()
  }

  fn make_param(&self, ty: Link<dyn Type>) -> RValLink {
    let curr_fn_id = self.curr_fn_id();
    let mut out_model = self.output_model().borrow_mut();
    let curr_fn_cell = out_model.node_cell(curr_fn_id);
    let mut curr_fn = curr_fn_cell.borrow_mut();
    curr_fn.add_param(&mut *out_model, ty)
  }

  fn output_model(&self) -> &ModelCell {
    &self.context().inner.output_model
  }

  fn quote_type<T: QuoteType>(&self) -> Link<dyn Type> {
    crate::quote_type::<T>(self.output_model())
  }

  fn set_func_id(&mut self, func_id: NodeId<dyn mir::Fn>) {
    self.context_mut().inner.func_id = Some(func_id);
  }

  fn set_mod_id(&mut self, mod_id: NodeId<mir::Mod>) {
    self.context_mut().inner.mod_id = Some(mod_id);
  }
}

impl<'a, R> CodegenCtxExt<'a, R> for ReduceCtxCore<'a, R> where
  R: Reduction<InnerCtx = CodegenCtx>
{
  fn context(&self) -> &ReduceCtxCore<R> { self }
  fn context_mut(&mut self) -> &mut ReduceCtxCore<'a, R> { self }
}

////////////////////////////////////////////////////////////////////////////////////////////////
