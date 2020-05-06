use {
  crate::{
    mir::{self, BlockId, RValLink, TypeLink},
    quote_type::quote_type,
  },
  north_core::{
    NodeId,
    model::{Child, ModelCell},
    node_id::ToNodeId,
    quote::QuoteType,
    structure::ForestExt,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct ItemFnBuilder {
  pub model_cell: ModelCell,
  pub name: String,
  pub blocks: Vec<Child<mir::Block>>,
  pub params: Vec<Child<mir::Param>>,
  pub param_types: Vec<TypeLink>
}

impl ItemFnBuilder {
  pub fn new<T: ToString>(model_cell: ModelCell, name: T) -> Self {
    Self {
      model_cell,
      name: name.to_string(),
      blocks: Vec::new(),
      params: Vec::new(),
      param_types: Vec::new(),
    }
  }

  pub fn block(&mut self) -> BlockId {
    let mut model = self.model_cell.borrow_mut();
    let block_id = model.new_node::<mir::Block>();
    self.blocks.push(block_id.into());
    block_id
  }

  pub fn end(self) -> NodeId<mir::ItemFn> {
    let mut model = self.model_cell.borrow_mut();

    let result_ty = model.build_node(mir::TypeUnit { });

    let fn_ty = model.build_node(mir::TypeFn {
      param_tys: self.param_types,
      result_ty: result_ty.up().into(),
    });

    model.build_node(mir::ItemFn {
      name: self.name,
      fn_ty: fn_ty.into(),
      params: self.params,
      locals: Vec::new(),
      blocks: self.blocks,
    })
  }

  pub fn param<I>(&mut self, id: I) -> RValLink where
    I: ToNodeId<Node=mir::Param>
  {
    let ty = {
      let model = self.model_cell.borrow();
      let node = model.node(id);
      node.ty.clone()
    };
    self.param_add(ty)
  }

  pub fn param_add(&mut self, ty: TypeLink) -> RValLink {
    let mut model = self.model_cell.borrow_mut();

    let param = model.build_node(mir::Param {
      ty: ty.clone(),
      index: self.params.len(),
      is_lval: false
    });

    self.params.push(param.into());
    self.param_types.push(ty);

    param.up().into()
  }

  pub fn param_rval<T: QuoteType>(&mut self) -> RValLink {
    let ty = quote_type::<T>(&self.model_cell).into();
    self.param_add(ty)
  }

  pub fn params<I, E>(&mut self, iter: I) -> Vec<RValLink> where
    I: IntoIterator<Item=E>, E: ToNodeId<Node=mir::Param>
  {
    iter.into_iter()
      .map(|id| self.param(id))
      .collect()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
