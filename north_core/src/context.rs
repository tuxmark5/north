use {
  crate::{
    Node, NodeId,
    iter,
    model::{
      Model, ModelCell,
      node::NodeCell,
      prop_map::{Prop, PropMapExt},
    },
    structure::{
      ForestExt, StorageExt,
    },
    util::{
      chain::{Chain2, ChainRef},
      cast::{Cast, DefaultCaster, dynamic_cast}
    },
  },
  std::{
    cell::{Ref, RefCell, RefMut},
    ops::Deref,
    rc::Rc
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait AspectCtxExt<A> {
  fn aspect(&self) -> &A;
  fn aspect_mut(&mut self) -> &mut A;
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait InnerCtxExt<I> {
  fn inner(&self) -> &I;
  fn inner_mut(&mut self) -> &mut I;
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait ModelCellCtxExt {
  fn model_cell(&self) -> &ModelCell;

  fn fetch_node_cell<N>(&self, id: NodeId<N>) -> Option<NodeCell<N>> where
    N: 'static + Node
  {
    let model = self.model_cell().borrow();
    let node_cell = model.data_cell(id.cast());
    dynamic_cast::<_, NodeCell<N>>(node_cell).ok()
  }

  fn fetch_node_owned<'b, N>(&self, id: NodeId<N>) -> ChainRef<NodeCell<N>, Ref<'b, N>> where
    N: 'static + Node
  {
    let cell = self.fetch_node_cell(id).unwrap();
    ChainRef::new(cell, |c| c.borrow())
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

// check_constraints
// check_constrs

pub type AncestorsChain<'a> = Chain2<'a, Ref<'a, Model>, iter::Ancestors<'a>>;
pub type ModelChain = Chain2<'static, ModelCell, Ref<'static, Model>>;

pub trait ModelNodeIdCtxExt: ModelCellCtxExt + NodeIdCtxExt {
  fn ancestor_ids(&self) -> AncestorsChain {
    let node_id = self.node_id();
    Chain2::new(self.model(), |m| m.ancestor_ids(node_id))
  }

  fn find_ancestor_id<N: Node>(&self) -> Option<NodeId<N>> {
    let node_id = self.node_id();
    self.model().find_ancestor::<_, N>(node_id)
  }

  fn lookup_node_cell(&self) -> Rc<RefCell<dyn Node>> {
    let node_id = self.node_id();
    self.model().data_cell(node_id)
  }

  fn model(&self) -> Ref<Model> {
    self.model_cell().borrow()
  }

  fn model_chain(&self) -> ModelChain {
    Chain2::new(self.model_cell().clone(), |m| m.borrow())
  }

  fn model_mut(&self) -> RefMut<Model> {
    self.model_cell().borrow_mut()
  }

  fn model_node_id(&self) -> ModelNodeId {
    ModelNodeId::new(self.model_cell().clone(), self.node_id())
  }

  fn next_sibling_id(&self) -> Option<NodeId> {
    self.model().next_sibling_id(self.node_id())
  }

  fn parent_id(&self) -> Option<NodeId> {
    self.model().parent_id(self.node_id())
  }

  fn prev_sibling_id(&self) -> Option<NodeId> {
    self.model().prev_sibling_id(self.node_id())
  }

  fn prop<P>(&self, _prop: P) -> Option<P::Type> where
    P: Prop, P::Type: Clone
  {
    self.model().prop::<P>(self.node_id()).cloned()
  }

  fn prop_set<P>(&self, prop: P, value: P::Type) where
    P: Prop
  {
    let mut model = self.model_mut();
    model.prop_set(self.node_id(), prop, value)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait NodeIdCtxExt {
  fn node_id(&self) -> NodeId;
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait NodeCellCtxExt<N> where
  N: 'static + ?Sized
{
  fn node_cell(&self) -> &NodeCell<N>;

  fn node(&self) -> Ref<N> {
    self.node_cell().borrow()
  }

  fn node_mut(&self) -> RefMut<N> {
    self.node_cell().borrow_mut()
  }

  fn node_owned<'b>(&self) -> ChainRef<Rc<RefCell<N>>, Ref<'b, N>> {
    ChainRef::new(self.node_cell().clone(), |c| c.borrow())
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone)]
pub struct ModelNodeId {
  pub model_cell: ModelCell,
  pub node_id: NodeId,
}

impl ModelNodeId {
  pub fn new(model_cell: ModelCell, node_id: NodeId) -> Self {
    Self { model_cell, node_id }
  }

  pub fn into_ref(self) -> ModelNodeRef<dyn Node> {
    let node_cell = self.lookup_node_cell();
    ModelNodeRef {
      model_cell: self.model_cell,
      node_id: self.node_id,
      node_cell
    }
  }
}

impl ModelCellCtxExt for ModelNodeId {
  fn model_cell(&self) -> &ModelCell { &self.model_cell }
}

impl NodeIdCtxExt for ModelNodeId {
  fn node_id(&self) -> NodeId { self.node_id }
}

impl ModelNodeIdCtxExt for ModelNodeId { }

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct ModelNodeRef<N> where
  N: 'static + ?Sized
{
  pub model_cell: ModelCell,
  pub node_id: NodeId,
  pub node_cell: NodeCell<N>,
}

impl<N> ModelNodeRef<N> where
  N: ?Sized
{
  pub fn new(model_node_id: ModelNodeId, node_cell: Rc<RefCell<N>>) -> Self {
    Self {
      model_cell: model_node_id.model_cell,
      node_id: model_node_id.node_id,
      node_cell
    }
  }
}

impl<A, B> Cast<ModelNodeRef<A>, ModelNodeRef<B>> for DefaultCaster where
  A: 'static + ?Sized, B: 'static + ?Sized
{
  fn cast(&self, value: ModelNodeRef<A>) -> Result<ModelNodeRef<B>, ModelNodeRef<A>> {
    let node_cell = value.node_cell.clone();
    let node_cell = match self.cast(node_cell) {
      Ok(node_cell) => node_cell,
      Err(_) => return Err(value),
    };

    Ok(ModelNodeRef {
      model_cell: value.model_cell,
      node_id: value.node_id,
      node_cell,
    })
  }
}

impl<N> Clone for ModelNodeRef<N> {
  fn clone(&self) -> Self {
    Self {
      model_cell: self.model_cell.clone(),
      node_id: self.node_id,
      node_cell: self.node_cell.clone(),
    }
  }
}

impl From<ModelNodeId> for ModelNodeRef<dyn Node> {
  fn from(model_node_id: ModelNodeId) -> Self {
    model_node_id.into_ref()
  }
}

impl<N: 'static + ?Sized> ModelCellCtxExt for ModelNodeRef<N> {
  fn model_cell(&self) -> &ModelCell { &self.model_cell }
}

impl<N: 'static + ?Sized> ModelNodeIdCtxExt for ModelNodeRef<N> { }

impl<N: 'static + ?Sized> NodeCellCtxExt<N> for ModelNodeRef<N> {
  fn node_cell(&self) -> &Rc<RefCell<N>> { &self.node_cell }
}

impl<N: 'static + ?Sized> NodeIdCtxExt for ModelNodeRef<N> {
  fn node_id(&self) -> NodeId { self.node_id }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct NodeCtx<'n, N> where
  N: Node + ?Sized
{
  node_id: NodeId,
  node_ref: &'n N,
}

impl<'n, N> Deref for NodeCtx<'n, N> where
  N: Node + ?Sized
{
  type Target = N;

  fn deref(&self) -> &N {
    self.node_ref
  }
}

impl<'n, N> NodeIdCtxExt for NodeCtx<'n, N> where
  N: Node + ?Sized
{
  fn node_id(&self) -> NodeId { self.node_id }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[macro_export]
macro_rules! context_impl {
  (ModelCellCtxExt, $field:ident) => {
    fn model_cell(&self) -> &ModelCell { &self.$field }
  };

  (ModelCellCtxExt, $getter:expr) => {
    fn model_cell(&self) -> &ModelCell { $getter }
  };

  (ModelNodeIdCtxExt) => {
    // empty
  };

  (NodeIdCtxExt, $field:ident) => {
    fn node_id(&self) -> NodeId { self.node_id }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
