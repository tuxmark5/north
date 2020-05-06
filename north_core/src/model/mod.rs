////////////////////////////////////////////////////////////////////////////////////////////////

pub mod concept;
pub mod concept_descr;
pub mod element;
pub mod language;
#[macro_use]
pub mod member;
pub mod node;
pub mod printer;
#[macro_use]
pub mod prop_map;
pub mod value;

pub use self::{
  concept::Concept,
  language::Language,
  member::{
    child::Child,
    link::Link,
    reference::Reference,
  },
  node::{NodeCell, NodeCellDyn},
  prop_map::{Prop, PropMap, PropMapExt},
  value::{M},
};

////////////////////////////////////////////////////////////////////////////////////////////////

use {
  crate::{
    Node, NodeId,
    node_id::ToNodeId,
    cell::CellMap,
    iter::{self, ModelIterator},
    structure::{
      Forest, ForestExt, Storage, StorageExt,
    },
    trait_manager::{TraitManager, TraitManagerExt},
    util::{
      cast::Cast,
      chain::ChainRef,
      dynamic_cast
    },
  },
  std::{
    any::TypeId,
    cell::{Ref, RefCell, RefMut},
    ops::Deref,
    rc::Rc,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////
pub type NodeRef<N> = ChainRef<NodeCell<N>, Ref<'static, N>>;
pub type NodeRefMut<N> = ChainRef<NodeCell<N>, RefMut<'static, N>>;
////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Default)]
pub struct Model {
  pub cell_map: CellMap,
  pub forest: Forest,
  pub languages: Vec<Rc<dyn Language>>,
  pub properties: PropMap,
  pub storage: Storage<dyn Node>,
  pub trait_manager: TraitManager
}

impl Model {
  pub fn new() -> Self {
    Self::default()
  }

  pub fn add_language(&mut self, lang: Rc<dyn Language>) {
    for part in lang.aspect_parts() {
      (*part).setup_traits(&mut self.trait_manager);
    }

    self.languages.push(lang);
  }

  pub fn build_node<N: Node>(&mut self, node: N) -> NodeId<N> {
    let node_id = self.forest.new_node();
    self.complete_node(node_id, node);
    node_id
  }

  pub fn cast_id<A, B>(&self, node_id: NodeId<A>) -> Option<NodeId<B>> where
    A: Node + ?Sized, B: Node + ?Sized
  {
    let node = self.data(node_id.cast());
    self.cast::<_, Ref<B>>(node)
      .map(|_| node_id.cast())
      .ok()
  }

  pub fn complete_node<N: Node>(&mut self, id: NodeId<N>, node: N) {
    self.forest.link_children(id.cast(), &node);
    self.storage.add_node(id, node);
  }

  pub fn find_ancestor<I, N: Node>(&self, id: I) -> Option<NodeId<N>> where
    I: ToNodeId
  {
    let node_id = id.to_top();
    self.ancestor_ids(node_id)
      .find_map(|id| self.cast_id::<_, N>(id))
  }

  pub fn get<I, B>(&self, id: I) -> Option<Ref<B>> where
    I: ToNodeId, B: Node + ?Sized
  {
    let node = self.data(id.to_top());
    self.cast(node).ok()
  }

  pub fn node<I>(&self, node_id: I) -> Ref<I::Node> where
    I: ToNodeId
  {
    let node = self.data(node_id.to_node_id().cast());
    self.cast(node).ok().unwrap()
  }

  pub fn node_cell<I>(&self, node_id: I) -> NodeCell<I::Node> where
    I: ToNodeId
  {
    let node = self.data_cell(node_id.to_node_id().cast());
    self.cast(node).ok().unwrap()
  }

  pub fn node_mut<N>(&self, node_id: NodeId<N>) -> RefMut<N> where
    N: Node + ?Sized
  {
    let node = self.data_mut(node_id.cast());
    self.cast(node).ok().unwrap()
  }

  pub fn nodes<I, E>(&self, iter: I) -> impl ModelIterator<Item=Ref<E::Node>> where
    I: IntoIterator<Item=E>, E: ToNodeId
  {
    self.iter(iter).borrow_cast_nodes()
  }

  pub fn nodes_with_id<I, E>(&self, iter: I)
    -> impl Iterator<Item=(NodeId<E::Node>, Ref<E::Node>)> where
    I: IntoIterator<Item=E>, E: ToNodeId
  {
    iter.into_iter().map(move |elem| {
      let node_id = elem.to_node_id();
      let node = self.data(node_id.cast());
      (node_id, self.cast(node).ok().unwrap())
    })
  }

  pub fn reduce_nodes<N, F>(&mut self, mut f: F) -> Result<(), ()> where
    N: Node,
    F: FnMut(&mut Model, &mut N) -> Option<NodeId>
  {
    type NodeCell = Rc<RefCell<dyn Node>>;

    let is_applicable = |node: &NodeCell| -> bool {
      let node_ref = node.borrow();
      (*node_ref).type_id() == TypeId::of::<N>()
      //self.trait_manager.cast::<N>(node_ref).is_some()
    };

    let applicable_nodes = self.storage.iter()
      .filter(|(_, v)| is_applicable(v))
      .map(|(k, v)| (*k, v.clone()))
      .collect::<Vec<(NodeId, NodeCell)>>();

    for (id, cell) in applicable_nodes {
      let mut node_a = cell.borrow_mut();
      let node_a = dynamic_cast::<_, &mut N>(&mut *node_a).ok().unwrap();

      if let Some(new_node_id) = f(self, node_a) {
        self.replace_node(new_node_id, id);
        // println!("node transformed {:?} => {:?}", new_node_id, id);
      } else {
        println!("node transformation failure");
        return Err(());
      }
    }

    Ok(())
  }

  pub fn relink_children<I: ToNodeId>(&mut self, node_id: I) {
    let node_id = node_id.to_top();
    let node = self.storage.data(node_id);
    self.forest.link_children(node_id, &*node);
  }

  pub fn replace_node(&mut self, new_node_id: NodeId, node_id: NodeId) {
    let src_node = self.storage.data.remove(&new_node_id).unwrap();
    self.storage.data.insert(node_id, src_node);
    self.relink_children(node_id);
    // relink_children
  }

  pub fn try_get<I, N>(&self, node_id: I) -> Option<Ref<N>> where
    I: ToNodeId, N: Node + ?Sized
  {
    let node = self.data(node_id.to_top());
    self.cast(node).ok()
  }
}

impl Model {
  pub fn ancestor_ids<I: ToNodeId>(&self, id: I) -> iter::Ancestors {
    iter::Ancestors::new(self, id.to_top())
  }

  pub fn children_ids<I: ToNodeId>(&self, id: I) -> iter::Children {
    iter::Children::new(self, id.to_top())
  }

  pub fn descendant_ids<I: ToNodeId>(&self, id: I) -> iter::Descendants {
    iter::Descendants::new(self, id.to_top())
  }

  pub fn iter<I: IntoIterator>(&self, iter: I) -> iter::Adapter<I::IntoIter> {
    iter::Adapter::new(self, iter.into_iter())
  }

  pub fn parent_id<I: ToNodeId>(&self, id: I) -> Option<NodeId> {
    self.forest().node(id.to_top()).parent
  }
}

impl ForestExt for Model {
  fn forest(&self) -> &Forest { &self.forest }
  fn forest_mut(&mut self) -> &mut Forest { &mut self.forest }
}

impl PropMapExt for Model {
  fn prop_map(&self) -> &PropMap { &self.properties }
  fn prop_map_mut(&mut self) -> &mut PropMap { &mut self.properties }
}

impl StorageExt<dyn Node> for Model {
  fn storage(&self) -> &Storage<dyn Node> { &self.storage }
  fn storage_mut(&mut self) -> &mut Storage<dyn Node> { &mut self.storage }
}

impl TraitManagerExt for Model {
  fn trait_manager(&self) -> &TraitManager { &self.trait_manager }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone)]
pub struct ModelCell(pub Rc<RefCell<Model>>);

impl ModelCell {
  pub fn new() -> Self {
    ModelCell(Rc::new(RefCell::new(Model::new())))
  }
  
  pub fn build_node<N: Node>(&self, node: N) -> NodeId<N> {
    let mut model = self.0.borrow_mut();
    model.build_node(node)
  }

  pub fn cast<A, B>(&self, value: A) -> Result<B, A> where
    TraitManager: Cast<A, B>
  {
    let model = self.0.borrow();
    Cast::cast(&model.trait_manager, value)
  }

  pub fn node_cell<I: ToNodeId>(&self, node_id: I) -> NodeCell<I::Node> {
    let model = self.0.borrow();
    model.node_cell(node_id)
  }

  pub fn node_cell_dyn(&self, node_id: NodeId) -> NodeCellDyn {
    let model = self.0.borrow();
    model.data_cell(node_id)
  }

  pub fn node<I: ToNodeId>(&self, node_id: I) -> NodeRef<I::Node> {
    let cell = self.node_cell(node_id);
    ChainRef::new(cell, |c| c.borrow())
  }

  pub fn node_mut<I: ToNodeId>(&self, node_id: I) -> NodeRefMut<I::Node> {
    let cell = self.node_cell(node_id);
    ChainRef::new(cell, |c| c.borrow_mut())
  }
}

impl Deref for ModelCell {
  type Target = RefCell<Model>;

  fn deref(&self) -> &Self::Target {
    self.0.deref()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
