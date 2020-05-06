use {
  crate::{
    Node, NodeId,
    iter::ChildrenRaw,
    structure::{storage::StorageExt},
    trait_manager::TraitManagerExt,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Forest {
  pub nodes: Vec<NodeData>
}

impl Forest {
  pub fn new() -> Forest {
    let dummy = NodeData::default();
    Forest {
      nodes: vec![dummy],
    }
  }

  pub fn add_child(&mut self, parent: NodeId, link_id: usize, child: NodeId) {
    let data = self.node_mut(child);
    data.parent = Some(parent);
    data.parent_link = link_id as u32;
  }

  pub fn attach_sibling(&mut self, anchor: NodeId, sibling: NodeId) {
    let parent = self.node(anchor).parent;
    let sibling_node = self.node_mut(sibling);

    if parent.is_none() {
      panic!("cannot attach a sibling to a node with no parent");
    }

    if sibling_node.parent.is_none() {
      sibling_node.parent = parent;
    } else {
      panic!("cannot attach a sibling that is already attached");
    }
  }

  // ForestMutExt
  pub fn insert_sibling_after(&mut self, anchor: NodeId, sibling: NodeId) {
    let next_sibling = self.node(anchor).next_sibling;
    self.attach_sibling(anchor, sibling);
    self.link_siblings(Some(anchor), sibling, next_sibling);
  }

  pub fn insert_sibling_before(&mut self, anchor: NodeId, sibling: NodeId) {
    let prev_sibling = self.node(anchor).prev_sibling;
    self.attach_sibling(anchor, sibling);
    self.link_siblings(prev_sibling, sibling, Some(anchor));
  }

  pub fn link_children(&mut self, node_id: NodeId, node: &dyn Node) {
    let mut prev_sibling = None;
    for (link_id, child_id) in ChildrenRaw::new(node) {
      self.add_child(node_id, link_id, child_id);
      self.link_siblings(prev_sibling, child_id, None);
      prev_sibling = Some(child_id);
    }
  }

  pub fn link_siblings(&mut self, a: Option<NodeId>, b: NodeId, c: Option<NodeId>) {
    if let Some(id_a) = a {
      let node_a = self.node_mut(id_a);
      node_a.next_sibling = Some(b);
    } else {
      // firstcomplete_node
    }

    if let Some(id_c) = c {
      let node_c = self.node_mut(id_c);
      node_c.prev_sibling = Some(b);
    } else {
      // last
    }

    {
      let node_b = self.node_mut(b);
      node_b.prev_sibling = a;
      node_b.next_sibling = c;
    }
  }

  pub fn new_node<N: Node>(&mut self) -> NodeId<N> {
    let node_id = NodeId::<N>::from_id(self.nodes.len() as u32);
    self.nodes.push(NodeData::new());
    node_id
  }

  pub fn node(&self, id: NodeId) -> &NodeData {
    &self.nodes[id.into(): usize]
  }

  pub fn node_mut(&mut self, id: NodeId) -> &mut NodeData {
    &mut self.nodes[id.into(): usize]
  }

  pub fn parent(&self, id: NodeId) -> Option<NodeId> {
    self.node(id).parent
  }
}

impl Default for Forest {
  fn default() -> Self {
    Self::new()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait ForestExt: Sized + StorageExt<dyn Node> + TraitManagerExt {
  fn forest(&self) -> &Forest;
  fn forest_mut(&mut self) -> &mut Forest;

  fn new_node<N: Node>(&mut self) -> NodeId<N> {
    self.forest_mut().new_node()
  }

  fn next_sibling_id(&self, id: NodeId) -> Option<NodeId> {
    self.forest().node(id).next_sibling
  }

  fn parent_id(&self, id: NodeId) -> Option<NodeId> {
    self.forest().node(id).parent
  }

  fn parent_link_id(&self, id: NodeId) -> usize {
    self.forest().node(id).parent_link as usize
  }

  fn prev_sibling_id(&self, id: NodeId) -> Option<NodeId> {
    self.forest().node(id).prev_sibling
  }

  fn root_id(&self, mut node: NodeId) -> NodeId {
    loop {
      match self.parent_id(node) {
        Some(parent) => { node = parent },
        None => break node
      }
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Default)]
pub struct NodeData {
  pub parent: Option<NodeId>,
  pub parent_link: u32,
  pub next_sibling: Option<NodeId>,
  pub prev_sibling: Option<NodeId>,
}

impl NodeData {
  pub fn new() -> Self {
    Self {
      parent: None,
      parent_link: 0u32,
      next_sibling: None,
      prev_sibling: None,
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
