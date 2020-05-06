use {
  crate::{
    Node, NodeId,
  },
  std::{
    cell::{Ref, RefCell, RefMut},
    collections::{HashMap, hash_map},
    marker::Unsize,
    rc::Rc,
  }
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Storage<T: ?Sized> {
  pub data: HashMap<NodeId, Rc<RefCell<T>>>
}

impl<T: ?Sized> Storage<T> {
  pub fn new() -> Self {
    Self {
      data: HashMap::new()
    }
  }

  pub fn add_node<N>(&mut self, node: NodeId<N>, data: N) where
    N: Node + Unsize<T>
  {
    let value = Rc::new(RefCell::new(data));
    self.data.insert(node.cast(), value);
  }

  pub fn iter(&self) -> hash_map::Iter<NodeId, Rc<RefCell<T>>> {
    self.data.iter()
  }
}

impl<T: ?Sized> Default for Storage<T> {
  fn default() -> Self {
    Self::new()
  }
}

impl<T: ?Sized> StorageExt<T> for Storage<T> {
  fn storage(&self) -> &Storage<T> { self }
  fn storage_mut(&mut self) -> &mut Storage<T> { self }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait StorageExt<T: ?Sized> {
  fn storage(&self) -> &Storage<T>;
  fn storage_mut(&mut self) -> &mut Storage<T>;

  fn data(&self, node: NodeId) -> Ref<T> {
    match self.storage().data.get(&node) {
      Some(value) => value.borrow(),
      None => panic!("no data for node {:?}", node),
    }
  }

  fn data_cell(&self, node: NodeId) -> Rc<RefCell<T>> {
    self.storage().data[&node].clone()
  }

  fn data_mut(&self, node: NodeId) -> RefMut<T> {
    let value = &self.storage().data[&node];
    value.borrow_mut()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
