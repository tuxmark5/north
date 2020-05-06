use {
  crate::{
    model::ModelCell,
    node_id::{NodeId, ToNodeId},
  },
  std::marker::PhantomData
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub mod borrow_cell;
pub mod cell_map;
pub mod compiler_cell;
pub mod macros;
pub mod model_key;
//pub mod small_box;

pub use self::{
  borrow_cell::BorrowCell,
  cell_map::{CellMap, Key, Scope, ToScope},
  compiler_cell::CompilerCell,
  macros::*,
  model_key::ModelKey,
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub mod prelude {
  pub use {
    crate::{
      node_id::NodeId,
    },
    super::{
      Cell, DefaultInit, ManualInit,
      borrow_cell::BorrowCell,
      compiler_cell::CompilerCell,
      macros::*,
      model_key::ModelKey,
    },
    std::{
      cell::RefCell,
      rc::{Rc},
    },
  };
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct DefaultInit;
pub struct ManualInit;

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait Cell {
  type Key;
  type KeyBuilder;
  type Value;

  fn get(&self, key: &Self::Key) -> Self::Value {
    self.try_get(key).unwrap()
  }

  fn key_b(&self) -> Self::KeyBuilder;

  fn set(&self, _key: &Self::Key, _value: Self::Value) {
    unimplemented!()
  }

  fn try_get(&self, key: &Self::Key) -> Option<Self::Value> {
    Some(self.get(key))
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct KeyBuilder<K>(PhantomData<K>);

impl KeyBuilder<()> {
  pub fn make(&self) -> () { () }
}

impl KeyBuilder<NodeId> {
  pub fn make<I: ToNodeId>(&self, id: I) -> NodeId {
    id.to_node_id().cast()
  }
}

impl KeyBuilder<(ModelCell, ())> {
  pub fn make(&self, model_cell: &ModelCell) -> (ModelCell, ()) {
    let model_cell = model_cell.clone();
    (model_cell, ())
  }
}

impl KeyBuilder<(ModelCell, NodeId)> {
  pub fn make<I>(&self, model_cell: &ModelCell, id: I) -> (ModelCell, NodeId) where
    I: ToNodeId
  {
    let model_cell = model_cell.clone();
    let node_id = id.to_node_id().cast();
    (model_cell, node_id)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
