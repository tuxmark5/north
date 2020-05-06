pub mod adapter;
pub mod ancestors;
pub mod borrow_nodes;
pub mod cast_id;
pub mod children;
pub mod descendants;
pub mod filter_cast;

pub use self::{
  adapter::Adapter,
  ancestors::Ancestors,
  borrow_nodes::BorrowNodes,
  cast_id::CastId,
  children::{Children, ChildrenRaw},
  descendants::Descendants,
  filter_cast::FilterCast,
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub type BorrowCastNodes<'m, I, N> = FilterCast<'m, BorrowNodes<'m, I>, Ref<'m, N>>;

////////////////////////////////////////////////////////////////////////////////////////////////

use {
  crate::{
    Model, Node,
    node_id::ToNodeId,
    trait_manager::{TraitManager},
    util::cast::Cast,
  },
  std::{
    cell::Ref,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait ModelIterator<'m>: Iterator {
  fn model(&self) -> &'m Model;

  fn borrow_cast_nodes<N>(self) -> BorrowCastNodes<'m, Self, N> where
    Self: Sized, Self::Item: ToNodeId<Node=N>, N: Node + ?Sized
  {
    self.borrow_nodes().filter_cast::<Ref<N>>()
  }

  fn borrow_cast_nodes_to<N>(self) -> BorrowCastNodes<'m, Self, N> where
    Self: Sized, Self::Item: ToNodeId, N: Node + ?Sized
  {
    self.borrow_nodes().filter_cast::<Ref<N>>()
  }

  fn borrow_nodes(self) -> BorrowNodes<'m, Self> where
    Self: Sized, Self::Item: ToNodeId
  {
    BorrowNodes::<Self>::new(self)
  }

  fn cast_ids<N>(self) -> CastId<'m, Self, N> where
    Self: Sized,  Self::Item: ToNodeId, N: Node + ?Sized
  {
    CastId::<Self, N>::new(self)
  }

  fn filter_cast<T>(self) -> FilterCast<'m, Self, T> where
    Self: Sized, TraitManager: Cast<Self::Item, T>
  {
    FilterCast::<Self, T>::new(self)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

// impl<'m, I, F> ModelIterator<'m> for iter::FilterMap<I, F> where
//   I: ModelIterator<'m>
// {
//   fn model(&self) -> &'m Model {
//     self.iter.model()
//   }
// }

////////////////////////////////////////////////////////////////////////////////////////////////
