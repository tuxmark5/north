use {
  crate::{
    collections::IndexMap,
  },
  std::{
    borrow::Borrow,
    cell::{Ref, RefCell},
    hash::Hash,
    marker::PhantomData,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Interner<T, Idx> {
  crate entries: RefCell<IndexMap<T, ()>>,
  crate index_type: PhantomData<Idx>,
}

impl<T, Idx> Interner<T, Idx> where
  T: Eq + Hash, Idx: From<usize> + Into<usize>
{
  pub fn new() -> Self {
    Self::default()
  }

  pub fn get<R>(&self, index: Idx) -> Ref<R> where
    T: Borrow<R>, R: ?Sized
  {
    let entries = self.entries.borrow();
    Ref::map(entries, |e| {
      let value = e.get_index(index.into()).unwrap().0;
      value.borrow()
    })
  }

  pub fn intern<A>(&self, value: A) -> Idx where
    A: ToOwned<Owned=T>, T: Borrow<A>
  {
    let mut entries = self.entries.borrow_mut();
    let entry = entries.entry(value.to_owned());
    let index = entry.index();
    entry.or_insert(());
    Idx::from(index)
  }

  pub fn len(&self) -> usize {
    let entries = self.entries.borrow();
    entries.len()
  }
}

impl<T, Idx> Default for Interner<T, Idx> where
  T: Eq + Hash, Idx: From<usize> + Into<usize>
{
  fn default() -> Self {
    Self { 
      entries: <_>::default(),
      index_type: PhantomData,
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

