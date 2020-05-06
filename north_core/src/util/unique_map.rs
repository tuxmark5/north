use {
  std::{
    collections::HashMap,
    hash::Hash,
    marker::PhantomData,
    rc::Rc,
  }
};

////////////////////////////////////////////////////////////////////////////////////////////////

//pub trait Index: From<usize> + Into<usize> { }

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct UniqueMap<I, V> {
  _index: PhantomData<I>,
  indices: HashMap<Rc<V>, usize>,
  values: Vec<Rc<V>>
}

impl<I, V> UniqueMap<I, V> where
  I: From<usize> + Into<usize>, V: Eq + Hash
{
  pub fn new() -> Self {
    Self {
      _index: PhantomData,
      indices: HashMap::new(),
      values: Vec::new(),
    }
  }

  pub fn get(&self, index: I) -> &Rc<V> {
    self.values.get(index.into()).unwrap()
  }

  pub fn insert_unique(&mut self, value: V) -> I {
    if let Some(index) = self.indices.get(&value) {
      return I::from(*index);
    }

    let cell = Rc::new(value);
    let index = self.values.len();
    self.indices.insert(cell.clone(), index);
    self.values.push(cell);

    I::from(index)
  }

  pub fn len(&self) -> usize {
    self.values.len()
  }
}

impl<I, V> Default for UniqueMap<I, V> where
  I: From<usize> + Into<usize>, V: Eq + Hash
{
  fn default() -> Self {
    Self::new()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
