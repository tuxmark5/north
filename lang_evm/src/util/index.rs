use {
  north_core::{
    quote::{QuoteType, RustQuoter, rust_type},
  },
  serde::{
    Serialize, Serializer,
  },
  std::{
    cmp::Ordering,
    convert::{TryFrom, TryInto},
    fmt::{self, Debug},
    hash::{Hash, Hasher},
    marker::PhantomData,
  }
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Index<I, T> {
  index: I,
  tag_type: PhantomData<T>
}

impl<I, T> Index<I, T> where
  I: Clone + Copy
{
  pub fn from_id<A: Into<I>>(index: A) -> Self {
    Self { 
      index: index.into(),
      tag_type: PhantomData 
    }
  }

  pub fn idx(&self) -> I {
    self.index
  }
}

impl<I: Clone, T> Clone for Index<I, T> { 
  fn clone(&self) -> Self {
    Self {
      index: self.index.clone(),
      tag_type: PhantomData
    }
  }
}

impl<I: Debug, T> Debug for Index<I, T> {
  default fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
    write!(fmt, "{:?}", self.index)
  }
}

impl<I: Copy, T> Copy for Index<I, T> { }

impl<I: Eq, T> Eq for Index<I, T> { }

// impl<I, T> From<I> for Index<I, T> {
//   fn from(index: I) -> Self {
//     Self { 
//       index: index.into(),
//       tag_type: PhantomData 
//     }
//   }
// }

impl<I, T> From<usize> for Index<I, T> where 
  I: TryFrom<usize> 
{
  fn from(index: usize) -> Self {
    Self { 
      index: I::try_from(index).ok().unwrap(),
      tag_type: PhantomData 
    }
  }
}

impl<I, T> From<Index<I, T>> for usize where 
  I: TryInto<usize> 
{
  fn from(index: Index<I, T>) -> Self {
    let result = index.index.try_into();
    result.ok().unwrap()
  }
}

impl<I: Hash, T> Hash for Index<I, T> { 
  fn hash<H: Hasher>(&self, state: &mut H) {
    self.index.hash(state)
  }
}

impl<I: Ord, T> Ord for Index<I, T> { 
  fn cmp(&self, other: &Self) -> Ordering {
    Ord::cmp(&self.index, &other.index)
  }
}

impl<I: PartialEq, T> PartialEq for Index<I, T> {   
  fn eq(&self, other: &Self) -> bool { 
    PartialEq::eq(&self.index, &other.index) 
  }
}

impl<I: PartialOrd, T> PartialOrd for Index<I, T> { 
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    PartialOrd::partial_cmp(&self.index, &other.index)
  }
}

impl<I: QuoteType, T: 'static> QuoteType for Index<I, T> {
  fn quote(q: &mut RustQuoter) -> rust_type::Type {
    I::quote(q)
  }
}

impl<I: Serialize, T> Serialize for Index<I, T> {
  fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
    self.index.serialize(serializer)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait IndexLike {
  fn from_usize(&self) -> usize;
  fn to_usize(&self) -> usize;
}

////////////////////////////////////////////////////////////////////////////////////////////////
