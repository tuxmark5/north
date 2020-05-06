use {
  std::{
    any::Any,
    cmp::PartialEq,
    hash::{Hash, Hasher},
  }
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Dyn<T: ?Sized>(pub T);
pub struct DynBox<T: ?Sized>(pub Box<T>);

/*impl<T: ?Sized> Borrow<Dyn<T>> for Box<Dyn<T>> {
  fn borrow(&self) -> &Dyn<T> {
    self.0
  }
}*/

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait EqDyn: PartialEqDyn { }

impl<T: Any + Eq> EqDyn for T { }

impl<T> Eq for Dyn<T> where
  T: AsRef<dyn Any> + EqDyn + ?Sized { }

impl<T> Eq for DynBox<T> where
  T: AsRef<dyn Any> + EqDyn + ?Sized { }

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait HashDyn {
  fn hash_dyn(&self, state: &mut dyn Hasher);
}

impl<T: Any + Hash> HashDyn for T {
  fn hash_dyn(&self, mut state: &mut dyn Hasher) {
    Hash::hash(self, &mut state);
  }
}

impl<T> Hash for Dyn<T> where
  T: HashDyn + ?Sized
{
  fn hash<H: Hasher>(&self, state: &mut H) {
    HashDyn::hash_dyn(&self.0, state);
  }
}

impl<T> Hash for DynBox<T> where
  T: HashDyn + ?Sized
{
  fn hash<H: Hasher>(&self, state: &mut H) {
    HashDyn::hash_dyn(&*self.0, state);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait PartialEqDyn {
  fn eq_dyn(&self, other: &dyn Any) -> bool;
}

impl<T: Any + PartialEq> PartialEqDyn for T {
  fn eq_dyn(&self, other: &dyn Any) -> bool {
    match other.downcast_ref::<T>() {
      Some(other) => self.eq(other),
      None => false,
    }
  }
}

impl<T> PartialEq for Dyn<T> where
  T: AsRef<dyn Any> + PartialEqDyn + ?Sized
{
  fn eq(&self, other: &Dyn<T>) -> bool {
    PartialEqDyn::eq_dyn(&self.0, (other.0).as_ref())
  }
}

impl<T> PartialEq for DynBox<T> where
  T: AsRef<dyn Any> + PartialEqDyn + ?Sized
{
  fn eq(&self, other: &DynBox<T>) -> bool {
    PartialEqDyn::eq_dyn(&*self.0, (*other.0).as_ref())
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
