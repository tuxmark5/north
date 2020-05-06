////////////////////////////////////////////////////////////////////////////////////////////////

use {
  stable_deref_trait::StableDeref,
  std::{
    marker::PhantomData,
    mem::transmute,
    ops::{Deref, DerefMut}
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

#[allow(dead_code)]
pub struct Chain<H, T> {
  head: H,
  tail: T,
}

impl<H, T> Chain<H, T> {
  pub fn new<'a, F>(head: H, f: F) -> Self where
    H: 'a + Deref, T: 'a,
    F: FnOnce(&'a <H as Deref>::Target) -> T,
  {
    let tail = unsafe { f(transmute(head.deref())) };
    Self { head, tail }
  }
}

impl<H, T> Deref for Chain<H, T> {
  type Target = T;

  fn deref(&self) -> &T {
    &self.tail
  }
}

impl<H, T> DerefMut for Chain<H, T> {
  fn deref_mut(&mut self) -> &mut T {
    &mut self.tail
  }
}

impl<H, T> Iterator for Chain<H, T> where
  T: Iterator 
{
  type Item = T::Item;

  fn next(&mut self) -> Option<Self::Item> {
    self.tail.next()
  }
}

unsafe impl<H, T> StableDeref for Chain<H, T> where
  H: StableDeref { }

////////////////////////////////////////////////////////////////////////////////////////////////

#[allow(dead_code)]
pub struct Chain2<'h, H: 'h, T> {
  head: H,
  head_lifetime: PhantomData<&'h ()>,
  tail: T,
}

impl<'h, H, T> Chain2<'h, H, T> {
  pub fn new<F>(head: H, f: F) -> Self where
    H: Deref, T: 'h,
    F: FnOnce(&'h <H as Deref>::Target) -> T,
  {
    let tail = unsafe { f(transmute(head.deref())) };
    Self { head, head_lifetime: PhantomData, tail }
  }
}

impl<'h, H, T> Deref for Chain2<'h, H, T> {
  type Target = T;

  fn deref(&self) -> &T {
    &self.tail
  }
}

impl<'h, H, T> DerefMut for Chain2<'h, H, T> {
  fn deref_mut(&mut self) -> &mut T {
    &mut self.tail
  }
}

impl<'h, H, T> Iterator for Chain2<'h, H, T> where
  T: Iterator 
{
  type Item = T::Item;

  fn next(&mut self) -> Option<Self::Item> {
    self.tail.next()
  }
}

unsafe impl<'h, H, T> StableDeref for Chain2<'h, H, T> where
  H: StableDeref { }


////////////////////////////////////////////////////////////////////////////////////////////////

#[allow(dead_code)]
pub struct ChainRef<H, T> {
  head: H,
  tail: T,
}

impl<H, T> ChainRef<H, T> {
  pub fn new<'a, F>(head: H, f: F) -> Self where
    H: 'a + Deref, T: 'a,
    F: FnOnce(&'a <H as Deref>::Target) -> T,
  {
    let tail = unsafe { f(transmute(head.deref())) };
    Self { head, tail }
  }
}

impl<H, T> Deref for ChainRef<H, T> where
  T: Deref
{
  type Target = T::Target;

  fn deref(&self) -> &T::Target {
    &self.tail
  }
}

impl<H, T> DerefMut for ChainRef<H, T> where
  T: DerefMut
{
  fn deref_mut(&mut self) -> &mut T::Target {
    &mut self.tail
  }
}

unsafe impl<H, T> StableDeref for ChainRef<H, T> where
  H: StableDeref, T: StableDeref { }

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait ChainExt: Deref + Sized {
  fn chain_map<'a, F, R>(self, f: F) -> Chain<Self, R> where
    Self: 'a,
    F: FnOnce(&'a <Self as Deref>::Target) -> R,
    R: 'a
  {
    let tail = unsafe { f(transmute(self.deref())) };
    Chain { head: self, tail }
  }
}

impl<A: StableDeref> ChainExt for A { }

////////////////////////////////////////////////////////////////////////////////////////////////
