use {
  std::{
    marker::PhantomData,
    ops::{BitAnd, BitOr, BitOrAssign},
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub macro bit_flags {
  ($($args:tt)*) => {
    bit_flags_impl!(__top, $($args)*);
  }
}

pub macro bit_flags_impl {
  (__top, ) => { },

  (__top, $vis:vis enum $name:ident, $tag:ident: $ty:ty { $($body:tt)* } $($rest:tt)*) => {
    bit_flags_impl!(__top_impl, $vis, $name, $tag, $ty, $($body)*);
    bit_flags_impl!(__top, $($rest)*);
  },

  (__top_impl, $vis:vis, $name:ident, $tag:ident, $ty:ty, $($body:tt)*) => {
    $vis type $name = Flags<$tag>;

    impl Flags<$tag> {
      bit_flags_impl!(__impl, $tag, $($body)*);
    }

    $vis enum $tag { }
    impl FlagsTag for $tag {
      type Value = $ty;
    }
  },

  (__impl, $tag:ty, ) => { },

  (__impl, $tag:ty, $vis:vis $name:ident = $bit:expr, $($rest:tt)*) => {
    $vis const $name: Flags<$tag> = Flags::new($bit);
    bit_flags_impl!(__impl, $tag, $($rest)*);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Flags<T: FlagsTag> {
  value: T::Value,
  tag_type: PhantomData<T>
}

impl<T> Flags<T> where
  T: FlagsTag
{
  pub const fn new(value: T::Value) -> Self {
    Self { value, tag_type: PhantomData }
  }

  pub fn is_non_zero(&self) -> bool {
    self.value != T::Value::from(0u8)
  }

  pub fn is_zero(&self) -> bool {
    self.value == T::Value::from(0u8)
  }
}

impl<T> BitAnd for Flags<T> where
  T: FlagsTag, T::Value: BitAnd<Output = T::Value> 
{
  type Output = Self;

  fn bitand(self, rhs: Self) -> Self {
    Self::new(self.value & rhs.value)
  }
}


impl<T> BitOr for Flags<T> where
  T: FlagsTag, T::Value: BitOr<Output = T::Value>
{
  type Output = Self;

  fn bitor(self, rhs: Self) -> Self {
    Self::new(self.value | rhs.value)
  }
}

impl<T> BitOrAssign for Flags<T> where
  T: FlagsTag, T::Value: BitOrAssign
{
  fn bitor_assign(&mut self, rhs: Self) {
    self.value |= rhs.value;
  }
}

impl<T> Clone for Flags<T> where
  T: FlagsTag, T::Value: Clone
{ 
  fn clone(&self) -> Self {
    Self {
      value: self.value.clone(),
      tag_type: PhantomData
    }
  }
}

impl<T> Copy for Flags<T> where
  T: FlagsTag, T::Value: Copy { }

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait FlagsTag: 'static {
  type Value: Clone + Copy + Eq + From<u8>;

  fn bit_name(_index: u32) -> &'static str {
    "MISSING"
  }
} 

////////////////////////////////////////////////////////////////////////////////////////////////
