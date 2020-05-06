use {
  north_core::util::token::{Token, TokenOpaque},
  std::fmt::Debug,
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait Operator: 'static + Clone + Debug + Default {
  const REPR: &'static str;
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait OperatorDyn: Send + Sync {
  fn repr(&self) -> &'static str;
  fn to_token(&self) -> TokenOpaque;
}

impl<K> OperatorDyn for K where
  K: Operator + Send + Sync
{
  fn repr(&self) -> &'static str {
    K::REPR
  }

  fn to_token(&self) -> TokenOpaque {
    Token::new(K::default()).into_opaque()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[macro_export]
macro_rules! define_operator {
  ($name:ident, $operator:expr) => {
    #[derive(Clone, Debug, Default)]
    pub struct $name;

    impl $crate::Operator for $name {
      const REPR: &'static str = $operator;
    }
  }
}

#[macro_export]
macro_rules! define_operators {
  ($list:ident { $($name:ident => $operator:expr,)* }) => {
    $( define_operator!($name, $operator); )*

    pub static $list: &'static [&'static dyn $crate::OperatorDyn] = &[
      $( &$name ),*
    ];
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
