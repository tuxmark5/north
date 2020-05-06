use {
  north_core::util::token::{Token, TokenOpaque},
  std::fmt::Debug,
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait Keyword: 'static + Clone + Debug + Default {
  const REPR: &'static str;
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait KeywordDyn: Send + Sync {
  fn repr(&self) -> &'static str;
  fn to_token(&self) -> TokenOpaque;
}

impl<K> KeywordDyn for K where
  K: Keyword + Send + Sync
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
macro_rules! define_keyword {
  ($name:ident, $keyword:expr) => {
    #[derive(Clone, Debug, Default)]
    pub struct $name;

    impl $crate::Keyword for $name {
      const REPR: &'static str = $keyword;
    }
  }
}

#[macro_export]
macro_rules! define_keywords {
  ($list:ident { $($name:ident => $keyword:expr,)* }) => {
    $( define_keyword!($name, $keyword); )*

    pub static $list: &'static [&'static dyn $crate::KeywordDyn] = &[
      $( &$name ),*
    ];
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
