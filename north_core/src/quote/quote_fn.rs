use {
  crate::{
    quote::{
      QuoteType, TypeId,
      quote_type_val
    },
  },
  std::{
    mem::transmute,
  }
};

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Debug)]
pub struct RustFn {
  pub fn_type: TypeId,
  pub raw_ptr: *const (),
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait QuoteFn: QuoteType { }

macro_rules! impl_quote_fn {
  ($name:ident, $($param:ident),*) => {
    impl<$($param,)* R> QuoteFn for extern fn($($param),*) -> R where
      $($param: QuoteType,)* R: QuoteType { }

    pub fn $name<$($param,)* R>(f: extern fn($($param),*) -> R) -> RustFn where
      $($param: QuoteType,)* R: QuoteType
    {
      RustFn {
        fn_type: quote_type_val(&f),
        raw_ptr: unsafe { transmute::<_, *const ()>(f) },
      }
    }
  }
}

impl_quote_fn!(quote_fn_0,);
impl_quote_fn!(quote_fn_1, A);
impl_quote_fn!(quote_fn_2, A, B);
impl_quote_fn!(quote_fn_3, A, B, C);
impl_quote_fn!(quote_fn_4, A, B, C, D);
impl_quote_fn!(quote_fn_5, A, B, C, D, E);

////////////////////////////////////////////////////////////////////////////////////////////////

#[macro_export]
macro_rules! quote_fn {
  ($fun:expr, 0) => { quote_fn_0($fun) };
  ($fun:expr, 1) => { quote_fn_1($fun) };
  ($fun:expr, 2) => { quote_fn_2($fun) };
  ($fun:expr, 3) => { quote_fn_3($fun) };
  ($fun:expr, 4) => { quote_fn_4($fun) };
  ($fun:expr, 5) => { quote_fn_5($fun) };
}

////////////////////////////////////////////////////////////////////////////////////////////////
