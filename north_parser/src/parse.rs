use {
  crate::{
    ParseCtx,
  },
  north_core::{
    prelude::*,
  },
  std::{
    marker::PhantomData,
    rc::Rc,
    result
  }
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait Parse<N> where
  N: Node + ?Sized
{
  const LEFT_RECURSIVE: bool = false;

  fn parse(&self, ctx: &mut ParseCtx) -> Result<NodeId<N>>;
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait ParseDyn {
  fn is_left_recursive(&self) -> bool;
  fn parse_dyn(&self, ctx: &mut ParseCtx) -> Result<NodeId>;
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct ParseDynAdapter<R, N> where
  N: ?Sized
{
  rule: R,
  node_type: PhantomData<N>
}

impl<R, N> ParseDynAdapter<R, N> where
  R: 'static + Parse<N>, N: Node + ?Sized
{
  pub fn wrap(rule: R) -> Rc<dyn ParseDyn> {
    Rc::new(Self {
      rule,
      node_type: PhantomData,
    })
  }
}

impl<R, N> ParseDyn for ParseDynAdapter<R, N> where
  R: Parse<N>, N: Node + ?Sized
{
  fn is_left_recursive(&self) -> bool {
    R::LEFT_RECURSIVE
  }

  fn parse_dyn(&self, ctx: &mut ParseCtx) -> Result<NodeId> {
    self.rule.parse(ctx).map(|id| id.cast())
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub type Result<T> = result::Result<T, ()>;

////////////////////////////////////////////////////////////////////////////////////////////////

#[macro_export]
macro_rules! parse_part {
  ($part:ident { $($nt:ty => $body:tt,)* }) => {
    pub struct $part;

    impl AspectPart for $part {
      type Aspect = ParseAspect;

      fn setup(&self, aspect: &mut ParseAspect) {
        $( parse_part_add_trait!(aspect, $nt, $body); )*
      }
    }
  }
}

#[macro_export]
macro_rules! parse_part_add_trait {
  ($aspect:expr, $nt:ty, { $($node_ty:ty => $prec:expr; $rule:expr,)* }) => {
    $( $aspect.add_rule::<$nt, $node_ty, _>($prec, $rule); )*
  };

  ($aspect:expr, $nt:ty, ($rule:expr)) => {
    $aspect.add_rule::<$nt, $nt, _>(100, $rule);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[macro_export]
macro_rules! parse_rules {
  ($($spec:tt $body:tt)*) => {
    $(parse_rules_impl!(__impl $spec $body);)*
  }
}

#[macro_export]
macro_rules! parse_rules_impl {
  (__impl [($($args:tt)*), $node_ty:ty => $rule_ty:ty] { $($body:tt)* }) => {
    impl<$($args)*> Parse<$node_ty> for $rule_ty {
      parse_rules_impl!(__member $node_ty, $($body)*);
    }
  };

  (__impl [$node_ty:ty => $rule_ty:ty] { $($body:tt)* }) => {
    impl Parse<$node_ty> for $rule_ty {
      parse_rules_impl!(__member $node_ty, $($body)*);
    }
  };

  (__member $node_ty:ty,) => {
    // tail
  };

  (__member $node_ty:ty,
    const LEFT_RECURSIVE: bool = $value:expr; $($rest:tt)*
  ) => {
    const LEFT_RECURSIVE: bool = $value;
    parse_rules_impl!(__member $node_ty, $($rest)*);
  };

  (__member $node_ty:ty,
    fn parse($self:ident, $ctx:ident) { $($body:tt)* } $($rest:tt)*
  ) => {
    fn parse(&self, $ctx: &mut ParseCtx) -> Result<NodeId<$node_ty>> { $($body)* }
    parse_rules_impl!(__member $node_ty, $($rest)*);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[macro_export]
macro_rules! parse_r {
  (__add, $aspect:expr, [$trait_ty:ty, $node_ty:ty => $prec:expr, $rule:expr]) => {
    if let Some(aspect) = aspect_cast::<ParseAspect>($aspect) {
      aspect.add_rule::<$trait_ty, $node_ty, _>($prec, $rule);
    }
  };

  (__const, LEFT_RECURSIVE,
    ($node_ty:ty),
    $value:expr
  ) => {
    const LEFT_RECURSIVE: bool = $value;
  };

  (__method, parse,
    ($node_ty:ty),
    ($self:ident, $ctx:ident),
    (), $body:tt
  ) => {
    fn parse(&self, $ctx: &mut ParseCtx) -> Result<NodeId<$node_ty>> $body
  };

  (__trait, ($($generics:tt)*), ($($params:tt)*), $target:ty, $body:tt) => {
    impl $($generics)* Parse<$($params)*> for $target $body
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
