use {
  crate::{
    pos::Span,
    util::{
      cast::{CastKind, CastRef, DefaultCaster, dynamic_cast},
    },
  },
  ::std::{
    self,
    any::{Any, TypeId},
    fmt::{self, Debug},
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone)]
pub struct Token<T: ?Sized> {
  pub span: Span,
  pub data: T,
}

impl<T: TokenExt> Token<T> {
  pub fn new(data: T) -> Self {
    Self {
      span: Span::empty(),
      data,
    }
  }

  pub fn into_opaque(self) -> TokenOpaque {
    box self
  }
}

impl<T: TokenExt + ?Sized> Token<T> {
  pub fn token_type(&self) -> TypeId {
    self.data.type_id()
  }

  pub fn set_span(&mut self, span: Span) {
    self.span = span;
  }
}

impl Token<dyn TokenExt> {
  pub fn downcast<T: 'static>(self: TokenOpaque) -> Result<Token<T>, TokenOpaque> {
    dynamic_cast::<_, Box<Token<T>>>(self).map(|t| *t)
  }

  pub fn downcast_ref<T: 'static>(&self) -> Option<&Token<T>> {
    dynamic_cast::<_, &Token<T>>(self).ok()
  }

  pub fn is_a<T: 'static>(&self) -> bool {
    self.downcast_ref::<T>().is_some()
  }
}

impl<T, R> AsRef<R> for Token<T> where
  T: AsRef<R>, R: ?Sized
{
  fn as_ref(&self) -> &R {
    self.data.as_ref()
  }
}

impl<A, B> CastRef<Token<A>, Token<B>> for DefaultCaster where
  A: ?Sized, B: ?Sized, DefaultCaster: CastRef<A, B>
{
  fn cast_ref<'a>(&self, value: &Token<A>) -> Option<CastKind> {
    CastRef::<A, B>::cast_ref(self, &value.data)
  }
}

impl<T> Debug for Token<T> where
  T: Debug + ?Sized
{
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "<{:?}>", &self.data)?;
    Ok(())
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait TokenExt: Any + Debug {
  fn name(&self) -> &'static str;
}

impl<T> TokenExt for T where
  T: 'static + Any + Debug
{
  fn name(&self) -> &'static str {
    std::intrinsics::type_name::<T>()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub type TokenOpaque = Box<Token<dyn TokenExt>>;

////////////////////////////////////////////////////////////////////////////////////////////////
