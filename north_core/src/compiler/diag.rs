use {
  crate::pos::Span,
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub type DiagBox = Box<dyn Diag>;
pub type DiagVec = Vec<DiagBox>;

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait Diag {
  fn level(&self) -> i32 { 0 }
  fn message(&self) -> &str { "<NO MESSAGE>" }
  fn span(&self) -> Option<Span> { None }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait DiagHandler {
  fn add_diag(&mut self, diag: Box<dyn Diag>);
  fn emit_all(&mut self);
}

////////////////////////////////////////////////////////////////////////////////////////////////
/*
#[macro_export]
macro_rules! define_diags {
  ($( $name:ident $fields:tt => $body:tt )*) => {
    $( define_diags!(impl $name $fields => $body) )*;
  };

  (impl $name:ident $fields:tt => { $($key:ident $value:tt),* }) => {
    pub struct $name $fields;

    impl Diag for $name {
      $( define_diags!(field $key $value); )*
    }
  };

  (field level(Error)) => {

  };

  (field message($msg:expr)) => {
    fn message(&self) -> &str { $msg(self) }
  };

  (field with_span($getter:expr)) => {
    fn span(&self) -> Option<Span> { Some($getter(self)) }
  }
}


pub struct UndefinedReferenceError {
  name: String,
  span: Span // store externally?
}

impl Diag for UndefinedReferenceError {
  diag_level!(ERROR);
  diag_span!(span);

  fn message(&self) -> String {
    format!("undefined reference: {}", self.name)
  }
}

define_diags! {
  UndefinedReferenceError(Span) => {
    level(Error),
    message(|_| "undefined reference"),
    with_span(|s: &UndefinedReferenceError| s.0)
  }
}*/

////////////////////////////////////////////////////////////////////////////////////////////////
