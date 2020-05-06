use {
  crate::{
    compiler::Compiler,
    trait_manager::TraitManager,
    util::dynamic_cast,
  },
  std::any::{Any, TypeId},
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait Aspect: 'static + Any {
  fn create(comp: &Compiler) -> Self where Self: Sized;
  fn name(&self) -> &str;
}

default impl<A> Aspect for A where A: 'static + Default {
  fn create(_comp: &Compiler) -> Self where Self: Sized {
    A::default()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait AspectPart {
  type Aspect: Aspect + ?Sized;

  fn setup(&self, _aspect: &mut Self::Aspect) { }

  fn setup_dyn(&self, _aspect: &mut dyn Aspect) { }

  fn setup_traits(&self, _tm: &mut TraitManager) { }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait AspectPartDyn {
  fn aspect_type(&self) -> TypeId;
  fn setup(&self, aspect: &mut dyn Aspect);
  fn setup_dyn(&self, aspect: &mut dyn Aspect);
  fn setup_traits(&self, tm: &mut TraitManager);
}

impl<A: Aspect, P: AspectPart<Aspect = A>> AspectPartDyn for P {
  fn aspect_type(&self) -> TypeId {
    TypeId::of::<A>()
  }

  fn setup(&self, aspect: &mut dyn Aspect) {
    match dynamic_cast::<_, &mut A>(aspect) {
      Ok(aspect) => self.setup(aspect),
      Err(_) => panic!("bads"),
    }
  }

  fn setup_dyn(&self, aspect: &mut dyn Aspect) {
    AspectPart::setup_dyn(self, aspect)
  }

  fn setup_traits(&self, tm: &mut TraitManager) {
    AspectPart::setup_traits(self, tm)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Default)]
pub struct VoidAspect;

impl Aspect for VoidAspect {
  fn name(&self) -> &str { "void" }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[macro_export]
macro_rules! lang_part {
  (__one, $name:ident, $comp:expr, $spec:tt) => {
    $name!(__add, $comp, $spec);
  };

  ($part:ident { $($name:ident! $spec:tt,)* }) => {
    pub struct $part;

    impl $crate::macro_prelude::AspectPart for $part {
      type Aspect = $crate::macro_prelude::VoidAspect;

      fn setup_dyn(&self, aspect: &mut dyn $crate::macro_prelude::Aspect) {
        $( lang_part!(__one, $name, aspect, $spec); )*
      }
    }
  };
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub fn aspect_cast<A: Aspect>(aspect: &mut dyn Aspect) -> Option<&mut A> {
  dynamic_cast::<_, &mut A>(aspect).ok()
}

////////////////////////////////////////////////////////////////////////////////////////////////
