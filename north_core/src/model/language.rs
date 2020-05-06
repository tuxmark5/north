use {
  crate::{
    compiler::aspect::AspectPartDyn,
    model::concept::Concept,
    trait_manager::TraitManager,
  },
  std::iter,
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait AspectParts {
  fn aspect_parts(&self) -> &[&dyn AspectPartDyn] { &[] }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait ConceptMap {
  fn concepts(&self) -> Box<dyn Iterator<Item=&dyn Concept>> { box iter::empty() }
  fn setup_traits(&self, _tm: &mut TraitManager) { }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait Language: AspectParts {
  fn name(&self) -> &str {
    "undefined"
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[macro_export]
macro_rules! language_parts {
  ($lang_ty:ty { $($parts:expr,)* }) => {
    impl $crate::macro_prelude::AspectParts for $lang_ty {
      fn aspect_parts(&self) -> &[&dyn $crate::macro_prelude::AspectPartDyn]  {
        let parts: &'static [&'static dyn $crate::macro_prelude::AspectPartDyn] = &[
          $(&$parts),*
        ];
        parts
      }
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
