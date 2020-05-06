use {
  crate::{
    model::member::Member,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait Concept: Send + Sync {
  fn members(&self) -> &[&dyn Member];
  fn name(&self) -> &str;
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[macro_export]
macro_rules! concept_map_part {
  ($part:ident { $($node_ty:ty => $traits:tt,)* }) => {
    pub struct $part;

    impl $crate::macro_prelude::AspectPart for $part {
      type Aspect = $crate::macro_prelude::VoidAspect;

      fn setup_traits(&self, tm: &mut $crate::macro_prelude::TraitManager) {
        $(concept_map_part!(@, tm, $node_ty, $traits);)*
      }
    }
  };

  (@, $tm:expr, $node_ty:ty, { $($trait_ty:ty),* }) => {
    $($tm.register::<$node_ty, $trait_ty>();)*
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
