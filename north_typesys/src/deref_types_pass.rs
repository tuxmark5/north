use {
  crate::{
    type_sys_aspect::TypeSysAspect,
  },
  north_core::{
    compiler::{Compiler, Pass, pass},
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct DerefTypesPass { }

impl DerefTypesPass {
  pub fn new() -> Self {
    Self { }
  }
}

impl Pass for DerefTypesPass {
  fn execute(&self, comp: &Compiler) -> pass::Result {
    let aspect = comp.aspect_mut::<TypeSysAspect>();
    aspect.deref_types();
    aspect.deref_types_deep();
    //aspect.dump_types();
    Ok(())
  }

  fn name(&self) -> &str {
    "deref types"
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
