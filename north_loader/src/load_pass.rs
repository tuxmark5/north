use {
  SourceCodeNode, SourceFileNode,
  loader_aspect::LoaderAspect,
  north_core::{
    compiler::{pass, Compiler, Pass},
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct LoadPass { }

impl LoadPass {
  pub fn new() -> Self {
    Self { }
  }
}

impl Pass for LoadPass {
  fn execute(&self, comp: &Compiler) -> pass::Result {
    let loader_aspect = comp.aspect_mut::<LoaderAspect>();
    let mut model = comp.model_cell.borrow_mut();

    model.reduce_nodes::<SourceFileNode, _>(|model, node| {
      let file_map = loader_aspect.load_file(&node.source_file)?;
      let new_node = SourceCodeNode { file_map };
      let node_id = model.build_node(new_node);
      Some(node_id.cast())
    })
  }

  fn name(&self) -> &str {
    "load"
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
