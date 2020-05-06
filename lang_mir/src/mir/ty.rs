use {
  crate::{
    layout::Layout,
  },
  north_core::{
    model::{Link, Model, ModelCell},
    prelude::*,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait Type: Node {
  fn layout_of(&self, model: &Model) -> Layout;
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct TypeArray {
  pub value_ty: Link<dyn Type>,
  pub elem_count: usize,
}

impl Type for TypeArray {
  fn layout_of(&self, model: &Model) -> Layout {
    let value_ty = model.node(&self.value_ty);
    let value_size = value_ty.layout_of(model);
    value_size.mul(self.elem_count)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct TypeInt {
  pub bit_width: u32,
}

impl Type for TypeInt {
  fn layout_of(&self, _model: &Model) -> Layout {
    let size = (self.bit_width / 8) as usize;
    Layout::new(size, size)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct TypeFn {
  pub param_tys: Vec<Link<dyn Type>>,
  pub result_ty: Link<dyn Type>
}

impl Type for TypeFn {
  fn layout_of(&self, _model: &Model) -> Layout {
    Layout::new(8, 8)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct TypePtr {
  pub target_ty: Link<dyn Type>,
}

impl Type for TypePtr {
  fn layout_of(&self, _model: &Model) -> Layout {
    Layout::new(8, 8)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct TypeStruct {
  pub elements: Vec<Link<dyn Type>>,
}

impl Type for TypeStruct {
  fn layout_of(&self, model: &Model) -> Layout {
    model.nodes(&self.elements)
      .map(|t| t.layout_of(model))
      .fold(Layout::new(0, 1), Layout::seq)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct TypeUnit { }

impl Type for TypeUnit {
  fn layout_of(&self, _model: &Model) -> Layout {
    Layout::new(0, 1)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

concept_map_part! {
  ConceptMapTypePart {
    TypeArray   => {dyn Type},
    TypeInt     => {dyn Type},
    TypeFn      => {dyn Type},
    TypePtr     => {dyn Type},
    TypeStruct  => {dyn Type},
    TypeUnit    => {dyn Type},
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub fn layout_of<I>(model_cell: &ModelCell, node_id: I) -> Layout where
  I: ToNodeId, I::Node: Type
{
  let model = model_cell.borrow();
  let node = model.node(node_id);
  node.layout_of(&*model)
}

////////////////////////////////////////////////////////////////////////////////////////////////
