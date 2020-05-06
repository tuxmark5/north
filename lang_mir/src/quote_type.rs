use {
  crate::mir,
  north_core::{
    cell::prelude::*,
    model::{Link, Model, ModelCell},
    quote::{
      RUST_TYPE_QUOTER,
      QuoteType, RustQuoter,
      rust_type::*
    },
  },
  north_derive::cell,
  std::{
    cell::RefCell,
    collections::{HashMap},
    ops::DerefMut,
    rc::{Rc, Weak},
  }
};

////////////////////////////////////////////////////////////////////////////////////////////////

#[cell(borrow, scope(model))]
pub static MIR_TYPE_QUOTER: TypeQuoter = ();

////////////////////////////////////////////////////////////////////////////////////////////////

pub type MIRTypeId = NodeId<dyn mir::Type>;

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct TypeQuoter {
  parent_model: Weak<RefCell<Model>>,
  rust_quoter: Rc<RefCell<RustQuoter>>,
  type_map: HashMap<TypeId, MIRTypeId>,
}

impl TypeQuoter {
  pub fn new(model_cell: &ModelCell) -> Self {
    Self {
      parent_model: Rc::downgrade(&model_cell.0),
      rust_quoter: cell_cell!(RUST_TYPE_QUOTER),
      type_map: HashMap::new(),
    }
  }

  pub fn build_type<T>(&mut self, node: T) -> MIRTypeId where
    T: mir::Type
  {
    let model_cell = self.parent_model.upgrade().unwrap();
    let mut model = model_cell.borrow_mut();
    model.build_node(node).up() // UNIQ!!!
  }

  pub fn int_type(&mut self, bit_width: u32) -> MIRTypeId {
    self.build_type(mir::TypeInt { bit_width })
  }

  pub fn quote<T: QuoteType>(&mut self) -> Link<dyn mir::Type> {
    let type_id = {
      let mut rust_quoter = self.rust_quoter.borrow_mut();
      rust_quoter.quote::<T>()
    };

    self.reduce_type(type_id).into()
  }

  pub fn reduce_type(&mut self, type_id: TypeId) -> MIRTypeId {
    if let Some(mir_type) = self.type_map.get(&type_id) {
      return *mir_type;
    }

    let rust_quoter = self.rust_quoter.clone();
    let rust_quoter = rust_quoter.borrow();
    let rust_type = rust_quoter.get(type_id);

    let mir_type = rust_type.reduce_type(self);
    self.type_map.insert(type_id, mir_type);
    mir_type
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait ReduceType {
  fn reduce_type(&self, q: &mut TypeQuoter) -> MIRTypeId;
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl ReduceType for Type {
  fn reduce_type(&self, q: &mut TypeQuoter) -> MIRTypeId {
    match self {
      Type::Prim(p)   => p.reduce_type(q),
      Type::Ptr(p)    => p.reduce_type(q),
      Type::Struct(s) => s.reduce_type(q),
      Type::Tuple(t)  => t.reduce_type(q),
      Type::Unit      => q.build_type(mir::TypeUnit { }),
      _               => unimplemented!("{:?}", self),
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl ReduceType for TypePrim {
  fn reduce_type(&self, q: &mut TypeQuoter) -> MIRTypeId {
    match self {
      TypePrim::Bool  => q.int_type(1),
      TypePrim::I8    => q.int_type(8),
      TypePrim::I16   => q.int_type(16),
      TypePrim::I32   => q.int_type(32),
      TypePrim::I64   => q.int_type(64),
      TypePrim::U8    => q.int_type(8),
      TypePrim::U16   => q.int_type(16),
      TypePrim::U32   => q.int_type(32),
      TypePrim::U64   => q.int_type(64),
      _               => unimplemented!(),
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl ReduceType for TypePtr {
  fn reduce_type(&self, q: &mut TypeQuoter) -> MIRTypeId {
    let target_ty = q.reduce_type(self.inner).into();
    q.build_type(mir::TypePtr { target_ty })
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl ReduceType for TypeStruct {
  fn reduce_type(&self, q: &mut TypeQuoter) -> MIRTypeId {
    let elements = self.elements.iter()
      .map(|e| q.reduce_type(*e).into())
      .collect::<Vec<_>>();
    q.build_type(mir::TypeStruct { elements })
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl ReduceType for TypeTuple {
  fn reduce_type(&self, q: &mut TypeQuoter) -> MIRTypeId {
    let elements = self.elements.iter()
      .map(|e| q.reduce_type(*e).into())
      .collect::<Vec<_>>();
    q.build_type(mir::TypeStruct { elements })
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub fn quote_type<T: QuoteType>(model_cell: &ModelCell) -> Link<dyn mir::Type> {
  let mut quoter = type_quoter(model_cell);
  quoter.quote::<T>()
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub fn type_quoter(model_cell: &ModelCell) -> impl DerefMut<Target=TypeQuoter> {
  cell_get_mut_or!(MIR_TYPE_QUOTER(model_cell), || TypeQuoter::new(model_cell))
}

////////////////////////////////////////////////////////////////////////////////////////////////
