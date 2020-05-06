use {
  crate::{
    context::Context,
  },
  llvm_sys::{
    prelude::*
  },
  north_core::{
    cell::prelude::*,
    quote::{
      RUST_TYPE_QUOTER,
      QuoteType, RustQuoter,
      rust_type::*
    },
  },
  std::{
    cell::RefCell,
    collections::{HashMap},
    rc::Rc,
  }
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Reducer {
  rust_quoter: Rc<RefCell<RustQuoter>>,
  llvm_types: HashMap<TypeId, LLVMTypeRef>,
}

impl Reducer {
  pub fn new() -> Self {
    Self {
      rust_quoter: cell_cell!(RUST_TYPE_QUOTER),
      llvm_types: HashMap::new(),
    }
  }

  pub fn quote<T: QuoteType>(&mut self, ctx: &Context) -> LLVMTypeRef {
    let type_id = {
      let mut rust_quoter = self.rust_quoter.borrow_mut();
      rust_quoter.quote::<T>()
    };
    self.reduce_type(ctx, type_id)
  }

  pub fn reduce_type(&mut self, ctx: &Context, type_id: TypeId) -> LLVMTypeRef {
    if let Some(llvm_type) = self.llvm_types.get(&type_id) {
      return *llvm_type;
    }

    let rust_quoter = self.rust_quoter.clone();
    let rust_quoter = rust_quoter.borrow();
    let rust_type = rust_quoter.get(type_id);

    let llvm_type = rust_type.reduce_type(ctx, self);
    self.llvm_types.insert(type_id, llvm_type);
    llvm_type
  }

  pub fn reduce_types<'a, I>(&mut self, ctx: &Context, iter: I) -> Vec<LLVMTypeRef> where
    I: IntoIterator<Item=&'a TypeId>
  {
    iter.into_iter()
      .map(|e| self.reduce_type(ctx, *e))
      .collect()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait ReduceType {
  fn reduce_type(&self, ctx: &Context, red: &mut Reducer) -> LLVMTypeRef;
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl ReduceType for Type {
  fn reduce_type(&self, ctx: &Context, red: &mut Reducer) -> LLVMTypeRef {
    match self {
      Type::Fn(f)     => f.reduce_type(ctx, red),
      Type::Prim(p)   => p.reduce_type(ctx, red),
      Type::Ptr(p)    => p.reduce_type(ctx, red),
      Type::Struct(s) => s.reduce_type(ctx, red),
      Type::Tuple(t)  => t.reduce_type(ctx, red),
      Type::Unit      => ctx.type_void(),
      _               => unimplemented!("{:?}", self),
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl ReduceType for TypeFn {
  fn reduce_type(&self, ctx: &Context, red: &mut Reducer) -> LLVMTypeRef {
    let mut params = red.reduce_types(ctx, &self.params);
    let result = red.reduce_type(ctx, self.result);
    ctx.type_function(result, params.as_mut())
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl ReduceType for TypePrim {
  fn reduce_type(&self, ctx: &Context, _red: &mut Reducer) -> LLVMTypeRef {
    match self {
      TypePrim::Bool  => ctx.type_int(1),
      TypePrim::I8    => ctx.type_int(8),
      TypePrim::I16   => ctx.type_int(16),
      TypePrim::I32   => ctx.type_int(32),
      TypePrim::I64   => ctx.type_int(64),
      TypePrim::U8    => ctx.type_int(8),
      TypePrim::U16   => ctx.type_int(16),
      TypePrim::U32   => ctx.type_int(32),
      TypePrim::U64   => ctx.type_int(64),
      _               => unimplemented!(),
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl ReduceType for TypePtr {
  fn reduce_type(&self, ctx: &Context, red: &mut Reducer) -> LLVMTypeRef {
    let inner = red.reduce_type(ctx, self.inner);
    ctx.type_pointer(inner)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl ReduceType for TypeStruct {
  fn reduce_type(&self, ctx: &Context, red: &mut Reducer) -> LLVMTypeRef {
    let mut elements = red.reduce_types(ctx, &self.elements);
    ctx.type_struct(elements.as_mut())
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

impl ReduceType for TypeTuple {
  fn reduce_type(&self, ctx: &Context, red: &mut Reducer) -> LLVMTypeRef {
    let mut members = self.elements.iter()
      .map(|e| red.reduce_type(ctx, *e))
      .collect::<Vec<_>>();
    ctx.type_struct(members.as_mut())
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
