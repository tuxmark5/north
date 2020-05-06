use {
  crate::{
    collections::IndexMap,
    optimizer::{Closure, ClosureId},
  },
  std::{
    cell::{Ref, RefCell},
    rc::Rc,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct ClosureInterner {
  closures: RefCell<IndexMap<Closure, ()>>,
}

impl ClosureInterner {
  pub fn new() -> Rc<ClosureInterner> {
    let mut closures = IndexMap::default();
    closures.insert(Closure::default(), ());

    Rc::new(Self { 
      closures: RefCell::new(closures)
    })
  }

  pub fn get(&self, id: ClosureId) -> Ref<Closure> {
    let closures = self.closures.borrow();
    Ref::map(closures, |c| {
      c.get_index(id.0 as usize).unwrap().0
    })
  }

  pub fn intern(&self, closure: Closure) -> ClosureId {
    let mut closures = self.closures.borrow_mut();
    let entry = closures.entry(closure);
    let index = entry.index();
    entry.or_insert_with(<_>::default);
    ClosureId(index)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

