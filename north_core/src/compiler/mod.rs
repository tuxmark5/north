pub mod aspect;
pub mod diag;
pub mod pass;
pub mod pass_manager;
pub mod rule;

pub use self::{
  aspect::{Aspect, AspectPart},
  diag::{Diag, DiagBox, DiagHandler, DiagVec},
  pass::Pass,
  pass_manager::PassManager,
};

////////////////////////////////////////////////////////////////////////////////////////////////

use {
  crate::{
    cell::CellMap,
    model::{
      Model, ModelCell,
      printer::Printer,
    },
    util::{
      chain::{ChainRef},
      dynamic_cast,
    }
  },
  futures::{
    Future, FutureExt,
    executor::{LocalPool, LocalSpawner},
    task::LocalSpawn,
  },
  std::{
    any::TypeId,
    cell::{RefCell},
    collections::HashMap,
    ops::{DerefMut},
    rc::Rc,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub type AspectCell<A> = Rc<RefCell<A>>;
pub type AspectCellDyn = AspectCell<dyn Aspect>;
pub type AspectMap = HashMap<TypeId, Rc<dyn Aspect>>;

////////////////////////////////////////////////////////////////////////////////////////////////

scoped_thread_local!(pub static COMPILER: Compiler);

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Compiler {
  pub aspects: RefCell<AspectMap>,
  pub diag_handler: Option<Box<dyn DiagHandler>>,
  pub local_pool: RefCell<LocalPool>,
  pub local_spawn: LocalSpawner,
  pub model_cell: ModelCell,
  pub pass_manager: PassManager,
  pub prop_map: RefCell<CellMap>,
}

impl Compiler {
  pub fn new() -> Self {
    let local_pool = LocalPool::new();
    let local_spawn = local_pool.spawner();

    Self {
      aspects: RefCell::new(AspectMap::new()),
      diag_handler: None,
      local_pool: RefCell::new(local_pool),
      local_spawn,
      model_cell: ModelCell::new(),
      pass_manager: PassManager::new(),
      prop_map: RefCell::new(CellMap::new()),
    }
  }

  pub fn aspect<A>(&self) -> Rc<A> where
    A: Aspect
  {
    let aspects = self.aspects.borrow();
    let aspect_type = TypeId::of::<A>();
    let aspect_cell_dyn = aspects.get(&aspect_type).unwrap().clone();
    dynamic_cast::<_, Rc<A>>(aspect_cell_dyn).ok().unwrap()
  }

  pub fn aspect_cell<A>(&self) -> Rc<A> where
    A: Aspect
  {
    let mut aspects = self.aspects.borrow_mut();
    let aspect_type = TypeId::of::<A>();

    let aspect_dyn = if aspects.contains_key(&aspect_type) {
      aspects.get_mut(&aspect_type)
        .unwrap().clone()
    } else {
      let aspect = self.init_aspect::<A>();
      aspects.entry(aspect_type)
        .or_insert(aspect).clone()
    };

    dynamic_cast::<_, Rc<A>>(aspect_dyn)
      .ok().unwrap()
  }

  pub fn aspect_mut<A>(&self) -> Rc<A> where
    A: Aspect
  {
    self.aspect_cell::<A>()
  }

  pub fn dump_model(&self) {
    let model = self.model_cell.borrow();
    let mut output = String::new();
    Printer::print_model(&*model, &mut output);
    println!("{}", output);
  }

  pub fn emit_diag(&self, diag: Box<dyn Diag>) {
    println!("diag: {}", diag.message());
    //unimplemented!()
    /*match self.diag_handler {
      Some(ref mut handler) => handler.add_diag(diag),
      None => println!("diag: {}", diag.message()),
    }*/
  }

  pub fn emit_diags<I>(&self, diags: I) where
    I: IntoIterator<Item=Box<dyn Diag>>
  {
    for diag in diags {
      self.emit_diag(diag);
    }
  }

  pub fn init_aspect<A>(&self) -> Rc<A> where
    A: 'static + Aspect
  {
    let mut aspect = A::create(self);
    let aspect_type = TypeId::of::<A>();
    let model = self.model_cell.borrow();

    for lang in &model.languages {
      for part in lang.aspect_parts() {
        part.setup_dyn(&mut aspect);
        if part.aspect_type() == aspect_type {
          part.setup(&mut aspect);
        }
      }
    }

    Rc::new(aspect)
  }

  pub fn model_mut(&self) -> impl DerefMut<Target=Model> {
    ChainRef::new(self.model_cell.clone(), |c| c.borrow_mut())
  }

  pub fn run_passes(&self) -> Result<(), ()> {
    let passes = self.pass_manager.passes.clone();

    for pass in passes {
      // println!("executing pass: {}", pass.name());
      pass.execute(self)?;
      self.run_tasks();
    }

    /*if let Some(ref mut handler) = self.diag_handler {
      handler.emit_all();
    }*/

    Ok(())
  }

  pub fn run_tasks(&self) {
    let mut local_spawn = self.local_spawn.clone();
    let mut local_pool = self.local_pool.borrow_mut();
    local_pool.run(); //local_pool.run(&mut local_spawn);
  }

  pub fn spawn_local<F>(&self, future: F) where
    F: 'static + Future<Output=Result<(), ()>>
  {
    let future = future.map(|_| ());
    let future_obj = (box future).into();
    let mut executor = self.local_spawn.clone();
    executor.spawn_local_obj(future_obj).unwrap();
  }

  pub fn with<F, R>(&self, f: F) -> R where
    F: FnOnce() -> R
  {
    COMPILER.set(self, f)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
