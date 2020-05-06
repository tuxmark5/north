pub mod rules;
pub mod visit;
pub mod visit_pass;

pub use self::{
  rules::{VisitAbort, VisitChildren},
  visit::{Visit, VisitCtx, VisitCtxCore, VisitDyn},
  visit_pass::VisitPass,
};

////////////////////////////////////////////////////////////////////////////////////////////////

use {
  crate::{
    compiler::{COMPILER, Aspect, Compiler},
    context::{
      NodeIdCtxExt,
    },
    model::ModelCell,
    node_id::ToNodeId,
    visitor::visit::{VisitRuleMap},
  },
  std::{
    any::TypeId,
    rc::Rc
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub mod prelude {
  pub use {
    crate::{
      visit_r,
      
      cell::prelude::*,
      compiler::{
        aspect::{AspectPart, aspect_cast},
      },
      context::{
        ModelCellCtxExt,
        ModelNodeIdCtxExt,
        NodeIdCtxExt
      },
      model::{Model, ModelCell, PropMapExt},
      prelude::*,
      structure::{ForestExt, StorageExt},
      trait_manager::TraitManagerExt,
      visitor::{
        Visit, Visitor, VisitorAspect, VisitCtx, VisitCtxCore, visit,
        rules::{VisitAbort, VisitChildren},
      }
    },
    north_derive::{
      aspect_rules, cell,
    },
  };
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait Visitor: 'static {
  type ImplicitArgs;
  type ExplicitArgs;
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct VisitorAspect {
  pub input_model: ModelCell,
  pub visit_rules: VisitRuleMap,
}

impl VisitorAspect {
  pub fn new(input_model: ModelCell) -> Self {
    Self {
      input_model,
      visit_rules: VisitRuleMap::new(),
    }
  }

  pub fn get_visit<V: Visitor>(&self, node_type: TypeId) 
    -> Option<Rc<dyn VisitDyn<V>>>
  {
    self.visit_rules.get_opt(node_type)
  }

  pub fn visit<V>(
    &self, core: VisitCtxCore<V>, args: &mut V::ExplicitArgs
  ) -> Result<(), ()> where
    V: Visitor
  {
    let node_id = core.node_id();
    let node_cell = self.input_model.node_cell_dyn(node_id);
    let node_ref = node_cell.borrow();
    let rule = self.visit_rules.get::<V>(&*node_ref);
    rule.visit_dyn((core, &*node_ref, args))
  }

  pub fn visit_full<'ia, 'ea, V: Visitor, I: ToNodeId>(
    &self, id: I,
    imp_args: &'ia mut V::ImplicitArgs, 
    exp_args: &'ea mut V::ExplicitArgs
  ) -> Result<(), ()> where
    V: Visitor
  {
    let core = VisitCtxCore::<V> {
      aspect: self,
      node_id: id.to_top(),
      imp_args,
    };

    self.visit(core, exp_args)
  }
}

impl Aspect for VisitorAspect {
  fn create(comp: &Compiler) -> Self where 
    Self: Sized 
  {
    Self::new(comp.model_cell.clone())
  }

  fn name(&self) -> &str {
    "visitor aspect"
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub fn visit<'ia, 'ea, V: Visitor, I: ToNodeId>(
  id: I,
  imp_args: &'ia mut V::ImplicitArgs, 
  exp_args: &'ea mut V::ExplicitArgs
) {
  COMPILER.with(|comp| {
    let aspect = comp.aspect_mut::<VisitorAspect>();
    let result = aspect.visit_full::<V, I>(id, imp_args, exp_args);
    let _ = result.unwrap();
  })
}

////////////////////////////////////////////////////////////////////////////////////////////////
