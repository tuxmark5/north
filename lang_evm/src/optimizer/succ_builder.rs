use {
  crate::{
    optimizer::{BuildSuccs, ClosureSeed},
  },
  north_core::{
    node_id::{NodeId},
    visitor::{VisitorAspect, VisitCtxCore},
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct SuccBuilder {
  pub queue: Vec<NodeId>,
  pub succ: Vec<NodeId>,
}

impl SuccBuilder {
  pub fn new(queue: Vec<NodeId>) -> Self {
    Self { 
      queue,
      succ: Vec::new(),
    }
  }

  pub fn add_succ(&mut self, id: NodeId) {
    if !self.succ.contains(&id) {
      self.succ.push(id);
    }
  }

  pub fn build(&mut self, aspect: &VisitorAspect) {
    let mut index = 0;
    while index < self.queue.len() {
      let node_id = self.queue[index];
      self.build_entry(aspect, node_id);
      index += 1;
    }
  }

  pub fn build_entry(&mut self, aspect: &VisitorAspect, node_id: NodeId) {
    let core = VisitCtxCore::<BuildSuccs> { aspect, node_id, imp_args: &mut () };
    let _ = aspect.visit(core, self);
  }

  pub fn complete(self) -> ClosureSeed {
    ClosureSeed::new(self.succ)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
