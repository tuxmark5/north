use {
  crate::{
    collections::{IndexMap, index_map::Entry::*},
    mir::{self},
    optimizer::{
      ClosureSeed, Group, GroupDyn, RuleBuilder,
      group::{UniqueGroup},
    },
  },
  lang_mir::Cursor,
  north_core::{
    Node,
    node_id::{NodeId},
    util::dyn_traits::DynBox,
  },
  std::{
    any::Any,
    hash::Hash,
    mem::{transmute_copy, replace},
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct GroupBuilder {
  pub groups: GroupSet,
}

impl GroupBuilder {
  pub fn new() -> Self {
    Self { 
      groups: GroupSet::new(),
    }
  }

  pub fn complete(&mut self) -> GroupSet {
    replace(&mut self.groups, GroupSet::new())
  }

  pub fn get_mut<G: Group>(&mut self, node_id: NodeId, group: G) -> &mut G::Value {
    self.groups.get_mut(node_id, group)
  }

  pub fn get_unique<N>(&mut self, node_id: NodeId, node: &N) where
    N: Clone + Eq + Hash + Node + PartialEq + mir::Stmt
  {
    let group = UniqueGroup::new(node.clone());
    let _ = self.groups.get_mut(node_id, group);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct GroupSet {
  set: IndexMap<DynBox<dyn GroupDyn>, GroupValue>,
}

impl GroupSet {
  pub fn new() -> Self {
    Self { set: IndexMap::default() }
  }

  pub fn build_mir(self, rb: &mut RuleBuilder, mut cur: Cursor) {
    let mut has_async = false;
    let mut sync = Vec::new();
    
    for (k, v) in self.set.into_iter() {
      if k.0.is_async() {
        has_async = true;
        k.0.build_mir_dyn(&*v.value, rb, &mut cur);
        if rb.num_built == 0 { 
          let mut stmts = rb.origin_stmts.borrow_mut();
          stmts.push((k.0, v.value)); 
        }
      } else {
        sync.push((k.0, v));
      }
    }

    if rb.num_built == 0 { 
      sync.retain(|(k, v)| k.is_accepted(&*v.value, rb));
    }

    if sync.is_empty() {
      rb.fuse_block(cur);
      return;
    }

    if !has_async && sync.len() == 1 {
      let (k, v) = sync.pop().unwrap();
      let closure = v.closure.into_closure();
      rb.queue_build_one(cur, closure, (k, v.value));
      return;
    }

    sync.sort_by_key(|p| p.0.priority());
    
    let mut blocks = sync.into_iter()
      .map(|(k, v)| rb.resolve_one(v.closure, (k, v.value)))
      .collect::<Vec<_>>();

    match blocks.len() {
      0 => unreachable!(),
      1 => cur.ctl_br(blocks.pop().unwrap()),
      _ => cur.build_ctl(mir::CtlFork { blocks }),
    }
  }

  pub fn get_mut<G: Group>(&mut self, node_id: NodeId, group: G) -> &mut G::Value {
    let key: DynBox<dyn GroupDyn> = DynBox(box group);

    let value = match self.set.entry(key) {
      Occupied(occupied) => {
        let value = occupied.into_mut();
        value.closure.insert(node_id);
        value
      },
      Vacant(vacant) => vacant.insert(GroupValue {
        closure: ClosureSeed::new_single(node_id),
        value: box G::Value::default(),
      }),
    };

    unsafe {
      transmute_copy::<Box<dyn Any>, _>(&mut value.value)
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct GroupValue {
  closure: ClosureSeed,
  value: Box<dyn Any>,
}

////////////////////////////////////////////////////////////////////////////////////////////////

// pub fn build_closure(seed: Vec<NodeId>) -> Vec<NodeId> {
//   COMPILER.with(|comp| {
//     let aspect = comp.aspect_mut::<VisitorAspect>();
//     let mut builder = ClosureBuilder::new(seed);
//     builder.build(&*aspect);
//     builder.closure
//   })
// }

////////////////////////////////////////////////////////////////////////////////////////////////
