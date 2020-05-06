use {
  crate::{
    infer_types::{
      InferTypesDyn,
      InferTypesRuleKey,
      InferTypesRuleMap
    },
    unify_types::{
      UnifyTypes,
      UnifyTypesCtx,
      UnifyTypesDyn,
      UnifyTypesDynAdapter,
      UnifyTypesRuleKey,
      UnifyTypesRuleMap
    }
  },
  north_core::{
    compiler::{Aspect, Compiler},
    futures::{
      executor::LocalSpawner,
      prelude::*,
      task::LocalSpawn,
    },
    log::warn,
    model::{
      ModelCell,
      element,
      node::NodeCell,
    },
    prelude::*,
    structure::StorageExt,
    trait_manager::TraitManagerExt,
    util::{
      dynamic_cast,
      NodeCache,
    },
  },
  std::{
    any::{TypeId},
    cell::RefCell,
    collections::{HashMap, hash_map::Entry},
    hash::Hash,
    pin::Pin,
    rc::Rc,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub enum DerefResult {
  Hole(NodeCell<HoleType>),
  Node(NodeCell<dyn Node>),
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub type NodeIdFuture = Pin<Box<dyn Future<Output=Result<NodeId, ()>>>>;

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Node)]
pub struct HoleType {
  target_type: Option<NodeId>, // REference
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub trait TypeRef: Node {
  fn target_type(&self) -> NodeId;
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct TypeSysAspect {
  pub spawn: LocalSpawner,
  pub infer_types_rules: InferTypesRuleMap,
  pub model_cell: ModelCell,
  pub type_cache: RefCell<NodeCache>,
  pub type_map: RefCell<HashMap<NodeId, NodeId>>,
  pub unify_types_rules: UnifyTypesRuleMap,
}

impl TypeSysAspect {
  pub fn new(spawn: LocalSpawner, model_cell: ModelCell) -> Self {
    Self {
      spawn,
      infer_types_rules: InferTypesRuleMap::new(),
      model_cell: model_cell.clone(),
      type_cache: RefCell::new(NodeCache::new(model_cell)),
      type_map: RefCell::new(HashMap::new()),
      unify_types_rules: UnifyTypesRuleMap::new(),
    }
  }

  pub fn add_unify_types<A, B, R>(&mut self, rule: R) where
    A: Node + ?Sized, B: Node + ?Sized, R: 'static + UnifyTypes<A, B>
  {
    let rule_key = UnifyTypesRuleKey::new_static::<A, B>();
    let rule = UnifyTypesDynAdapter::wrap(rule);
    self.unify_types_rules.insert(rule_key, rule);
  }

  pub fn deref_type(&self, mut id: NodeId) -> (NodeId, DerefResult) {
    let model = self.model_cell.borrow();

    loop {
      let node_cell = model.data_cell(id);

      // Deref the node
      let maybe_ref = model.cast::<_, NodeCell<dyn TypeRef>>(node_cell);
      let node_cell = match maybe_ref {
        Ok(ref_node) => { id = ref_node.borrow().target_type(); continue; },
        Err(node_cell) => node_cell,
      };

      // Test hole
      let maybe_hole = dynamic_cast::<_, NodeCell<HoleType>>(node_cell);
      let hole_cell = match maybe_hole {
        Ok(hole_cell) => hole_cell,
        Err(node_cell) => return (id, DerefResult::Node(node_cell))
      };

      // Test hole target
      let target_type = hole_cell.borrow().target_type;
      match target_type {
        Some(target) => { id = target; },
        None => return (id, DerefResult::Hole(hole_cell))
      }
    }
  }

  /*pub fn deref_type_async(mut id: NodeId)
    -> impl Future<Item=(NodeId<dyn 'static + Node>, i32), Error=()>
  {
    /*let model_cell = self.model_cell.clone();

    let future = async_block! {
      loop {
        let node_cell = model_cell.node_cell_dyn(id);
        let downcast_result = dynamic_cast::<_, NodeCell<HoleType>>(node_cell.clone());

        let hole_cell = match downcast_result {
          Ok(hole_cell) => hole_cell,
          Err(_) => return Ok((id, DerefResult::Node(node_cell)))
        };

        let target_type = hole_cell.borrow().target_type;
        match target_type {
          Some(target) => { id = target; },
          None => return Ok((id, DerefResult::Hole(hole_cell)))
        }
      }
    };

    future.pin_local()*/

    let nid = NodeId::from_id(10);
    //let cell = unimplemented!();
    future::ok((nid, 456))
  }*/

  pub fn deref_types(&self) {
    let mut type_map = self.type_map.borrow_mut();

    let new_type_map = {
      let deref_pair = |(node_id, type_id): (&NodeId, &NodeId)| {
        let (type_id, result) = self.deref_type(*type_id);
        // if let DerefResult::Hole(_) = result {
        //   warn!("incomplete type: {:?} => {:?}", node_id, type_id);
        // }
        (*node_id, type_id)
      };

      type_map.iter()
        .map(deref_pair)
        .collect::<HashMap<_, _>>()
    };

    *type_map = new_type_map;
  }

  pub fn deref_types_deep(&self) {
    let model = self.model_cell.borrow();
    let type_map = self.type_map.borrow();

    for type_id in type_map.values() {
      let mut type_node = model.data_mut(*type_id);
      element::map_children(&mut *type_node, &mut |id| {
        self.deref_type(id).0
      });
    }
  }

  pub fn dump_types(&self) {
    let type_map = self.type_map.borrow();
    for (node_id, type_id) in &*type_map {
      println!("TYPE_OF {:?} => {:?}", node_id, type_id);
    }
  }

  pub fn get_infer_types(&self, node_type: TypeId)
    -> Option<Rc<dyn InferTypesDyn>>
  {
    let rule_key = InferTypesRuleKey { node_type };
    self.infer_types_rules.get(rule_key)
  }

  pub fn get_unify_types(&self, left_type: TypeId, right_type: TypeId)
    -> Option<Rc<dyn UnifyTypesDyn>>
  {
    let rule_key = UnifyTypesRuleKey { left_type, right_type };
    self.unify_types_rules.get(&rule_key).cloned()
  }

  pub fn make_type<N: Node>(&self, node: N) -> NodeId where
    N: Clone + Eq + Hash + Node + PartialEq
  {
    let mut type_cache = self.type_cache.borrow_mut();
    type_cache.get(node).cast()
  }

  pub fn type_of(&self, node: NodeId) -> NodeId {
    let id = {
      let mut type_map = self.type_map.borrow_mut();

      match type_map.entry(node) {
        Entry::Occupied(occupied) => *occupied.get(),
        Entry::Vacant(vacant) => {
          let mut model = self.model_cell.borrow_mut();
          let type_node = HoleType { target_type: None };
          let node_type = model.build_node(type_node).cast();
          vacant.insert(node_type);
          return node_type;
        }
      }
    };

    self.deref_type(id).0
  }

  pub fn type_of_concrete(&self, node_id: NodeId) -> Option<NodeId> {
    let type_map = self.type_map.borrow();
    type_map.get(&node_id).cloned()
  }

  pub async fn unify_types(self: Rc<Self>, type_a: NodeId, type_b: NodeId) -> Result<(), ()> {
    let (id_a, node_a) = self.deref_type(type_a);
    let (id_b, node_b) = self.deref_type(type_b);

    //let (id_a, node_a) = await!(self.deref_type_async(type_a))?;
    //let (id_b, node_b) = await!(self.deref_type_async(type_b))?;

    if id_a == id_b {
      return Ok(());
    }

    use self::DerefResult::*;
    match (node_a, node_b) {
      (Hole(hole_a), Hole(hole_b)) => {
        if id_a < id_b {
          let mut hole_a = hole_a.borrow_mut();
          hole_a.target_type = Some(id_b);
        } else {
          let mut hole_b = hole_b.borrow_mut();
          hole_b.target_type = Some(id_a);
        }
      },

      (Hole(hole_a), Node(_)) => {
        let mut hole_a = hole_a.borrow_mut();
        hole_a.target_type = Some(id_b);
      },

      (Node(_), Hole(hole_b)) => {
        let mut hole_b = hole_b.borrow_mut();
        hole_b.target_type = Some(id_a);
      },

      (Node(node_a), Node(node_b)) => {
        let node_a = node_a.borrow();
        let node_b = node_b.borrow();
        self.unify_type_nodes(&*node_a, &*node_b);
      },
    }

    Ok(())
  }

  pub fn unify_type_nodes(self: Rc<Self>, node_a: &dyn Node, node_b: &dyn Node) {
    let node_a_tid = (*node_a).type_id();
    let node_b_tid = (*node_b).type_id();

    if let Some(rule) = self.get_unify_types(node_a_tid, node_b_tid) {
      let model_cell = self.model_cell.clone();
      let ctx = UnifyTypesCtx::new(self, model_cell);
      rule.unify_types_dyn(ctx, node_a, node_b);
    } else {
      panic!("no unify rule {} with {}",
        node_a.instance_concept().name(),
        node_b.instance_concept().name()
      );
    }
  }

  pub fn unify_types_start(self: Rc<Self>, type_a: NodeId, type_b: NodeId) {
    let mut spawn = self.spawn.clone();
    let future = self.unify_types(type_a, type_b);
    let future = (box future.map(|_| ())).into();
    spawn.spawn_local_obj(future).unwrap();
  }
}

impl Aspect for TypeSysAspect {
  fn create(comp: &Compiler) -> Self where Self: Sized {
    let spawn = comp.local_spawn.clone();
    let model_cell = comp.model_cell.clone();
    Self::new(spawn, model_cell)
  }

  fn name(&self) -> &str { "type system" }
}

////////////////////////////////////////////////////////////////////////////////////////////////
