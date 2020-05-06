use {
  crate::{
    mir::{self, LValLink, ReduceKind},
    optimizer::{
      ClosureSeed, MatchMap, RuleBuilder,
      group::{Group},
      prelude::*,
    },
    runtime::{
      MatchId, ReduceId,
    },
  },
  interval_map::{IntervalMap},
  lang_mir::{Cursor},
  north_core::{
    Node, 
    node_id::ToNodeId,
    visitor::prelude::*,
  },
  north_derive::{
    aspect_rules
  },
  std::{
    cmp, u32,
    any::Any,
    collections::BTreeMap,
    cmp::{Ordering, min},
    hash::Hash,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

macro_rules! define_group {
  (
    async: $async:expr,
    priority: $priority:expr,
    key: $key:ident $key_body:tt
    value: $value:ident $value_body:tt
    $($rest:tt)*
  ) => {
    #[derive(Eq, Hash, PartialEq)]
    struct $key $key_body

    #[derive(Default)]
    struct $value $value_body

    impl AsRef<dyn Any> for $key {
      fn as_ref(&self) -> &dyn Any { self }
    }

    impl Group for $key {
      const ASYNC: bool = $async;
      const PRIORITY: i32 = $priority;

      type Value = $value;

      impl_group!(__method, $value: $($rest)*);
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

macro_rules! impl_group {
  (__method, $value:ident: ) => {

  };

  (__method, $value:ident: 
    fn build($key_p:ident, $value_p:ident, $rb_p:ident, $cur_p: ident) 
    $build_body:tt $($rest:tt)*
  ) => {
    fn build_mir(&self, $value_p: &$value, $rb_p: &mut RuleBuilder, $cur_p: &mut Cursor) {
      let $key_p = self; $build_body
    }

    impl_group!(__method, $value: $($rest)*);
  };

  (__method, $value:ident: 
    fn is_accepted($key_p:ident, $value_p:ident, $rb_p:ident) 
    $is_accepted_body:tt $($rest:tt)*
  ) => {
    fn is_accepted(&self, $value_p: &$value, $rb_p: &mut RuleBuilder) -> bool {
      let $key_p = self; $is_accepted_body
    }

    impl_group!(__method, $value: $($rest)*);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

define_group! {
  async: false,
  priority: 0,

  key: CtlExecDFAKey { }
  
  value: CtlExecDFAValue {
    closure: ClosureSeed,
  }

  fn build(_key, value, rb, cur) {
    let closure = value.closure.clone().into_closure();
    let closure_id = rb.intern_closure(closure);
    let (dfa, next) = rb.dfa_builder.build_dfa(closure_id);

    let fail = rb.fail_block();
    let next = next.into_iter()
      .map(|closure| rb.resolve_closure(closure))
      .collect();

    cur.build_ctl(mir::CtlExecDFA {
      dfa, fail, next,
    });
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

define_group! {
  async: false,
  priority: 0,

  key: CtlLoopEndGroup {
    var: NodeId,
  }

  value: CtlLoopEndValue {
    ok: IntervalMap<u32, ClosureSeed>,
  }

  fn build(key, value, rb, cur) {
    let ok = value.ok.iter_raw()
      .map(|e| e.map_value_ref(|v| rb.resolve_closure_seed(v.clone())))
      .collect::<IntervalMap<_, _>>();

    cur.build_ctl(mir::CtlLoopEnd {
      var: key.var.cast().into(),
      ok,
      fail: rb.fail_block(),
    });
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

define_group! {
  async: false,
  priority: 0,

  key: CtlLoopNextGroup {
    var: NodeId,
  }

  value: CtlLoopNextValue {
    ok_head: ClosureSeed,
    ok_tail: ClosureSeed,
    max_iter_head: Option<u32>,
    complete_head: ClosureSeed,
  }

  fn build(key, value, rb, cur) {
    let ok = ClosureSeed::new_pair(&value.ok_head, &value.ok_tail);
    let ok = rb.resolve_closure_seed(ok);

    let complete = if value.max_iter_head.is_some() {
      let complete = ClosureSeed::new_pair(&value.complete_head, &value.ok_tail);
      Some(rb.resolve_closure_seed(complete))
    } else {
      None
    };

    cur.build_ctl(mir::CtlLoopNext {
      var: key.var.cast().into(),
      ok,
      max_iter: value.max_iter_head,
      complete,
    });
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

define_group! {
  async: false,
  priority: 0,

  key: CtlMatchCharKey { }
  
  value: CtlMatchCharValue {
    map: IntervalMap<u8, ClosureSeed>,
  }

  fn build(_key, value, rb, cur) {
    let ok = value.map.iter_raw()
      .cloned()
      .map(|e| e.map_value(|v| rb.resolve_closure_seed(v)))
      .collect::<IntervalMap<_, _>>();

    cur.build_ctl(mir::CtlMatchClass {
      ok, 
      fail: rb.fail_block(), 
      dom_group: 0,
      prefer_shift: false,
    });
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

type SymEntries = Vec<mir::CtlMatchSymEntry>;

define_group! {
  async: false,
  priority: -100,

  key: CtlMatchSymKey { }

  value: CtlMatchSymValue {
    pos: MatchMap,
    neg: MatchMap,
  }

  fn build(_key, value, rb, cur) {
    let pos = if rb.at_origin() && rb.is_origin_pure() { // false &&
      rb.origin_matches = value.pos.clone();
      Vec::new()
    } else {
      flatten_sym_map(&value.pos, rb)
    };

    let neg = flatten_sym_map(&value.neg, rb);
    if pos.len() + neg.len() > 0 {
      cur.build_ctl(mir::CtlMatchSym { pos, neg });
    } else {
      cur.ctl_br(rb.fail_block());
    }
  }

  fn is_accepted(_key, value, rb) {
    if rb.at_origin() && rb.is_origin_pure() {
      rb.origin_matches = value.pos.clone();
      value.neg.len() > 0
    } else {
      true
    }
  }
}

fn extend_sym_map(map: &mut MatchMap, entries: &SymEntries) {
  for e in entries {
    let seed = ClosureSeed::new_single(&e.block);
    let mut interval = IntervalMap::new();
    interval.insert(e.prec_range.clone(), Some(seed));

    let entry = map.entry(e.match_id).or_insert_with(<_>::default);
    entry.merge(interval.into_entries(), |a, b| a.merge(b));
  }
}

fn flatten_sym_map(map: &MatchMap, rb: &mut RuleBuilder) -> SymEntries {
  let mut result = Vec::new();
  
  for (match_id, prec_map) in map {
    for (prec_range, seed) in prec_map.iter() {
      let block = rb.resolve_closure_seed(seed.clone());
      result.push(mir::CtlMatchSymEntry {
        match_id: *match_id,
        prec_range,
        block,
      })
    }
  }

  result
}

////////////////////////////////////////////////////////////////////////////////////////////////

define_group! {
  async: false,
  priority: 0,

  key: CtlReduceKey { 
    reduce_id: ReduceId
  }
  
  value: CtlReduceValue {
    kind: Option<ReduceKind>,
    next: ClosureSeed,
  }

  fn build(key, value, rb, cur) {
    let mut next = value.next.clone();

    let is_origin_pure = rb.is_origin_pure();
    let optimize_reduce = is_origin_pure && (value.kind != Some(ReduceKind::Reject));

    if optimize_reduce {
      let reduce_next = rb.origin_matches(key.reduce_id);
      //if !reduce_next.is_empty() { rb.build_origin_stmts(cur); }
      next.merge(&reduce_next);
    }
    
    cur.build_ctl(mir::CtlReduce {
      kind: value.kind.unwrap(),
      reduce_id: key.reduce_id,
      short: optimize_reduce && !rb.is_reduce_external(key.reduce_id),
      next: rb.resolve_closure_seed(next),
      fail: rb.fail_block(),
    });
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

define_group! {
  async: true,
  priority: 0,

  key: StmtCallRuleKey { } // key - params

  value: StmtCallRuleValue {
    call_spec: BTreeMap<MatchId, u16>,
  }

  fn build(_key, value, _rb, cur) {
    cur.build_stmt(mir::StmtCallRuleDyn {
      call_spec: value.call_spec.clone(),
      args: vec![]
    });
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

define_group! {
  async: true,
  priority: 0,

  key: StmtLoopBeginKey {
    depth: usize,
  }

  value: StmtLoopBeginValue {
    var: Option<LValLink>,
  }

  fn build(key, value, _rb, cur) {
    cur.build_stmt(mir::StmtLoopBegin {
      var: value.var.clone().unwrap(),
      depth: key.depth,
    });
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

define_group! {
  async: true,
  priority: 0,

  key: StmtReduceKey { 
    reduce_id: ReduceId
  }
  
  value: StmtReduceValue {
    kind: Option<ReduceKind>,
  }

  fn build(key, value, _rb, cur) {
    cur.build_stmt(mir::StmtReduce {
      kind: value.kind.unwrap(),
      reduce_id: key.reduce_id,
    });
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

struct DefaultRule;
struct UniqueStmtRule;

aspect_rules! {
  <N: Clone + Eq + Hash + Node + PartialEq + mir::Stmt> 
  visit_r![BuildGroups, N => UniqueStmtRule] {
    fn visit_ok(self, ctx, node, gb) {
      gb.get_unique(ctx.node_id(), &*node);
    }
  }

  visit_r![BuildGroups, mir::CtlLoopEnd => DefaultRule] {
    fn visit_ok(self, ctx, node, gb) {
      let var = ctx.imp_args.map_lval_lookup(&node.var);
      let key = CtlLoopEndGroup { var: var.to_top() };
      let group = gb.get_mut(ctx.node_id(), key);

      let ok = node.ok.iter_raw()
        .map(|e| e.map_value_ref(|v| ClosureSeed::new_single(v)));
      group.ok.merge(ok, |a, b| a.merge(b));

      // XXX: fail
    }
  }

  visit_r![BuildGroups, mir::CtlLoopNext => DefaultRule] {
    fn visit_ok(self, ctx, node, gb) {
      let var = ctx.imp_args.map_lval_lookup(&node.var); // XXX: merge if labels match
      let key = CtlLoopNextGroup { var: var.to_top() };
      let group = gb.get_mut(ctx.node_id(), key);

      let node_max = node.max_iter.unwrap_or(u32::MAX);
      let group_max = group.max_iter_head.unwrap_or(u32::MAX);
      let ok = ClosureSeed::new_single(&node.ok);

      match node_max.cmp(&group_max) {
        Ordering::Less => {
          let complete = ClosureSeed::new_single(&node.complete.as_ref().unwrap());
          group.max_iter_head = Some(node_max);
          group.ok_tail.merge(&group.ok_head);
          group.ok_head = ok;
          group.complete_head = complete;
        },
        Ordering::Equal => {
          let complete = ClosureSeed::new_single(&node.complete.as_ref().unwrap());
          group.ok_head.merge(&ok);
          group.complete_head.merge(&complete);
        },
        Ordering::Greater => {
          group.ok_tail.merge(&ok);
        },
      }
    }
  }

  visit_r![BuildGroups, mir::CtlMatchChar => DefaultRule] {
    fn visit_ok(self, ctx, node, gb) {
      if true {
        let key = CtlExecDFAKey { };
        let value = gb.get_mut(ctx.node_id(), key);
        value.closure.insert(ctx.node_id());
      } else {
        let chr = node.char as u8;
        let ok_seed = ClosureSeed::new_single(&node.ok);
        let fail_seed = ClosureSeed::new_single(&node.fail);

        let mut new_map = IntervalMap::new();
        new_map.insert(chr..(chr + 1), Some(ok_seed));
        new_map.add_negative(fail_seed);

        let key = CtlMatchCharKey { };
        let value = gb.get_mut(ctx.node_id(), key);
        value.map.merge(new_map.into_entries(), |a, b| a.merge(b));
      }
    }
  }

  visit_r![BuildGroups, mir::CtlMatchClass => DefaultRule] {
    fn visit_ok(self, ctx, node, gb) {
      if true {
        let key = CtlExecDFAKey { };
        let value = gb.get_mut(ctx.node_id(), key);
        value.closure.insert(ctx.node_id());
      } else {
        let entries = node.ok.iter_raw()
          .map(|e| e.map_value_ref(|v| ClosureSeed::new_single(v)));

        let key = CtlMatchCharKey { };
        let value = gb.get_mut(ctx.node_id(), key);
        value.map.merge(entries, |a, b| a.merge(b));
      }
    }
  }

  visit_r![BuildGroups, mir::CtlMatchSym => DefaultRule] {
    fn visit_ok(self, ctx, node, gb) {
      let key = CtlMatchSymKey { };
      let value = gb.get_mut(ctx.node_id(), key);

      extend_sym_map(&mut value.pos, &node.pos);
      extend_sym_map(&mut value.neg, &node.neg);
    }
  }

  visit_r![BuildGroups, mir::CtlReduce => DefaultRule] {
    fn visit_ok(self, ctx, node, gb) {
      let key = CtlReduceKey { reduce_id: node.reduce_id };
      let value = gb.get_mut(ctx.node_id(), key);

      let prev_kind = value.kind.unwrap_or(ReduceKind::Prefer);
      let next = ClosureSeed::new_single(&node.next);
      value.kind = Some(cmp::min(prev_kind, node.kind));
      value.next.merge(&next);
    }
  }
  
  visit_r![BuildGroups, mir::StmtCallRule => DefaultRule] {
    fn visit_ok(self, ctx, node, gb) {
      let key = StmtCallRuleKey { };
      let value = gb.get_mut(ctx.node_id(), key);

      let call_entry = value.call_spec.entry(node.match_id);
      let min_prec = call_entry.or_insert(node.min_prec);
      *min_prec = min(*min_prec, node.min_prec);
    }
  }

  visit_r![BuildGroups, mir::StmtCallRuleDyn => DefaultRule] {
    fn visit_ok(self, ctx, node, gb) {
      let key = StmtCallRuleKey { };
      let value = gb.get_mut(ctx.node_id(), key);

      for (reduce_id, min_prec) in &node.call_spec {
        let call_entry = value.call_spec.entry(*reduce_id);
        let call_min_prec = call_entry.or_insert(*min_prec);
        *call_min_prec = min(*call_min_prec, *min_prec);
      }
    }
  }

  visit_r![BuildGroups, mir::StmtLoopBegin => DefaultRule] {
    fn visit_ok(self, ctx, node, gb) {
      let key = StmtLoopBeginKey { depth: node.depth };
      let value = gb.get_mut(ctx.node_id(), key);

      match &value.var {
        Some(new_var) => {
          ctx.imp_args.add_value_map(&node.var, new_var);
        },
        None => {
          let new_var = ctx.imp_args.map_local_iter(&node.var, node.depth);
          value.var = Some(new_var);
        }
      }
    }
  }

  visit_r![BuildGroups, mir::StmtReduce => DefaultRule] {
    fn visit_ok(self, ctx, node, gb) {
      let key = StmtReduceKey { reduce_id: node.reduce_id };
      let value = gb.get_mut(ctx.node_id(), key);

      let prev_kind = value.kind.unwrap_or(ReduceKind::Prefer);
      value.kind = Some(cmp::min(prev_kind, node.kind));
    }
  }
}

lang_part! {
  BuildGroupsPart {
    visit_r![BuildGroups, mir::CtlLoopEnd => DefaultRule],
    visit_r![BuildGroups, mir::CtlLoopNext => DefaultRule],
    visit_r![BuildGroups, mir::CtlMatchChar => DefaultRule],
    visit_r![BuildGroups, mir::CtlMatchClass => DefaultRule],
    visit_r![BuildGroups, mir::CtlMatchSym => DefaultRule],
    visit_r![BuildGroups, mir::CtlReduce => DefaultRule],
    visit_r![BuildGroups, mir::StmtCallRule => DefaultRule],
    visit_r![BuildGroups, mir::StmtCallRuleDyn => DefaultRule],
    visit_r![BuildGroups, mir::StmtLoopBegin => DefaultRule],
    visit_r![BuildGroups, mir::StmtReduce => DefaultRule],
    visit_r![BuildGroups, mir::StmtRewind => UniqueStmtRule],
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
