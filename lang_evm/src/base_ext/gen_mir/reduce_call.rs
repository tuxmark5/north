use {
  crate::{
    ast,
    base_ext::{
      gen_mir::{
        FailBlock, 
        reduction::{CallRed, call_red_params},
      },
    },
    mir::{self, BlockRef},
    runtime::{MatchId},
  },
  interval_map::IntervalMap,
  lang_mir::{CodegenCtxExt, Cursor, ValueRed},
  north_core::{NodeId, model::value::M},
  north_derive::aspect_rules,
  north_gen::prelude::*,
  std::{
    collections::BTreeMap,
    ops::RangeInclusive,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone)]
pub struct CallInfo {
  pub match_id: MatchId,
  pub call_node: NodeId<dyn mir::Call>,
}

impl CallInfo {
  pub fn new(match_id: MatchId, call_node: NodeId<dyn mir::Call>) -> Self {
    Self { match_id, call_node }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

struct DefaultRule;

aspect_rules! {
  reduce_r![CallRed, ast::BuiltinAny => DefaultRule] {
    fn reduce(self, ctx, _node, cur) -> Option<CallInfo> {
      let block_ok = ctx.block();
      let block_fail = FailBlock::get_any(ctx);
      let call_params = call_red_params();

      let mut ok = IntervalMap::<u8, BlockRef>::new();
      ok.insert(0x00..=0xFF, Some(block_ok.into()));

      cur.build_ctl(mir::CtlMatchClass {
        ok, 
        fail: block_fail,
        dom_group: call_params.dom_group,
        prefer_shift: call_params.prefer_shift,
      });

      cur.seek(block_ok);

      None
    }
  }

  reduce_r![CallRed, ast::BuiltinAnyUtf8 => DefaultRule] {
    fn reduce(self, ctx, _node, cur) -> Option<CallInfo> {
      let block_b1 = ctx.block();
      let block_b2 = ctx.block();
      let block_ok = ctx.block();
      let block_fail = FailBlock::get_any(ctx);
      let call_params = call_red_params();

      let mut ok0 = IntervalMap::<u8, BlockRef>::new();
      ok0.insert(0x00..=0x7F, Some(block_ok.into()));
      ok0.insert(0x80..=0xFF, Some(block_b1.into()));

      let mut ok1 = IntervalMap::<u8, BlockRef>::new();
      ok1.insert(0x80..=0xFF, Some(block_b2.into()));

      cur.build_ctl(mir::CtlMatchClass { 
        ok: ok0, 
        fail: block_fail.clone(), 
        dom_group: call_params.dom_group,
        prefer_shift: call_params.prefer_shift,
      });

      cur.seek(block_b1);
      cur.build_ctl(mir::CtlMatchClass { 
        ok: ok1, 
        fail: block_fail, 
        dom_group: call_params.dom_group,
        prefer_shift: call_params.prefer_shift,
      });

      cur.seek(block_b2);
      cur.build_ctl(mir::CtlFork { blocks: vec![block_b1.into(), block_ok.into()] });

      cur.seek(block_ok);

      None
    }
  }

  reduce_r![CallRed, ast::BuiltinBoundary => DefaultRule] {
    fn reduce(self, ctx, _node, cur) -> Option<CallInfo> {
      gen_boundary(ctx, cur, &['a'..='z', 'A'..='Z', '0'..='9', '_'..='_'])
    }
  }

  reduce_r![CallRed, ast::BuiltinOperatorEnd => DefaultRule] {
    fn reduce(self, ctx, _node, cur) -> Option<CallInfo> {
      gen_boundary(ctx, cur, &['+'..='+', '-'..='-', '&'..='&', '='..='='])
    }
  }

  reduce_r![CallRed, ast::BuiltinWhitespaceEnd => DefaultRule] {
    fn reduce(self, ctx, _node, cur) -> Option<CallInfo> {
      gen_boundary(ctx, cur, &[' '..=' ', '\t'..='\t', '\n'..='\n', '\r'..='\r'])
    }
  }

  reduce_r![CallRed, ast::ItemGroup => DefaultRule] {
    fn reduce(self, _ctx, node, cur) -> Option<CallInfo> {
      let mut call_spec = BTreeMap::new();

      let match_id = MatchId::new(node.rule.target());
      let min_prec = node.prec.as_ref()
        .map(|p| p.data.value.parse::<u16>().unwrap())
        .unwrap_or(0);
      call_spec.insert(match_id, min_prec);

      let result = cur.build_stmt(mir::StmtCallRuleDyn {
        call_spec,
        args: vec![]
      });

      Some(CallInfo::new(match_id, result.up()))
    }
  }

  reduce_r![CallRed, ast::ItemRule => DefaultRule] {
    fn reduce(self, ctx, _node, cur) -> Option<CallInfo> {
      let match_id = MatchId::new(ctx.node_id());
      
      if let Some(call_id) = test_token_group(ctx) {
        let call_node = gen_call_dyn(cur, call_id);
        return Some(CallInfo::new(match_id, call_node));
      }
  
      let target = ctx.reduce_link_with(ValueRed, ctx.node_id(), cur);

      let result = cur.build_stmt(mir::StmtCallRule {
        target: target.unwrap(),
        match_id,
        min_prec: 0,
        args: vec![]
      });

      Some(CallInfo::new(match_id, result.up()))
    }
  }

  reduce_r![CallRed, ast::ItemRuleDyn => DefaultRule] {
    fn reduce(self, ctx, _node, cur) -> Option<CallInfo> {
      let match_id = MatchId::new(ctx.node_id());
      let call_node = gen_call_dyn(cur, match_id);
      Some(CallInfo::new(match_id, call_node))
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

lang_part! {
  ReduceCallPart {
    reduce_r![CallRed, ast::BuiltinAny => DefaultRule],
    reduce_r![CallRed, ast::BuiltinAnyUtf8 => DefaultRule],
    reduce_r![CallRed, ast::BuiltinBoundary => DefaultRule],
    reduce_r![CallRed, ast::BuiltinOperatorEnd => DefaultRule],
    reduce_r![CallRed, ast::BuiltinWhitespaceEnd => DefaultRule],
    reduce_r![CallRed, ast::ItemGroup => DefaultRule],
    reduce_r![CallRed, ast::ItemRule => DefaultRule],
    reduce_r![CallRed, ast::ItemRuleDyn => DefaultRule],
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

macro_rules! try_opt {
  ($expr:expr) => {
    match $expr {
      Some(expr) => expr,
      None => return None,
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

fn add_range(
  map: &mut IntervalMap<u8, BlockRef>, 
  range: RangeInclusive<char>,
  value: &BlockRef,
) {
  let start = *range.start() as u8;
  let end = *range.end() as u8;
  map.insert(start..=end, Some(value.clone()));
}

////////////////////////////////////////////////////////////////////////////////////////////////

fn erase_range(
  map: &mut IntervalMap<u8, BlockRef>, 
  range: RangeInclusive<char>,
) {
  let start = *range.start() as u8;
  let end = *range.end() as u8;
  map.insert(start..=end, None);
}

////////////////////////////////////////////////////////////////////////////////////////////////

fn gen_boundary(
  ctx: &mut ReduceCtxCore<CallRed>,
  cur: &mut Cursor,
  ranges: &[RangeInclusive<char>],
) -> Option<CallInfo> {
  let block_ok = ctx.block();
  let block_fail = FailBlock::get_any(ctx);
  let call_params = call_red_params();

  let mut ok = IntervalMap::<u8, _>::new();
  ok.insert(0..=255, Some(block_ok.into()));

  for range in ranges {
    erase_range(&mut ok, range.clone());
  }

  cur.build_ctl(mir::CtlMatchClass {
    ok, 
    fail: block_ok.into(), 
    dom_group: call_params.dom_group,
    prefer_shift: call_params.prefer_shift,
  });

  cur.seek(block_ok);
  cur.build_stmt(mir::StmtRewind { delta: 1 });

  None
}

////////////////////////////////////////////////////////////////////////////////////////////////

fn gen_call_dyn(
  cur: &mut Cursor,
  match_id: MatchId,
) -> NodeId<dyn mir::Call> {
  let mut call_spec = BTreeMap::new();
  call_spec.insert(match_id, 0);

  let result = cur.build_stmt(mir::StmtCallRuleDyn {
    call_spec,
    args: vec![]
  });

  result.up()
}

////////////////////////////////////////////////////////////////////////////////////////////////

fn test_token_group(
  ctx: &mut ReduceCtxCore<CallRed>,
) -> Option<MatchId> {
  let model = ctx.model();
  let group = try_opt!(model.find_ancestor::<_, ast::ItemGroup>(ctx.node_id()));
  let group = model.node(group);
  let attrs = try_opt!(group.attrs);
  let attrs = model.node(attrs);
  
  if !attrs.contains_attr::<ast::AttrTokenGroup>(&*model) {
    return None;
  }

  let match_id = MatchId::new(group.rule.target());
  Some(match_id)
}

////////////////////////////////////////////////////////////////////////////////////////////////

fn test_token_group2(
  ctx: &mut ReduceCtxCore<CallRed>,
) -> Option<MatchId> {
  let model = ctx.model();
  let curr = M::new(&*model, ctx.node_id());

  let group = try_opt!(curr.ancestor_id::<ast::ItemGroup>());
  let group = group.data();
  let attrs = try_opt!(group.map_opt(|g| g.attrs));
  let attrs = attrs.data();
  
  if !attrs.contains_attr_::<ast::AttrTokenGroup>() {
    return None;
  }

  let match_id = MatchId::new(group.rule.target());
  Some(match_id)
}

////////////////////////////////////////////////////////////////////////////////////////////////
