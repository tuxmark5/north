use {
  crate::{
    ast,
    base_ext::{
      gen_mir::{
        ITER_DEPTH, FailBlock, get_iter_depth,
        reduction::{CALL_RED_PARAMS, CallRed, CallRedCtx, GreedyRed, LazyRed},
      },
      sema::{get_prefer_shift},
    },
    mir::{self, RValLinkOpt},
    runtime::PosId,
  },
  interval_map::IntervalMap,
  lang_base::token,
  lang_mir::{
    CodegenCtxExt, Cursor, IntoBlockId, ValueRed,
    mir::BlockRef,
  },
  north_core::cell::prelude::*,
  north_derive::{aspect_rules},
  north_gen::prelude::*,
};

////////////////////////////////////////////////////////////////////////////////////////////////

struct DefaultRule;

aspect_rules! {
  reduce_r![ValueRed, ast::GExprCall => DefaultRule] {
    fn reduce(self, ctx, node, cur) -> RValLinkOpt {
      let target = node.target.target();

      let recursion_delta = if node.recursive { 0u16 } else { 1u16 };
      let mut min_prec = match node.precedence {
        Some(prec) => prec,
        None => match get_call_precedence(ctx, target) {
          Some(prec) => prec + recursion_delta,
          None => 0u16,
        }
      };

      // Generate call statement and initialize min_prec and args
      let match_id = {
        let shift_opts = get_prefer_shift(ctx); // need callee info
        let call_params = CallRedCtx { 
          dom_group: shift_opts.0,
          prefer_shift: shift_opts.1,
        };

        let call_info = CALL_RED_PARAMS.set(&call_params, || {
          ctx.reduce_cld_with(CallRed, target, cur)
        });

        call_info.map(|call_info| {
          let call_node = call_info.call_node;
          let mut call_stmt = ctx.model_cell().node_mut(call_node);
          call_stmt.init(&mut min_prec, vec![]);
          call_info.match_id
        })
      };

      if let Some(match_id) = match_id {
        let prec_range = min_prec..=std::u16::MAX;
        let end = ctx.block();

        let pos = vec![mir::CtlMatchSymEntry {
          match_id,
          prec_range: prec_range.clone(),
          block: end.into(),
        }];

        let neg = if let Some(block_fail) = FailBlock::get_partial(ctx) {
          vec![mir::CtlMatchSymEntry {
            match_id,
            prec_range,
            block: block_fail,
          }]
        } else {
          Vec::new()
        };

        cur.build_ctl(mir::CtlMatchSym { pos, neg });
        cur.seek(end);
      }

      None
    }
  }

  reduce_r![ValueRed, ast::GExprDom => DefaultRule] {
    fn reduce(self, ctx, node, cur) -> RValLinkOpt {
      ctx.reduce_cld(node.inner, cur)
    }
  }

  reduce_r![GreedyRed, ast::GExprList => DefaultRule] {
    fn reduce(self, _ctx, _node, _cur) -> RValLinkOpt {
      unimplemented!("not implemented yet")
    }
  }

  reduce_r![LazyRed, ast::GExprList => DefaultRule] {
    fn reduce(self, ctx, node, cur) -> RValLinkOpt {
      let mut block_start = ctx.block_start(cur);
      let mut block_elem = ctx.block_cur();
      let mut block_sep = ctx.block_cur();
      let block_end = ctx.block();

      cur.seek(block_end);

      match (node.min_elems, node.trailing_sep) {
        (0, true) => {
          ctl_fork_2(&mut block_start, &block_elem, block_end);
          ctx.reduce_cld_with(ValueRed, node.left, &mut block_elem);
          ctl_fork_2(&mut block_elem, &block_sep, block_end);
          ctx.reduce_cld_with(ValueRed, node.right, &mut block_sep);
          block_sep.ctl_br(&block_start);
        },

        (0, false) => {
          ctl_fork_2(&mut block_start, &block_elem, block_end);
          ctx.reduce_cld_with(ValueRed, node.left, &mut block_elem);
          ctl_fork_2(&mut block_elem, &block_sep, block_end);
          ctx.reduce_cld_with(ValueRed, node.right, &mut block_sep);
          block_sep.ctl_br(&block_elem);
        },

        (1, true) => {
          block_start.ctl_br(&block_elem);
          ctx.reduce_cld_with(ValueRed, node.left, &mut block_elem);
          ctl_fork_2(&mut block_elem, &block_sep, block_end);
          ctx.reduce_cld_with(ValueRed, node.right, &mut block_sep);
          ctl_fork_2(&mut block_sep, &block_elem, block_end);
        },

        (1, false) => {
          block_start.ctl_br(&block_elem);
          ctx.reduce_cld_with(ValueRed, node.left, &mut block_elem);
          ctl_fork_2(&mut block_elem, &block_sep, block_end);
          ctx.reduce_cld_with(ValueRed, node.right, &mut block_sep);
          block_sep.ctl_br(&block_elem);
        },

        _ => {
          panic!("bad combo");
        }
      }

      None
    }
  }

  reduce_r![ValueRed, ast::GExprList => DefaultRule] {
    fn reduce(self, ctx, _node, cur) -> RValLinkOpt {
      match is_greedy(ctx) {
        true => ctx.reduce_cld_with(GreedyRed, ctx.node_id(), cur),
        false => ctx.reduce_cld_with(LazyRed, ctx.node_id(), cur),
      }
    }
  }

  reduce_r![ValueRed, ast::GExprLitClass => DefaultRule] {
    fn reduce(self, ctx, node, cur) -> RValLinkOpt {
      let next_block = ctx.block();
      let shift_opts = get_prefer_shift(ctx);

      let mut cases = IntervalMap::<u8, BlockRef>::new();
      for group in &node.lit.data.groups {
        use self::token::LitCharClassGroup::*;
        match group {
          Any => {
            cases.add_negative(next_block.into());
          },

          Range(from, to) => {
            let from = *from as u8;
            let to = *to as u8 + 1;
            cases.insert(from..to, Some(next_block.into()));
          },

          Single(char) => {
            let from = *char as u8;
            let to = *char as u8 + 1;
            cases.insert(from..to, Some(next_block.into()));
          },
        }
      }

      if node.lit.data.invert {
        cases.invert(next_block.into());
      }

      let block_fail = FailBlock::get(ctx);
      if let FailBlock::Partial(id) = block_fail {
        cases.add_negative(id.into());
      }

      cases.dedup();

      cur.build_ctl(mir::CtlMatchClass {
        ok: cases,
        fail: block_fail.to_ref(),
        dom_group: shift_opts.0,
        prefer_shift: shift_opts.1,
      });

      cur.seek(next_block);

      None
    }
  }

  reduce_r![ValueRed, ast::GExprLitStr => DefaultRule] {
    fn reduce(self, ctx, node, cur) -> RValLinkOpt {
      let block_fail = FailBlock::get_any(ctx);
      let shift_opts = get_prefer_shift(ctx);
      let string_bytes = &node.lit.data.value.as_bytes();

      for (i, char) in string_bytes.iter().enumerate() {
        let block_next = ctx.block();
        let is_last = i == string_bytes.len() - 1;
        let dom_group = if is_last { shift_opts.0 } else { 0 };

        cur.build_ctl(mir::CtlMatchChar {
          char: *char as u32,
          dom_group,
          prefer_shift: shift_opts.1,
          ok: block_next.into(),
          fail: block_fail.clone(),
        });

        cur.seek(block_next);
      }
      None
    }
  }

  reduce_r![ValueRed, ast::GExprOpAlt => DefaultRule] {
    fn reduce(self, ctx, node, cur) -> RValLinkOpt {
      let block_end = ctx.block();

      let blocks = node.args.iter()
        .map(|a| {
          let mut cur = ctx.block_cur();
          ctx.reduce_cld(a, &mut cur);
          cur.ctl_br(block_end);
          cur.orig_block_id().into()
        })
        .collect::<Vec<_>>();

      cur.build_ctl(mir::CtlFork { blocks });
      cur.seek(block_end);

      None
    }
  }

  reduce_r![GreedyRed, ast::GExprOpOpt => DefaultRule] {
    fn reduce(self, ctx, node, cur) -> RValLinkOpt {
      gen_greedy_loop(ctx, node.inner, cur, 0, Some(1));
      None
    }
  }

  reduce_r![LazyRed, ast::GExprOpOpt => DefaultRule] {
    fn reduce(self, ctx, node, cur) -> RValLinkOpt {
      let block_end = ctx.block();
      let mut block_inner = ctx.block_cur();

      ctl_fork_2(cur, &block_inner, block_end);

      ctx.reduce_cld_with(ValueRed, node.inner, &mut block_inner);
      block_inner.ctl_br(block_end);

      cur.seek(block_end);

      None
    }
  }

  reduce_r![ValueRed, ast::GExprOpOpt => DefaultRule] {
    fn reduce(self, ctx, _node, cur) -> RValLinkOpt {
      match is_greedy(ctx) {
        true => ctx.reduce_cld_with(GreedyRed, ctx.node_id(), cur),
        false => ctx.reduce_cld_with(LazyRed, ctx.node_id(), cur),
      }
    }
  }

  reduce_r![GreedyRed, ast::GExprOpPlus => DefaultRule] {
    fn reduce(self, ctx, node, cur) -> RValLinkOpt {
      gen_greedy_loop(ctx, node.inner, cur, 1, None);
      None
    }
  }

  reduce_r![LazyRed, ast::GExprOpPlus => DefaultRule] {
    fn reduce(self, ctx, node, cur) -> RValLinkOpt {
      let mut block_start = ctx.block_start(cur);
      let block_start_id = block_start.orig_block_id();
      let block_end = ctx.block();

      ctx.reduce_cld_with(ValueRed, node.inner, &mut block_start);
      ctl_fork_2(&mut block_start, block_start_id, block_end);

      cur.seek(block_end);

      None
    }
  }

  reduce_r![ValueRed, ast::GExprOpPlus => DefaultRule] {
    fn reduce(self, ctx, _node, cur) -> RValLinkOpt {
      match is_greedy(ctx) {
        true => ctx.reduce_cld_with(GreedyRed, ctx.node_id(), cur),
        false => ctx.reduce_cld_with(LazyRed, ctx.node_id(), cur),
      }
    }
  }

  reduce_r![ValueRed, ast::GExprOpSome => DefaultRule] {
    fn reduce(self, ctx, node, cur) -> RValLinkOpt {
      let block_end = ctx.block();

      let mut blocks = Vec::new();
      let mut cursors = Vec::new();

      for arg in &node.args {
        let mut cur = ctx.block_cur();
        ctx.reduce_cld(arg, &mut cur);
        blocks.push(cur.orig_block_id().into());
        cursors.push(cur);
      }

      cur.build_ctl(mir::CtlFork { 
        blocks: blocks.clone() 
      });

      for (index, cur) in cursors.iter_mut().enumerate() {
        let mut rest = blocks[index + 1..].to_owned();
        rest.push(block_end.into());
        cur.build_ctl(mir::CtlFork { blocks: rest });
      }

      cur.seek(block_end);

      None
    }
  }

  reduce_r![GreedyRed, ast::GExprOpStar => DefaultRule] {
    fn reduce(self, ctx, node, cur) -> RValLinkOpt {
      gen_greedy_loop(ctx, node.inner, cur, 0, None);
      None
    }
  }

  reduce_r![LazyRed, ast::GExprOpStar => DefaultRule] {
    fn reduce(self, ctx, node, cur) -> RValLinkOpt {
      let mut block_start = ctx.block_start(cur);
      let block_end = ctx.block();
      let mut block_inner = ctx.block_cur();

      ctl_fork_2(&mut block_start, &block_inner, block_end);

      ctx.reduce_cld_with(ValueRed, node.inner, &mut block_inner);
      block_inner.ctl_br(&block_start);

      cur.seek(block_end);

      None
    }
  }

  reduce_r![ValueRed, ast::GExprOpStar => DefaultRule] {
    fn reduce(self, ctx, _node, cur) -> RValLinkOpt {
      match is_greedy(ctx) {
        true => ctx.reduce_cld_with(GreedyRed, ctx.node_id(), cur),
        false => ctx.reduce_cld_with(LazyRed, ctx.node_id(), cur),
      }
    }
  }

  reduce_r![ValueRed, ast::GExprPrefShift => DefaultRule] {
    fn reduce(self, ctx, node, cur) -> RValLinkOpt {
      ctx.reduce_cld(node.inner, cur)
    }
  }

  reduce_r![ValueRed, ast::GExprSeq => DefaultRule] {
    fn reduce(self, ctx, node, cur) -> RValLinkOpt {
      for arg in &node.args {
        let _ = ctx.reduce_cld(arg, cur);
      }
      None
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

lang_part! {
  ReduceGExprPart {
    reduce_r![ValueRed , ast::GExprCall      => DefaultRule],
    reduce_r![ValueRed , ast::GExprDom       => DefaultRule],
    reduce_r![GreedyRed, ast::GExprList      => DefaultRule],
    reduce_r![LazyRed  , ast::GExprList      => DefaultRule],
    reduce_r![ValueRed , ast::GExprList      => DefaultRule],
    reduce_r![ValueRed , ast::GExprLitClass  => DefaultRule],
    reduce_r![ValueRed , ast::GExprLitStr    => DefaultRule],
    reduce_r![ValueRed , ast::GExprOpAlt     => DefaultRule],
    reduce_r![GreedyRed, ast::GExprOpOpt     => DefaultRule],
    reduce_r![LazyRed  , ast::GExprOpOpt     => DefaultRule],
    reduce_r![ValueRed , ast::GExprOpOpt     => DefaultRule],
    reduce_r![GreedyRed, ast::GExprOpPlus    => DefaultRule],
    reduce_r![LazyRed  , ast::GExprOpPlus    => DefaultRule],
    reduce_r![ValueRed , ast::GExprOpPlus    => DefaultRule],
    reduce_r![ValueRed , ast::GExprOpSome    => DefaultRule],
    reduce_r![GreedyRed, ast::GExprOpStar    => DefaultRule],
    reduce_r![LazyRed  , ast::GExprOpStar    => DefaultRule],
    reduce_r![ValueRed , ast::GExprOpStar    => DefaultRule],
    reduce_r![ValueRed , ast::GExprPrefShift => DefaultRule],
    reduce_r![ValueRed , ast::GExprSeq       => DefaultRule],
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

fn ctl_fork_2<A, B>(cur: &mut Cursor, block_a: A, block_b: B) where
  A: IntoBlockId, B: Clone + IntoBlockId
{
  let block_a = block_a.into_block_id().into();
  let block_b = block_b.into_block_id().into();
  cur.build_ctl(mir::CtlFork {
    blocks: vec![block_a, block_b],
  });
}

////////////////////////////////////////////////////////////////////////////////////////////////

// fn find_rule_ancestor(ctx: &ReduceCtxCore<ValueRed>) -> NodeId<ast::ItemRule> {
//   ctx.find_ancestor_id::<ast::ItemRule>().unwrap()
// }

////////////////////////////////////////////////////////////////////////////////////////////////

fn get_call_precedence(
  ctx: &ReduceCtxCore<ValueRed>, 
  target_id: NodeId
) -> Option<u16> {
  let model = ctx.model();
  let rule_id = model.find_ancestor::<_, ast::ItemRule>(ctx.node_id()).unwrap();
  
  let rule_parent = model.parent_id(rule_id).unwrap();
  if let Some(group_node) = model.get::<_, ast::ItemGroup>(rule_parent) {
    if group_node.rule.target() == target_id {
      if let Some(prec) = &group_node.prec {
        return Some(prec.data.value.parse::<u16>().unwrap());
      }
    }
  }

  let rule_node = model.node(rule_id);
  let result = rule_node.attrs_filter::<ast::AttrPartOf>(&*model)
    .find(|attr| attr.rule.target() == target_id)
    .map(|attr| attr.prec());

  result
}

////////////////////////////////////////////////////////////////////////////////////////////////

fn is_greedy(ctx: &ReduceCtxCore<ValueRed>) -> bool {
  let model = ctx.model();
  let expr_id = model.find_ancestor::<_, ast::ExprParse>(ctx.node_id()).unwrap();
  let expr = model.node(expr_id);
  expr.greedy.unwrap_or(false)
}

////////////////////////////////////////////////////////////////////////////////////////////////

// (A S)*
// 10: A, FAIL: 90
// 20: S, FAIL: 90
// 30: NEXT 10
// 90: CTL_END

// (A % S)*  (A % S)+
// (A S)* with last S optional
// 10: A, FAIL: 90
// 15: STORE_POS
// 20: S, FAIL: 90
// 30: NEXT 10 
// 90: CTL_END

// (A %% S)*  (A %% S)+
// (A S)* with required seps in between
// 10: A, FAIL: 90
// 15: NEXT 20
// 20: S, FAIL: 90
// 30: GOTO 10
// 90: CTL_END


/*

blocks! {
  entry: {
    let var = new_local!((*const u8, u32));
    ctl_loop_begin!(var, main);
  }

  main: {
    reduce!(inner);
    ctl_loop_next!(var, #max_iter, main, end);
  }

  complete: {
    ctl_loop_end!(var, fail, #range => #end);
  }

  end: { }
}

*/


fn gen_greedy_loop(
  ctx     : &ReduceCtxCore<GreedyRed>,
  inner   : NodeId<dyn ast::GExpr>,
  cur     : &mut Cursor,
  min_iter: u32,
  max_iter: Option<u32>,
) {
  let ty = ctx.quote_type::<(PosId, u32)>();
  let var = ctx.make_local(ty).cast();

  let depth = get_iter_depth(ctx);
  cell_set!(ITER_DEPTH(ctx.node_id()), depth + 1);

  let fail = FailBlock::get(ctx);
  let main = ctx.block();
  let complete = ctx.block();
  let end = ctx.block();  

  let mut ok = IntervalMap::<u32, BlockRef>::new();
  {
    let end_opt = Some(end.into());
    match max_iter {
      Some(max_iter) => ok.insert(min_iter..max_iter, end_opt),
      None => ok.insert(min_iter.., end_opt),
    }

    if let FailBlock::Partial(fail_id) = fail {
      ok.add_negative(fail_id.into());
    }
  }

  cur.build_stmt(mir::StmtLoopBegin {
    var: var.clone(),
    depth,
  });
  cur.ctl_br(main);

  cur.seek(main);
  FailBlock::set(FailBlock::Partial(complete), || {
    ctx.reduce_cld_with(ValueRed, inner, cur);
  });
  
  cur.build_ctl(mir::CtlLoopNext {
    var: var.clone(),
    max_iter,
    ok: main.into(),
    complete: Some(end.into()),
  });

  cur.seek(complete);
  cur.build_ctl(mir::CtlLoopEnd {
    var,
    ok,
    fail: fail.to_ref(),
  });

  cur.seek(end);
}

////////////////////////////////////////////////////////////////////////////////////////////////
