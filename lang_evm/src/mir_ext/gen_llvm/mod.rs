use {
  crate::{
    base_ext::gen_mir::PERSISTENT_BLOCK_ID,
    mir::{self},
    mir_ext::{
      gen_llvm::reduce_rule::{
        TASK_RESUME_FN_PARAMS,
        task_resume_params,
      }
    },
    optimizer::DFA,
    runtime::{
      self as rt, CallElem, MatchId, ParserCtx, ReduceId
    },
  },
  dsl_mir::prelude::*,
  interval_map::{IntervalMap},
  lang_llvm_sys::{
    BasicBlockRed, Builder, ContextExt, Native, ValueRed,
    dsl::{self, BuildItems as LLVMBuildItems},
    llvm_sys::{
      LLVMIntPredicate::{LLVMIntEQ, LLVMIntULT},
      prelude::*
    },
    serialize,
  },
  north_core::{
    cell::prelude::*,
  },
  north_derive::{aspect_rules, quote_mir, quote_mir_block},
  north_gen::prelude::*,
  std::{mem, ptr},
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub mod reduce_rule;

////////////////////////////////////////////////////////////////////////////////////////////////

struct DefaultRule;

aspect_rules! {
  reduce_r![ValueRed, mir::CtlExecDFA => DefaultRule] {
    fn reduce(self, ctx, node, builder) -> LLVMValueRef {
      let params = task_resume_params(ctx);

      let dfa_ptr = builder.quote_ptr(&*node.dfa as *const DFA);
      let dfa_ptr = dsl::RVal::new(dfa_ptr);
      let fail_block = ctx.reduce_ref_with(BasicBlockRed, &node.fail, builder);
      let cases = node.next.iter()
        .enumerate()
        .map(|(id, b)| {
          let id = builder.quote_const(&(id as u32));
          let b = ctx.reduce_ref_with(BasicBlockRed, b, builder);
          (dsl::RVal::new(id), b)
        })
        .collect::<Vec<_>>();

      builder.with_block(quote_mir_block! {
        _curr: {
          let result = builtin!(rt::TaskCtx::exec_dfa)(
            #{params.param_ctx}, #{params.param_self}, dfa_ptr
          );

          match result {
            default => fail_block,
            #(cases)*
          };
        }
      });

      ptr::null_mut()
    }
  }

  reduce_r![ValueRed, mir::CtlFail => DefaultRule] {
    fn reduce(self, ctx, _node, builder) -> LLVMValueRef {
      let params = task_resume_params(ctx);

      builder.with_block(quote_mir_block! {
        _curr: {
          builtin!(rt::TaskCtx::complete_task)(
            #{params.param_ctx}, #{params.param_self}
          );

          return;
        }
      });

      ptr::null_mut()
    }
  }

  reduce_r![ValueRed, mir::CtlFork => DefaultRule] {
    fn reduce(self, ctx, node, builder) -> LLVMValueRef {
      let params = task_resume_params(ctx);
      let head = &node.blocks[0];
      let head = ctx.reduce_ref_with(BasicBlockRed, head, builder);

      builder.with_block(quote_mir_block! {
        _curr: {
          #for# tail in node.blocks[1..].iter().rev() {
            #{let tail_id = cell_get!(PERSISTENT_BLOCK_ID(tail)) as u32;}*
            builtin!(rt::TaskCtx::fork_task)(
              #{params.param_ctx.clone()},
              #{params.param_self.clone()},
              #(tail_id)
            );
          }
          ctl_br!(head);
        }
      });

      ptr::null_mut()
    }
  }

  reduce_r![ValueRed, mir::CtlLoopEnd => DefaultRule] {
    fn reduce(self, ctx, node, builder) -> LLVMValueRef {
      let params = task_resume_params(ctx);
      let var = ctx.reduce_ref(&node.var, builder);
      let fail = ctx.reduce_ref_with(BasicBlockRed, &node.fail, builder);

      let ok = node.ok.entries()
        .map(|e| e.map_value_ref(|v| ctx.reduce_ref_with(BasicBlockRed, v, builder)))
        .collect::<IntervalMap<_, _>>()
        .into_entries();
      
      builder.with_block(quote_mir_block! {
        _curr: {
          let final_pos = stmt_use!(&var.0);
          let final_count = stmt_use!(&var.1);
          stmt_set!(#{params.position}, final_pos);
          match_range final_count {
            default => #{fail},
            #(ok)*
          };
        }
      });

      ptr::null_mut()
    }
  }

  reduce_r![ValueRed, mir::CtlLoopNext => DefaultRule] {
    fn reduce(self, ctx, node, builder) -> LLVMValueRef {
      let params = task_resume_params(ctx);
      let var = ctx.reduce_ref(&node.var, builder);
      let ok = ctx.reduce_ref_with(BasicBlockRed, &node.ok, builder);
      let complete = node.complete.as_ref().map(|b| {
        ctx.reduce_ref_with(BasicBlockRed, b, builder)
      });

      builder.with_block(quote_mir_block! {
        _curr: {
          let new_pos = stmt_use!(#{params.position});
          let new_count = stmt_bin_op!(#{BinOp::Add}, stmt_use!(&var.1), 1);
          stmt_set!(&var.0, new_pos);
          stmt_set!(&var.1, new_count);

          // TODO: if max_iter == 1, br to complete
          #if# let Some(max_iter) = node.max_iter {
            let max_reached = stmt_bin_op!(#{BinOp::CmpEq}, new_count, #(max_iter));
            ctl_if!(max_reached, #{complete.unwrap()}, ok);
          } else {
            ctl_br!(ok);
          }
        }
      });

      ptr::null_mut()
    }
  }

  reduce_r![ValueRed, mir::CtlMatchClass => DefaultRule] {
    fn reduce(self, ctx, node, builder) -> LLVMValueRef {
      let block_fail = ctx.reduce_ref_with(BasicBlockRed, &node.fail, builder);
      let input_chr = advance_char(ctx, builder, block_fail);

      // let slice = node.ok.as_ref();
      // let block_curr = builder.current_block().unwrap();
      // let block = gen_binary_search(ctx, input_chr, block_fail, builder, &None, slice);
      // builder.position_at_end(block_curr);
      // builder.build_br(block)

      let mut ok = node.ok.iter_raw()
        .cloned()
        .map(|v| v.map_key(|k| k as u32))
        .map(|v| v.map_value(|b| ctx.reduce_ref_with(BasicBlockRed, b, builder)))
        .collect::<IntervalMap<_, _>>();
      ok.insert_one(256, None);

      dsl::BuildCtlSwitchRange::gen_switch_2(
        builder,
        input_chr,
        block_fail,
        &ok
      );

      ptr::null_mut()
    }
  }

  reduce_r![ValueRed, mir::CtlMatchChar => DefaultRule] {
    fn reduce(self, ctx, node, builder) -> LLVMValueRef {
      let block_ok = ctx.reduce_ref_with(BasicBlockRed, &node.ok, builder);
      let block_fail = ctx.reduce_ref_with(BasicBlockRed, &node.fail, builder);

      let input_chr = advance_char(ctx, builder, block_fail);
      let target_chr = node.char as u8;
      let target_chr = serialize(ctx.llvm_ctx(), &target_chr).unwrap();

      let result = builder.build_icmp(LLVMIntEQ, input_chr, target_chr);
      builder.build_cond_br(result, block_ok, block_fail)
    }
  }

  reduce_r![ValueRed, mir::CtlMatchSym => DefaultRule] {
    fn reduce(self, ctx, node, builder) -> LLVMValueRef {
      let params = task_resume_params(ctx);

      let map_elem = |entry: &mir::CtlMatchSymEntry| {
        let state_id = cell_get!(PERSISTENT_BLOCK_ID(&entry.block));
        rt::MatchElem {
          match_id: entry.match_id,
          prec_range: entry.prec_range.clone(),
          state_id: state_id.into(),
        }
      };

      let pos_elems = node.pos.iter().map(map_elem).collect::<Vec<_>>();
      let parser_ctx = ParserCtx::get();
      let match_interner = parser_ctx.match_spec_interner();
      let pos_spec_id = match_interner.intern(pos_elems);

      let neg_elems = node.neg.iter().map(map_elem).collect::<Vec<_>>();
      let neg_elems_len = neg_elems.len();

      builder.with_block(quote_mir_block! {
        _curr: {
          //static POS_SUSPEND_SPEC: [rt::MatchElem; pos_elems_len] = #(pos_elems);
          //let pos_suspend_data = rval_cast!(POS_SUSPEND_SPEC, <*const rt::MatchElem>);

          static NEG_SUSPEND_SPEC: [rt::MatchElem; neg_elems_len] = #(neg_elems);
          let neg_suspend_data = rval_cast!(NEG_SUSPEND_SPEC, <*const rt::MatchElem>);

          builtin!(rt::TaskCtx::add_suspend)(
            #{params.param_ctx}, 
            #{params.param_self},
            #(pos_spec_id),
            (neg_suspend_data, #(neg_elems_len)),
            //(pos_suspend_data, #(pos_elems_len), 0usize), // WTF is that 0
          );

          return;
        }
      });

      ptr::null_mut()
    }
  }

  reduce_r![ValueRed, mir::CtlReduce => DefaultRule] {
    fn reduce(self, ctx, node, builder) -> LLVMValueRef {
      let params = task_resume_params(ctx);
      let kind: u8 = unsafe { mem::transmute_copy(&node.kind) };

      let next = ctx.reduce_ref_with(BasicBlockRed, &node.next, builder);
      let fail = ctx.reduce_ref_with(BasicBlockRed, &node.fail, builder);

      builder.with_block(quote_mir_block! {
        _curr: {
          #if# let true = node.short {
            let added = builtin!(rt::TaskCtx::add_reduce_short)(
              #{params.param_ctx}, 
              #{params.param_self},
              #(kind), 
              #(node.reduce_id),
            );

            ctl_if!(added, next, fail);
          } else {
            let added = builtin!(rt::TaskCtx::add_reduce)(
              #{params.param_ctx}, 
              #{params.param_self},
              #(kind), 
              #(node.reduce_id),
            );

            ctl_if!(added, next, fail);
          }

          // let added = builtin!(rt::TaskCtx::add_reduce)(
          //   #{params.param_ctx}, 
          //   #{params.param_self},
          //   #(kind), 
          //   #(node.reduce_id),
          // );

          // ctl_if!(added, next, fail);
          // ctl_br!(next);
        }
      });

      ptr::null_mut()
    }
  }

  reduce_r![ValueRed, mir::ItemGrammar => DefaultRule] {
    fn reduce(self, ctx, node, builder) -> LLVMValueRef {
      let context = ctx.inner.llvm_ctx.clone();
      let mut b = LLVMBuildItems::new(context, ctx.curr_mod());
      let b = &mut b;

      let map_entry = |e: &mir::GrammarEntry| GrammarEntry {
        match_id  : e.match_id,
        reduce_id : e.reduce_id,
        prec      : e.prec,
        ctor_fn   : Native(ctx.reduce_ref(&e.rule, builder)),
      };

      let entries = node.impls
        .iter()
        .map(map_entry)
        .collect::<Vec<_>>();
      let entries_len = entries.len();
      
      quote_mir! {
        static GRAMMAR_DATA: [GrammarEntry; entries_len] = #(entries);
        static GRAMMAR: (*const (), usize) = (GRAMMAR_DATA, #(entries_len));
      }

      GRAMMAR.raw()
    }
  }

  reduce_r![ValueRed, mir::RuleLocal => DefaultRule] {
    fn reduce(self, ctx, node, builder) -> LLVMValueRef {
      let params = task_resume_params(ctx);
      builder.build_struct_gep(params.data.raw(), node.index + 5)
    }
  }

  reduce_r![ValueRed, mir::RuleParam => DefaultRule] {
    fn reduce(self, _ctx, _node, _builder) -> LLVMValueRef {
      //let params = task_resume_params(ctx);
      //builder.build_struct_gep(params.data.raw(), node.index + 4)
      unimplemented!("RuleParam needs reduce")
    }
  }

  reduce_r![ValueRed, mir::StmtCallRule => DefaultRule] {
    fn reduce(self, ctx, node, builder) -> LLVMValueRef {
      let params = task_resume_params(ctx);
      let ctor = ctx.reduce_link(&node.target, builder).into();
      
      builder.with_block(quote_mir_block! {
        _curr: {
          let position = stmt_use!(#{params.position});
          let ctor_args = rval_cast!(0usize, <*const ()>);
          let task_box = #{ctor}(position, ctor_args);
          builtin!(rt::TaskCtx::add_task)(#{params.param_ctx}, task_box);
        }
      });

      ptr::null_mut()
    }
  }

  reduce_r![ValueRed, mir::StmtCallRuleDyn => DefaultRule] {
    fn reduce(self, ctx, node, builder) -> LLVMValueRef {
      let params = task_resume_params(ctx);

      let spec = node.call_spec.iter()
        .map(|(m, p)| CallElem { match_id: *m, min_prec: *p })
        .collect::<Vec<_>>();
      let parser_ctx = ParserCtx::get();
      let call_interner = parser_ctx.call_spec_interner();
      let call_spec_id = call_interner.intern(spec);

      builder.with_block(quote_mir_block! {
        _curr: {
          //static CALL_SPEC_DATA: [(u16, u16); spec_len] = #(spec);
          //let spec_data = rval_cast!(CALL_SPEC_DATA, <*const (u16, u16)>);
          // (spec_data, #(spec_len)),

          let data = rval_cast!(#{params.data}, <*const rt::TaskData>);
          let ctor_args = rval_cast!(0usize, <*const ()>);
          
          builtin!(rt::TaskCtx::call_rule_dyn)(
            #{params.param_ctx},
            data,
            #(call_spec_id),
            ctor_args
          );
        }
      });

      ptr::null_mut()
    }
  }

  reduce_r![ValueRed, mir::StmtLoopBegin => DefaultRule] {
    fn reduce(self, ctx, node, builder) -> LLVMValueRef {
      let params = task_resume_params(ctx);
      let var = ctx.reduce_ref(&node.var, builder);

      builder.with_block(quote_mir_block! {
        _curr: {
          let init_pos = stmt_use!(#{params.position});
          let init_count = 0u32;
          stmt_set!(&var.0, init_pos);
          stmt_set!(&var.1, init_count);
        }
      });

      ptr::null_mut()
    }
  }

  reduce_r![ValueRed, mir::StmtReduce => DefaultRule] {
    fn reduce(self, ctx, node, builder) -> LLVMValueRef {
      let params = task_resume_params(ctx);
      let kind: u8 = unsafe { mem::transmute_copy(&node.kind) };

      builder.with_block(quote_mir_block! {
        _curr: {
          builtin!(rt::TaskCtx::add_reduce)(
            #{params.param_ctx}, 
            #{params.param_self},
            #(kind),
            #(node.reduce_id),
          );
        }
      });

      ptr::null_mut()
    }
  }

  reduce_r![ValueRed, mir::StmtRewind => DefaultRule] {
    fn reduce(self, ctx, node, builder) -> LLVMValueRef {
      let params = task_resume_params(ctx);

      builder.with_block(quote_mir_block! {
        _curr: {
          let pos = stmt_use!(#{params.position});
          let pos = stmt_bin_op!(#{BinOp::Sub}, pos, #(node.delta as i32));
          stmt_set!(#{params.position}, pos);
        }
      });

      ptr::null_mut()
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

lang_part! {
  ReduceCtlPart {
    reduce_r![ValueRed, mir::CtlExecDFA => DefaultRule],
    reduce_r![ValueRed, mir::CtlFail => DefaultRule],
    reduce_r![ValueRed, mir::CtlFork => DefaultRule],
    reduce_r![ValueRed, mir::CtlLoopEnd => DefaultRule],
    reduce_r![ValueRed, mir::CtlLoopNext => DefaultRule],
    reduce_r![ValueRed, mir::CtlMatchClass => DefaultRule],
    reduce_r![ValueRed, mir::CtlMatchChar => DefaultRule],
    reduce_r![ValueRed, mir::CtlMatchSym => DefaultRule],
    reduce_r![ValueRed, mir::CtlReduce => DefaultRule],
    reduce_r![ValueRed, mir::ItemGrammar => DefaultRule],
    reduce_r![ValueRed, mir::RuleLocal => DefaultRule],
    reduce_r![ValueRed, mir::RuleParam => DefaultRule],
    reduce_r![ValueRed, mir::StmtCallRule => DefaultRule],
    reduce_r![ValueRed, mir::StmtCallRuleDyn => DefaultRule],
    reduce_r![ValueRed, mir::StmtLoopBegin => DefaultRule],
    reduce_r![ValueRed, mir::StmtReduce => DefaultRule],
    reduce_r![ValueRed, mir::StmtRewind => DefaultRule],
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Serialize)]
struct GrammarEntry {
  match_id: MatchId,
  reduce_id: ReduceId,
  prec: u16,
  ctor_fn: Native,
}

use north_core::quote::{
  QuoteType, RustQuoter, rust_type
};

impl QuoteType for GrammarEntry {
  fn quote(q: &mut RustQuoter) -> rust_type::Type {
    rust_type::Type::make_struct(vec![
      q.quote::<MatchId>(),
      q.quote::<ReduceId>(),
      q.quote::<u16>(),
      q.quote::<*const ()>(),
    ])
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

/*

quote_mir! {
  entry: {
    let cur_0 = stmt_use!(p_cur_0);
    let cur_1 = stmt_use!(p_cur_1);
    let ok = stmt_bin_op!(LESS, cur_0, cur_1);
    ctl_if!(ok, ok_b, #fail);
  }

  ok_b: {
    let curr_chr = stmt_use!(cur_0);
    let cur_0 = stmt_bin_op!(ADD, cur_0, 1);
    stmt_set!(p_cur_0, cur_0);
  }
}

curr_chr.raw()

*/

fn advance_char(
  core  : &ReduceCtxCore<ValueRed>,
  b     : &mut Builder,
  fail  : LLVMBasicBlockRef
) -> LLVMValueRef {
  /*let curr_rule = core.find_ancestor_id::<mir::ItemRule>().unwrap();
  let params = cell_get!(TASK_RESUME_FN_PARAMS(curr_rule));
  let cursor_0 = params.cursor_0;
  let cursor_1 = params.cursor_1;

  let ptr_start = b.build_load(cursor_0);
  let ptr_end = b.build_load(cursor_1);
  let result = b.build_icmp(LLVMIntULT, ptr_start, ptr_end);

  let ok = core.make_bb();
  b.build_cond_br(result, ok, fail);

  b.position_at_end(ok);
  let cst_1 = core.llvm_ctx().quote_const(&1);
  let curr_chr = b.build_load(ptr_start);
  let ptr_start = b.build_in_bounds_gep(ptr_start, &mut [cst_1]);
  b.build_store(ptr_start, cursor_0);

  curr_chr*/
  panic!("NO_ADVANCE")
}

////////////////////////////////////////////////////////////////////////////////////////////////

// type Entry = interval_map::Entry<u32, BlockRef>;

// fn gen_binary_search(
//   core      : &ReduceCtxCore<ValueRed>,
//   curr_chr  : LLVMValueRef,
//   fail      : LLVMBasicBlockRef,
//   b         : &mut Builder,
//   value     : &Option<BlockRef>,
//   slice     : &[Entry],
// ) -> LLVMBasicBlockRef
// {
//   if slice.is_empty() {
//     return match value {
//       Some(block) => core.reduce_ref_with(BasicBlockRed, block, b),
//       None => fail,
//     };
//   }

//   let middle = slice.len() / 2;
//   let left = &slice[0..middle];
//   let curr = &slice[middle];
//   let right = &slice[(middle + 1)..];

//   let block_left = gen_binary_search(core, curr_chr, fail, b, value, left);
//   let block_right = gen_binary_search(core, curr_chr, fail, b, &curr.value, right);

//   let block = core.make_bb();
//   b.position_at_end(block);

//   let key = curr.key as u8;
//   let key_const = core.llvm_ctx().quote_const(&key);
//   let result = b.build_icmp(LLVMIntULT, curr_chr, key_const);
//   b.build_cond_br(result, block_left, block_right);

//   block
// }

////////////////////////////////////////////////////////////////////////////////////////////////
