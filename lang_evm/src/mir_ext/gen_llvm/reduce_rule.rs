use {
  crate::{
    alloc::{ABox},
    base_ext::gen_mir::PERSISTENT_BLOCK_ID,
    mir::{self},
    runtime::{self as rt},
  },
  dsl_mir::*,
  lang_llvm_sys::{
    BasicBlockRed, Builder, ContextExt, TypeRed, ValueRed,
    dsl::{self, BuildItems as LLVMBuildItems},
    llvm_sys::{prelude::*},
  },
  north_core::{
    cell::prelude::*,
    context::{ModelNodeIdCtxExt},
    quote::{*, rust_type::Type},
  },
  north_derive::{aspect_rules, cell, quote_mir},
  north_gen::prelude::*,
};

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone)]
pub struct ResumeFnParams {
  pub clone_fn    : dsl::RVal,
  // pub cursor      : LLVMValueRef,
  // pub cursor_0    : LLVMValueRef,
  // pub cursor_1    : LLVMValueRef,
  pub data        : dsl::RVal,
  pub param_ctx   : dsl::RVal,
  pub param_self  : dsl::RVal,
  pub position    : LLVMValueRef,
  pub vtable      : dsl::RVal,
}

#[cell(scope(compiler, node))]
pub static TASK_RESUME_FN_PARAMS: ResumeFnParams = ();

pub fn task_resume_params<R: Reduction>(ctx: &mut ReduceCtxCore<R>) -> ResumeFnParams {
  let curr_rule = ctx.find_ancestor_id::<mir::ItemRule>().unwrap();
  cell_get!(TASK_RESUME_FN_PARAMS(curr_rule))
}

////////////////////////////////////////////////////////////////////////////////////////////////

struct DefaultRule;

aspect_rules! {
  reduce_r![ValueRed, mir::ItemRule => DefaultRule] {
    fn reduce(self, ctx, node, _builder) -> LLVMValueRef {
      let context = ctx.inner.llvm_ctx.clone();
      let mut b = LLVMBuildItems::new(context, ctx.curr_mod());
      gen_rule(ctx, node, &mut b)
    }

    fn reduce_late(self, ctx, node, builder, raw_fn) {
      let context = ctx.inner.llvm_ctx.clone();
      let mut b = LLVMBuildItems::new(context, ctx.curr_mod());
      gen_rule_complete(ctx, node, &mut b, builder, raw_fn);
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

lang_part! {
  ReduceRulePart {
    reduce_r![ValueRed, mir::ItemRule => DefaultRule],
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

struct TaskVTable {
  pub dtor: *const (),
  pub size: usize,
  pub align: usize,
  pub clone: *const (),
  pub eq: *const (),
  pub hash: *const (),
  pub resume: *const (),
}

impl QuoteType for TaskVTable {
  fn quote(q: &mut RustQuoter) -> Type {
    Type::make_struct(vec![
      q.quote::<*const ()>(),
      q.quote::<*const usize>(),
      q.quote::<*const usize>(),
      q.quote::<*const ()>(),
      q.quote::<*const ()>(),
      q.quote::<*const ()>(),
      q.quote::<*const ()>(),
    ])
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

fn gen_rule(
  _core   : &mut ReduceCtxCore<ValueRed>, 
  rule    : &mir::ItemRule, 
  b       : &mut LLVMBuildItems,
) -> LLVMValueRef {
  quote_mir! {
    fn #(ctor: rule.name.as_ref())(
      _position: rt::PosId,
      _user_args: *const ()
    ) -> ABox<dyn rt::Task> { }
  }

  ctor.raw()
}

////////////////////////////////////////////////////////////////////////////////////////////////

fn gen_rule_complete(
  core    : &mut ReduceCtxCore<ValueRed>, 
  rule    : &mir::ItemRule, 
  b       : &mut LLVMBuildItems,
  builder : &mut Builder,
  raw_fn  : LLVMValueRef,
) {
  let raw_fn = raw_fn.into();
  let blocks = &rule.blocks;
  let entry_block_id = blocks[0].idx() as u32;

  let mut cases = Vec::new();

  let _user_params;
  let user_locals;
  {
    let model = core.model();

    _user_params = model.nodes(&rule.params)
      .map(|n| core.reduce_link_with(TypeRed, &n.ty, &mut ()))
      .collect::<Vec<_>>();

    user_locals = model.nodes(&rule.locals)
      .map(|n| core.reduce_link_with(TypeRed, &n.ty, &mut ()))
      .collect::<Vec<_>>();
  }

  let init_locals = user_locals.iter()
    .map(|ty| b.const_null(*ty))
    .collect::<Vec<_>>();

  quote_mir! {
    type TaskType = (
      rt::StateId, 
      rt::ChartId, 
      rt::PosId, 
      rt::TraceId,
      rt::GrammarId, 
      #(user_locals)*
    );

    const TASK_SIZE = size_of!(TaskType);
    const TASK_ALIGN = align_of!(TaskType);

    static VTABLE: TaskVTable = (
      dtor,
      TASK_SIZE,
      TASK_ALIGN,
      clone,
      200u64,
      300u64,
      resume,
    );

    const VTABLE_PTR = rval_cast!(VTABLE, <*const i8>);

    // , #(params: user_params)*   => Vec<RVal>
    fn #@raw_fn(
      position: rt::PosId,
      _user_args: *const ()
    ) -> ABox<dyn rt::Task> {
      _entry: {
        let task_data = builtin!(rt::TaskCtx::alloc)(TASK_SIZE, TASK_ALIGN);
        let task_data_l = lval_cast!(lval!(task_data), TaskType);
        let task_init = <#TaskType> (
          #(entry_block_id), 
          0xFFFF_FFFFu32, 
          position, 
          0u32, 0u16, 
          #(init_locals)*
        );
        stmt_set!(task_data_l, task_init);

        let fat_ptr = <rt::TaskBox> (task_data, VTABLE_PTR);
        return fat_ptr;
      }
    }

    fn clone(
      self_: &#TaskType, 
      new_state: u32
    ) -> rt::TaskBox {
      _entry: {
        //let task_data = builtin!(rt::TaskCtx::alloc)(pool_set, TASK_SIZE, TASK_ALIGN);
        let task_data = builtin!(rt::TaskCtx::alloc)(TASK_SIZE, TASK_ALIGN);
        let task_data_l = lval_cast!(lval!(task_data), TaskType);
        let task_init = stmt_use!(lval!(self_));
        stmt_set!(task_data_l, task_init);
        stmt_set!(&task_data_l.0, new_state);
        
        let fat_ptr = <rt::TaskBox> (task_data, VTABLE_PTR);
        return fat_ptr;
      }
    }

    fn dtor(_self_: &mut #TaskType) {
      _entry: {
        return;
      }
    }

    fn resume(self_: (*mut #TaskType), ctx: &mut rt::TaskCtx) {
      #{
        // let attr_noalias = core.llvm_ctx().attribute("noalias");
        // unsafe {
        //   llvm_core::LLVMAddAttributeAtIndex(resume.raw(), 1, attr_noalias);
        // }
      }*

      _entry: {
        let data = self_.0;
        let data_l = lval!(data);
        let position = &data_l.2;
        let param_self = <rt::TaskBox> (data, VTABLE_PTR);

        #{
          cell_set!(
            TASK_RESUME_FN_PARAMS(core.node_id()), 
            ResumeFnParams {
              clone_fn: clone,
              data,
              param_ctx: ctx,
              param_self,
              position,
              vtable: VTABLE_PTR.clone(),
            }
          );

          for local in &rule.locals {
            let _ = core.reduce_cld_with(ValueRed, local, &mut __b__entry.builder);
          }

          builder.reset();
          core.inner.curr_fn = Some(resume.raw());//set_curr_fn(resume);

          for block in blocks {
            let llvm_block = core.reduce_cld_with(BasicBlockRed, block, builder);
            if let Some(id) = cell_try_get!(PERSISTENT_BLOCK_ID(block)) {
              let id = b.quote_const(&(id as u32));
              cases.push((id, llvm_block));
            }
          }
        }*

        let curr_state = stmt_use!(&data_l.0);
        match curr_state {
          default => invalid_state,
          #(cases)*
        };
      }

      invalid_state: {
        return;
      }
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
