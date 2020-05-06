use {
  crate::{
    collections::HashMap,
    optimizer::RuleBuilder,
    runtime::{
      CallElem, CallSpecId, Grammar, TaskCtorFn,
      grammar::RuleSet,
      options::Options,
      parser_ctx::ParserCtx,
    },
  },
  lang_llvm_sys::{
    Builder, Context, ContextInner, JITEngine, ValueRed,
  },
  north_core::{
    NodeId, flame,
    compiler::Compiler,
    log::trace,
  },
  north_gen::{GenAspect},
  std::{
    mem::transmute,
    rc::Rc,
  }
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct JITResolver {
  ctor_cache: HashMap<RuleSet, TaskCtorFn>,
  rule_builder: RuleBuilder,
  gen_aspect: Rc<GenAspect>,
  llvm_context: Rc<Context>,
  jit_engine: Rc<JITEngine>,
  parser_ctx: Rc<ParserCtx>,
  options: Options,
}

impl JITResolver {
  pub fn new(
    comp: &Compiler, 
    llvm_context: Rc<Context>, 
    jit_engine: Rc<JITEngine>,
    parser_ctx: Rc<ParserCtx>,
    options: Options,
  ) -> Self {
    Self {
      ctor_cache: HashMap::default(),
      rule_builder: RuleBuilder::new(comp, options.clone()),
      gen_aspect: comp.aspect_mut::<GenAspect>(),
      llvm_context,
      jit_engine,
      parser_ctx,
      options
    }
  }

  pub fn compile_rule(&self, rule_id: NodeId) {
    let llvm_mod = self.llvm_context.create_module("opt");
    let mut inner = ContextInner::new(self.llvm_context.clone());
    inner.curr_mod = Some(llvm_mod);

    let core = self.gen_aspect.core_ctx(rule_id, inner);
    let mut builder = Builder::new(self.llvm_context.clone());
    let _ = self.gen_aspect.reduce_cld::<ValueRed>(core, &mut builder);

    unsafe {
      let _guard = flame::start_guard("add_ir");
      let result = self.jit_engine.add_module(llvm_mod);
      result.expect("failed to compile optimized rule");
    }
  }

  pub fn resolve(
    &mut self, 
    grammar: Rc<Grammar>, 
    call_spec_id: CallSpecId,
  ) -> TaskCtorFn {
    let call_spec = self.parser_ctx.get_call_spec(call_spec_id).to_owned();
    self.resolve_spec(grammar, call_spec.as_ref())
  }

  pub fn resolve_spec(
    &mut self, 
    grammar: Rc<Grammar>, 
    call_spec: &[CallElem],
  ) -> TaskCtorFn {
    let (rule_set, reduce_ids) = grammar.matching_rules(&*call_spec);

    if let Some(ctor_fn) = self.ctor_cache.get(&rule_set) {
      return *ctor_fn;
    }

    trace!("OPTIMIZED RULE_2 `{:?}':", call_spec);
    let (name, opt_rule) = flame::span_of("build_opt", || {
      self.rule_builder.build(grammar, &rule_set, reduce_ids)
    });
    
    if self.options.show_opt_mir {
      north_print::print_nodes(opt_rule);
    }

    flame::span_of("compile_rule", || {
      self.compile_rule(opt_rule.cast());
    });

    let ctor_fn = self.jit_engine.get_symbol_address(name.as_ref())
      .expect("cannot find compiled rule constructor");
    let ctor_fn = unsafe { transmute::<_, TaskCtorFn>(ctor_fn) };
    self.ctor_cache.insert(rule_set, ctor_fn);

    ctor_fn
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
