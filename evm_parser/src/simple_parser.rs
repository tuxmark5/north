use {
  lang_evm::{
    LangEVM, JITResolver, ParserCtx, TaskCtx, ast, mir,
    runtime::{Cursor, GrammarId, MatchId, Options},
  },
  lang_base::{LangBase},
  lang_llvm_sys::{Context, JITEngine},
  lang_mir::{CodegenCtx, LangMIR, ModRed},
  lang_mir_to_llvm::{LangMIRLLVM},
  north_core::prelude::*,
  north_gen::{reduce_node},
  north_loader::{DiagHandler},
  std::{
    cell::{Ref, RefMut},
    path::Path,
    rc::Rc,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct SimpleParser {
  comp: Rc<Compiler>,
  main_rule: Option<NodeId<ast::ItemRule>>,
  model_cell: ModelCell,
  task_ctx: TaskCtx,
}

impl SimpleParser {
  pub fn new() -> SimpleParser {
    Self::new_with_settings(true, true)
  }

  pub fn new_with_settings(enable_gc: bool, enable_ri: bool) -> SimpleParser {
    let mut comp = Compiler::new();
    Self::init_diagnostics(&mut comp);
    Self::init_passes(&mut comp);

    let model_cell = comp.model_cell.clone();
    Self::init_language(&mut *model_cell.borrow_mut());

    let options = Options {
      dump_parse_tree: false,
      enable_gc,
      incorporate_reductions: enable_ri,
      show_opt_mir: false,
      show_reductions: false,
    };

    let task_ctx = Self::create_parser_ctx(&comp, options);

    Self { 
      comp: Rc::new(comp), 
      main_rule: None,
      model_cell, 
      task_ctx 
    }
  }

  fn create_parser_ctx(comp: &Compiler, options: Options) -> TaskCtx {
    comp.with(|| {
      let parser_ctx = ParserCtx::get();
      let llvm_ctx = Rc::new(Context::new());
      let jit_engine = Rc::new(JITEngine::new());

      let resolver = JITResolver::new(
        comp, 
        llvm_ctx.clone(), 
        jit_engine.clone(), 
        parser_ctx.clone(),
        options.clone(),
      );

      TaskCtx::new(
        parser_ctx, resolver, options
      )
    })
  }

  fn find_main_grammar(&self, main_mod: NodeId<mir::Mod>) 
    -> Option<NodeId<mir::ItemGrammar>>
  {
    let model = self.model();
    let module = model.node(main_mod);

    module.items.iter().find_map(|id| {
      model.get::<_, mir::ItemGrammar>(id)
        .map(|_| id.to_node_id().cast())
    })
  }

  fn find_main_rule(&self, root: NodeId<mir::Mod>) 
    -> Option<NodeId<ast::ItemRule>>
  {
    let model = self.model();

    let result = model.descendant_ids(root).find_map(|id| {
      model.get::<_, ast::ItemRule>(id)
        .filter(|r| r.name.as_ref() == "main")
        .map(|_| id.to_node_id().cast())
    });

    result
  }

  fn init_diagnostics(comp: &mut Compiler) {
    let handler = DiagHandler::new(comp);
    comp.diag_handler = Some(Box::new(handler));
  }

  fn init_language(model: &mut Model) {
    model.add_language(Rc::new(LangBase::new()));
    model.add_language(Rc::new(LangEVM));
    model.add_language(Rc::new(LangMIR));
    model.add_language(Rc::new(LangMIRLLVM));
  }

  fn init_passes(comp: &mut Compiler) {
    let pass_manager = &mut comp.pass_manager;
    pass_manager.add_pass(LangBase::new_load_pass());
    pass_manager.add_pass(LangBase::new_lex_pass());
    pass_manager.add_pass(LangBase::new_parse_pass());
    pass_manager.add_pass(LangBase::new_resolve_constrs_pass());
    pass_manager.add_pass(LangBase::new_resolve_pass());
    pass_manager.add_pass(LangBase::new_infer_types_pass());
    pass_manager.add_pass(LangBase::new_deref_types_pass());
  }

  pub fn load_grammar<P: AsRef<Path>>(&mut self, grammar_file: P) {
    self.comp.clone().with(|| {
      let root_id = {
        let mut model = self.model_mut();
        let root = LangBase::new_root(grammar_file.as_ref().to_owned());
        model.build_node(root).to_top()
      };

      if let Err(_) = self.comp.run_passes() {
        panic!("error while compiling input grammar")
      }

      let inner = CodegenCtx::new(self.model_cell.clone());
      let mir_mod = reduce_node::<ModRed>(root_id.cast(), inner, &mut ());
      let grammar_id = self.find_main_grammar(mir_mod)
        .expect("couldn't locate grammar element");

      let main_rule = self.find_main_rule(root_id.cast())
        .expect("couldn't find main grammar rule");
      self.main_rule = Some(main_rule);

      let grammar = self.task_ctx.grammar_vec.new_grammar();
      grammar.add(self.model_cell.clone(), grammar_id);
    });
  }

  fn model(&self) -> Ref<Model> {
    self.model_cell.borrow()
  }

  fn model_mut(&mut self) -> RefMut<Model> {
    self.model_cell.borrow_mut()
  }

  pub fn parse(&mut self, input: &[u8]) -> bool {
    self.comp.clone().with(|| {
      let grammar_id = GrammarId::from(0);
      let main_match_id = MatchId::new(self.main_rule.unwrap());
      let cursor = Cursor::new(input);

      self.task_ctx.start_rule(grammar_id, main_match_id, cursor);
      self.task_ctx.exec();
      
      true
    })
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
