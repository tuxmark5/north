use {
  crate::{
    command_line::GlobalSettings,
    profiler::Profiler,
  },
  lang_base::{LangBase},
  lang_evm::{
    LangEVM, JITResolver, ParserCtx, TaskCtx, ast, mir,
    runtime::{Cursor, GrammarId, MatchId, Options},
  },
  lang_llvm_sys::{Context, JITEngine},
  lang_mir::{CodegenCtx, LangMIR, ModRed},
  lang_mir_to_llvm::{LangMIRLLVM},
  north_core::{
    flame,
    prelude::*,
  },
  north_gen::{reduce_node},
  north_loader::{DiagHandler},
  north_print::{print_nodes},
  std::{
    cell::{Ref, RefMut},
    fs::read_to_string,
    rc::Rc,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Parser {
  comp: Compiler,
  model_cell: ModelCell,
  settings: GlobalSettings,
}

impl Parser {
  pub fn new(settings: GlobalSettings) -> Self {
    let comp = Compiler::new();
    let model_cell = comp.model_cell.clone();

    Self { 
      comp, 
      model_cell,
      settings,
    }
  }

  fn create_parser_ctx(&self) -> TaskCtx {
    let parser_ctx = ParserCtx::get();
    let llvm_ctx = Rc::new(Context::new());
    let jit_engine = Rc::new(JITEngine::new());

    let options = Options {
      dump_parse_tree: self.settings.show_parse_tree,
      enable_gc: self.settings.enable_gc,
      incorporate_reductions: self.settings.incorporate_reductions,
      show_opt_mir: self.settings.show_opt_mir,
      show_reductions: self.settings.show_reductions,
    };

    let resolver = JITResolver::new(
      &self.comp, 
      llvm_ctx.clone(), 
      jit_engine.clone(), 
      parser_ctx.clone(),
      options.clone(),
    );

    TaskCtx::new(
      parser_ctx, resolver, options
    )
  }

  pub fn exec(&mut self) {
    flame::span_of("MAIN", || {
      self.exec_parser();
    });

    if self.settings.profile {
      Profiler::dump();
    }
  }

  pub fn exec_parser(&mut self) {
    self.init_diagnostics();
    self.init_language();
    self.init_passes();

    let model_cell = self.model_cell.clone();
    let root_id = self.init_root_node();

    self.comp.with(|| {
      if let Err(_) = self.comp.run_passes() {
        panic!("error while compiling input grammar")
      }

      let inner = CodegenCtx::new(self.model_cell.clone());
      let mir_mod = reduce_node::<ModRed>(root_id.cast(), inner, &mut ());
      if self.settings.show_mir {
        print_nodes(mir_mod);
      }

      let grammar_id = self.find_main_grammar(mir_mod)
        .expect("couldn't locate grammar element");
      let main_rule = self.find_main_rule(root_id.cast())
        .expect("couldn't find main grammar rule");

      let mut ctx = self.create_parser_ctx();
      // Add the grammar 
      {
        let grammar = ctx.grammar_vec.new_grammar();
        grammar.add(model_cell.clone(), grammar_id);
      }

      let input = self.read_input();

      let grammar_id = GrammarId::from(0);
      let main_match_id = MatchId::new(main_rule);
      let cursor = Cursor::new(input.as_ref());
      ctx.start_rule(grammar_id, main_match_id, cursor);

      ctx.exec();

      if self.settings.dump_evm_stats {
        let _guard = flame::start_guard("dump_evm");
        ctx.dump_info();
      }

      if self.settings.show_parse_tree {
        let _guard = flame::start_guard("dump_tree");
        ctx.dump_parse_tree();
      }
    });
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

  fn init_diagnostics(&mut self) {
    let handler = DiagHandler::new(&mut self.comp);
    self.comp.diag_handler = Some(Box::new(handler));
  }

  fn init_language(&mut self) {
    let mut model = self.model_mut();
    model.add_language(Rc::new(LangBase::new()));
    model.add_language(Rc::new(LangEVM));
    model.add_language(Rc::new(LangMIR));
    model.add_language(Rc::new(LangMIRLLVM));
  }

  fn init_passes(&mut self) {
    let pass_manager = &mut self.comp.pass_manager;
    pass_manager.add_pass(LangBase::new_load_pass());
    pass_manager.add_pass(LangBase::new_lex_pass());
    pass_manager.add_pass(LangBase::new_parse_pass());
    pass_manager.add_pass(LangBase::new_resolve_constrs_pass());
    pass_manager.add_pass(LangBase::new_resolve_pass());
    pass_manager.add_pass(LangBase::new_infer_types_pass());
    pass_manager.add_pass(LangBase::new_deref_types_pass());
  }

  fn init_root_node(&mut self) -> NodeId {
    let grammar_file = self.settings.grammar_file.clone();
    let mut model = self.model_mut();
    let root = LangBase::new_root(grammar_file);
    model.build_node(root).to_top()
  }

  fn model(&self) -> Ref<Model> {
    self.model_cell.borrow()
  }

  fn model_mut(&mut self) -> RefMut<Model> {
    self.model_cell.borrow_mut()
  }

  fn read_input(&self) -> String {
    let filename = &self.settings.input_file;
    match read_to_string(filename) {
      Ok(input) => input,
      Err(_) => panic!("couldn't read parse input file: {:?}", filename),
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
