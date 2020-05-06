use {
  loader_aspect::LoaderAspect,
  north_core::{
    compiler::{self, Compiler, DiagBox, DiagVec}
  },
  /*rustc_errors::{
    DiagnosticId, Handler,
    emitter::ColorConfig
  },*/
  std::{
    rc::Rc,
  },
  /*syntax_pos::{
    GLOBALS,
    BytePos, Globals, Span,
    hygiene::SyntaxContext
  },*/
};

////////////////////////////////////////////////////////////////////////////////////////////////

#[allow(dead_code)]
pub struct DiagHandler {
  diagnostics: DiagVec,
  loader_aspect: Rc<LoaderAspect>,
}

impl DiagHandler {
  pub fn new(comp: &mut Compiler) -> Self {
    Self {
      diagnostics: Vec::new(),
      loader_aspect: comp.aspect_cell::<LoaderAspect>(),
    }
  }
}

impl compiler::DiagHandler for DiagHandler {
  fn add_diag(&mut self, diag: DiagBox) {
    self.diagnostics.push(diag);
  }

  fn emit_all(&mut self) {
    /*GLOBALS.set(&Globals::new(), || {
      let loader = &*self.loader_aspect;
      let handler = Handler::with_tty_emitter(ColorConfig::Always, true, false,
        Some(Rc::new(loader.code_map()))
      );

      for diag in &self.diagnostics {
        let span = diag.span().map(|span| {
          let lo = BytePos(span.lo().0);
          let hi = BytePos(span.hi().0);
          Span::new(lo, hi, SyntaxContext::empty())
        });

        let mut builder = handler.struct_err(diag.message());

        if true {
          builder.code(DiagnosticId::Error("E000".to_string()));
        }

        if let Some(span) = span {
          builder.set_span(span);
        }

        builder.emit();
      }

      self.diagnostics.clear();
    });*/
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////



////////////////////////////////////////////////////////////////////////////////////////////////
