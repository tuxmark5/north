use {
  rustc_errors::{CodeMapper},
  std::{
    rc::Rc
  },
  syntax_pos::{
    BytePos, CharPos, FileLinesResult, FileName, FileMap, Loc, Span,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct CodeMap {
  pub file_maps: Vec<Rc<FileMap>>,
}

impl CodeMap {
  pub fn lookup_file(&self, pos: BytePos) -> Option<Rc<FileMap>> {
    let index = self.file_maps.binary_search_by_key(&pos, |map| map.start_pos);
    let index = match index {
      Ok(index) => index,
      Err(index) => index - 1,
    };

    self.file_maps.get(index)
      .cloned()
  }
}

impl CodeMapper for CodeMap {
  fn call_span_if_macro(&self, sp: Span) -> Span {
    sp
  }

  fn doctest_offset_line(&self, line: usize) -> usize {
    line
  }

  fn ensure_filemap_source_present(&self, _file_map: Rc<FileMap>) -> bool {
    true
  }

  fn lookup_char_pos(&self, pos: BytePos) -> Loc {
    let file = self.lookup_file(pos).unwrap();
    let line_id = file.lookup_line(pos).unwrap();
    let (line_pos, _) = file.line_bounds(line_id);
    let col = pos - line_pos;

    Loc {
      file,
      line: line_id + 1,
      col: CharPos(col.0 as usize),
      col_display: col.0 as usize,
    }
  }

  fn merge_spans(&self, _sp_lhs: Span, _sp_rhs: Span) -> Option<Span> {
    unimplemented!()
  }

  fn span_to_filename(&self, sp: Span) -> FileName {
    let file = self.lookup_file(sp.lo()).unwrap();
    file.name.clone()
  }

  fn span_to_lines(&self, _sp: Span) -> FileLinesResult {
    unimplemented!()
  }

  fn span_to_string(&self, _sp: Span) -> String {
    unimplemented!()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
