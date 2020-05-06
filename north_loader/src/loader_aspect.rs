use {
  FileMap,
  //code_map::CodeMap,
  north_core::{
    compiler::Aspect,
  },
  std::{
    cell::RefCell,
    fs,
    path::PathBuf,
    rc::Rc,
  },
  /*syntax_pos::{
    BytePos, FileName, FileMap,
  },*/
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct LoaderAspect(RefCell<LoaderAspectInner>);

impl LoaderAspect {
  pub fn new() -> Self {
    let inner = LoaderAspectInner {
      //file_maps: Vec::new(),
      //last_pos: BytePos(0),
    };

    LoaderAspect(RefCell::new(inner))
  }

  /*pub fn code_map(&self) -> CodeMap  {
    let inner = self.0.borrow();
    CodeMap { file_maps: inner.file_maps.clone() }
  }*/

  pub fn load_file(&self, path: &PathBuf) -> Option<Rc<FileMap>> {
    let src = fs::read_to_string(path).unwrap();
    let mut inner = self.0.borrow_mut();
    let file_map = inner.add_file(path.clone(), src);
    Some(file_map)
  }
}

impl Aspect for LoaderAspect {
  fn name(&self) -> &str {
    "loader"
  }
}

impl Default for LoaderAspect {
  fn default() -> Self {
    Self::new()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct LoaderAspectInner {
  //file_maps: Vec<Rc<FileMap>>,
  //last_pos: BytePos,
}

impl LoaderAspectInner {
  pub fn add_file(&mut self, _path: PathBuf, src: String) -> Rc<FileMap> {
    Rc::new(FileMap { src })
  }

  /*pub fn add_file(&mut self, path: PathBuf, src: String) -> Rc<FileMap> {
    let name = FileName::Real(path);
    let unmapped_path = name.clone();
    let start_pos = self.last_pos;
    let src_size = src.as_bytes().len();

    let file = FileMap::new(name, false, unmapped_path, src, start_pos);
    self.scan_file(&file);

    let file_map = Rc::new(file);
    self.file_maps.push(file_map.clone());
    self.last_pos.0 += src_size as u32;

    file_map
  }

  pub fn scan_file(&self, map: &FileMap) {
    let src = map.src.as_ref().unwrap();
    let bytes = src.as_bytes();

    let base_pos = self.last_pos;
    //map.next_line(base_pos);

    for (pos, byte) in bytes.iter().enumerate() {
      if *byte == ('\n' as u8) {
        let offset = BytePos(pos as u32 + 1);
        //map.next_line(base_pos + offset);
      }
    }
  }*/
}

////////////////////////////////////////////////////////////////////////////////////////////////
