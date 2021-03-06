////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Debug)]
pub struct Location {
  id: u16
}

impl Location {
  pub fn empty() -> Self {
    Self { id: 0 }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct LocationEntry {
  pub file_id: u16,
  pub file_offset: u32,
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[allow(dead_code)]
pub struct LocationManager {
  files: Vec<String>,
  locations: Vec<LocationEntry>
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Debug)]
pub struct LocationRange {
  pub start_loc: Location,
  pub end_loc: Location,
}

impl LocationRange {
  pub fn empty() -> Self {
    Self {
      start_loc: Location::empty(),
      end_loc: Location::empty(),
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
