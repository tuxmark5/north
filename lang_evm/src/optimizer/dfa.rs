use {
  crate::{
    runtime::{Cursor, PosId},
  },
  itertools::Itertools,
  north_core::{
    quote::{QuoteType, RustQuoter, rust_type},
  },
  std::{
    fmt::{self, Debug},
    rc::Rc,
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////
pub type Transitions = Rc<[u8; 256]>;
////////////////////////////////////////////////////////////////////////////////////////////////

pub struct DFA {
  crate states: Box<[State]>,
}

impl DFA {
  pub fn exec(&self, mut cursor: Cursor, position: &mut PosId) -> u32 {
    let start = cursor.adjust_begin(*position);

    // println!("BEGIN");
    match self.exec_inner(0, &mut cursor) {
      Ok(result) => {
        // println!("END_OK {:?}", result);
        cursor.adjust_end(start, position);
        result
      }

      Err(result) => {
        // println!("END_ERR {:?}", result);
        result
      }
    }
  }

  pub fn exec_inner(&self, mut curr_state: usize, cursor: &mut Cursor) -> Result<u32, u32> {
    let start = cursor.start;

    loop {
      let state = &self.states[curr_state];
      // println!("EXEC_DFA {:?} => {:?}", curr_state, state);

      match state {
        State::Complete { result_id } => {
          return Ok(*result_id as u32);
        }

        State::Fail { result_id } => {
          cursor.start = start;
          return Err(*result_id as u32);
        }

        State::Lookahead { try_state, complete_id } => {
          let result = self.exec_inner(*try_state as usize, cursor);
          if result.is_ok() { return result }
          return Ok(*complete_id as u32);
        }

        State::Shift { eof_state, state_ids, transitions } => {
          match cursor.next() {
            Some(sym) => { curr_state = state_ids[transitions[sym as usize] as usize] as usize; }
            None => { curr_state = *eof_state as usize; }
          }
        }
      }
    }
  }
}

impl Debug for DFA {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let mut ds = f.debug_struct("DFA");
    for (i, state) in self.states.iter().enumerate() { 
      let state_name = format!("{}", i);
      ds.field(state_name.as_str(), state); 
    }
    ds.finish()
  }
}

impl QuoteType for DFA {
  fn quote(q: &mut RustQuoter) -> rust_type::Type {
    <()>::quote(q)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

// rename to Instr
pub enum State {
  Complete {
    result_id: u16 
  },

  Fail {
    result_id: u16 
  },

  Lookahead { 
    try_state: u16,
    complete_id: u16,
  },

  Shift { 
    eof_state: u16,
    state_ids: Vec<u16>,
    transitions: Transitions,
  },
}

impl State {
  crate fn format_transitions(state_ids: &[u16], transitions: &[u8]) -> String {
    let groups = transitions.iter().group_by(|id| *id);

    let mut pos = 0;
    let mut groups = groups.into_iter().map(|(trans_id, group)| {
      let trans_id = *trans_id as usize;
      let state_id = state_ids[trans_id];
      let end_pos = pos + group.count();
      let result = format!("{}:{} => {:?}", pos, end_pos - 1, state_id);
      pos = end_pos;
      result
    });

    groups.join(", ")
  }
}

impl Debug for State {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      State::Complete { result_id } => {
        write!(f, "<C:{}>", result_id)
      }
      State::Fail { result_id } => {
        write!(f, "<F:{}>", result_id)
      }
      State::Lookahead { try_state, complete_id } => {
        write!(f, "<L: try={}, comp={}>", try_state, complete_id)
      }
      State::Shift { eof_state, state_ids, transitions } => {
        let text = Self::format_transitions(state_ids.as_ref(), transitions.as_ref());
        write!(f, "<S:{}; EOF={}>", text, eof_state)
      }
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
