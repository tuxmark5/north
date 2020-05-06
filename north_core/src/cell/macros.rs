////////////////////////////////////////////////////////////////////////////////////////////////

pub macro cell_cell {
  ($cell:expr $(, $args:expr)* ) => {
    $cell.cell(&$cell.key_b().make( $($args),* ))
  }
}

pub macro cell_get {
  ($cell:ident) => {{
    use north_core::cell::Cell;
    $cell.get(&$cell.key_b().make())
  }},

  ($cell:ident($($args:expr),*)) => {{
    use north_core::cell::Cell;
    $cell.get(&$cell.key_b().make( $($args),* ))
  }}
}

pub macro cell_get_mut {
  ($cell:expr $(, $args:expr)* ) => {
    $cell.get_mut(&$cell.key_b().make( $($args),* ))
  }
}

pub macro cell_get_mut_or {
  ($cell:ident, $init:expr) => {
    $cell.get_mut_or(&$cell.key_b().make(), $init)
  },

  ($cell:ident($($args:expr),*), $init:expr) => {
    $cell.get_mut_or(&$cell.key_b().make( $($args),* ), $init)
  }
}

pub macro cell_set {
  ($cell:ident, $value:expr) => {{
    use north_core::cell::Cell;
    $cell.set(&$cell.key_b().make(), $value)
  }},

  ($cell:ident($($args:expr),*), $value:expr) => {{
    use north_core::cell::Cell;
    $cell.set(&$cell.key_b().make( $($args),* ), $value)
  }}
}

pub macro cell_try_get {
  ($cell:ident) => {{
    use north_core::cell::Cell;
    Cell::try_get(&$cell, &$cell.key_b().make())
  }},

  ($cell:ident($($args:expr),*)) => {{
    use north_core::cell::Cell;
    Cell::try_get(&$cell, &$cell.key_b().make( $($args),* ))
  }}
}

////////////////////////////////////////////////////////////////////////////////////////////////
