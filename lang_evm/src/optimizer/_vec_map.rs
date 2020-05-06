use {

};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct VecMap<K, V> {
  entries: Vec<(K, V)>
}

impl<K, V> VecMap<K, V> where
  K: Eq, V: Default
{
  pub fn new() -> Self {
    Self { entries: Vec::new() }
  }

  pub fn get_mut(&mut self, key: K) -> &mut V {
    for (k, v) in &mut self.entries {
      if *k == key { return v; }
    }

    let value = V::default();
    self.entries.push((key, value));

    let last_entry = self.entries.last_mut().unwrap();
    &mut last_entry.1
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

