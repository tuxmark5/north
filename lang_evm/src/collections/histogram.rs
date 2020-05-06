use {
  std::{
    collections::BTreeMap,
    fmt::{self, Debug},
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Histogram<K> where
  K: Ord
{
  freqs: BTreeMap<K, usize>,
  total: usize,
}

impl<K> Histogram<K> where
  K: Ord
{
  pub fn new() -> Self {
    Self::default()
  }

  pub fn insert(&mut self, value: K) {
    self.total += 1;
    let entry = self.freqs.entry(value);
    let freq = entry.or_default();
    *freq += 1;
  }
}

impl<K> Debug for Histogram<K> where
  K: Debug + Ord
{
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let mut accum = 0;
    for (value, freq) in &self.freqs {
      accum += *freq;
      let percentage = accum * 100 / self.total;
      writeln!(f, "{:15?}: {:7} [{:3}%]", value, freq, percentage)?;
    }
    writeln!(f, "TOTAL: {:7}", self.total)
  }
}

impl<K> Default for Histogram<K> where
  K: Ord
{
  fn default() -> Self {
    Self {
      freqs: <_>::default(),
      total: 0,
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
