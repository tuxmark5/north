use {
  crate::{
    alloc::{ABox, ReverseAlloc},
  },
  std::{
    ptr,
    alloc::{Global},
    fmt::{self, Debug},
  },
};


////////////////////////////////////////////////////////////////////////////////////////////////
pub type EntryBox<T, A> = ABox<Entry<T, A>, A>;
////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Entry<T, A: ReverseAlloc> {
  next: Option<EntryBox<T, A>>,
  value: T,
}

impl<T, A: ReverseAlloc> Entry<T, A> {
  pub fn new(value: T) -> Self {
    Self { next: None, value }
  }

  pub fn get_mut(&mut self) -> &mut T {
    &mut self.value
  }

  pub fn into_value(self) -> T {
    self.value
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct LinkedList<T, A: ReverseAlloc = Global> {
  head: Option<EntryBox<T, A>>,
  last: *mut Entry<T, A>,
  alloc: A,
}

impl<T> LinkedList<T, Global> {
  pub fn new() -> Self {
    Self::default()
  }
}

impl<T, A> LinkedList<T, A> where
  A: ReverseAlloc
{
  pub fn back_mut(&mut self) -> Option<&mut T> {
    if self.last.is_null() {
      None
    } else {
      unsafe { Some(&mut (*self.last).value) }
    }
  }

  #[inline]
  pub fn clear(&mut self) {
    self.head = None;
    self.last = ptr::null_mut();
  }

  pub fn contains(&self, elem: &T) -> bool where
    T: PartialEq
  {
    self.iter().find(|e| *e == elem).is_some()
  }

  pub fn consume<F, R>(&mut self, f: F) -> Option<R> where
    F: Fn(&mut T) -> Option<R>
  {
    while let Some(mut entry) = self.pop_front_entry() {
      let result = f(entry.get_mut());
      if result.is_some() {
        self.push_front_entry(entry);
        return result;
      }
    }
    None
  }

  pub fn extend_front(&mut self, other: &mut LinkedList<T, A>) {
    let new_entry = other.head.take();
    if new_entry.is_none() { return; }

    let other_last = other.last;
    other.last = ptr::null_mut();

    if self.last.is_null() {
      self.head = new_entry;
      self.last = other_last;
    } else {
      unsafe { (*other_last).next = self.head.take(); }
      self.head = new_entry;
    }
  }

  #[inline]
  pub fn front(&self) -> Option<&T> {
    self.head.as_ref().map(|e| &e.value)
  }

  #[inline]
  pub fn front_mut(&mut self) -> Option<&mut T> {
    self.head.as_mut().map(|e| &mut e.value)
  }

  pub fn iter(&self) -> Iter<T, A> {
    Iter { head: &self.head }
  }

  pub fn iter_mut(&mut self) -> IterMut<T, A> {
    IterMut { head: &mut self.head }
  }

  #[inline]
  pub fn is_empty(&self) -> bool {
    self.head.is_none()
  }

  pub fn len(&self) -> usize {
    self.iter().count()
  }

  #[inline]
  pub fn pop_front(&mut self) -> Option<T> {
    self.pop_front_entry().map(|entry| {
      entry.unbox().value
    })
  }

  #[inline]
  pub fn pop_front_entry(&mut self) -> Option<EntryBox<T, A>> {
    let mut result = self.head.take();

    if let Some(ref mut entry) = result {
      self.head = entry.next.take();
      if self.head.is_none() {
        self.last = ptr::null_mut();
      }
    }

    result
  }

  #[inline]
  pub fn push_back(&mut self, value: T) {
    let new_entry = ABox::new_in(Entry::new(value), &mut self.alloc);
    self.push_back_entry(new_entry);
  }

  #[inline]
  pub fn push_back_entry(&mut self, mut new_entry: EntryBox<T, A>) {
    let new_last = &mut *new_entry as *mut _;

    if self.last.is_null() {
      self.head = Some(new_entry);
    } else {
      unsafe { (*self.last).next = Some(new_entry); };
    }

    self.last = new_last;
  }

  #[inline]
  pub fn push_front(&mut self, value: T) {
    let new_entry = ABox::new_in(Entry::new(value), &mut self.alloc);
    self.push_front_entry(new_entry);
  }

  #[inline]
  pub fn push_front_entry(&mut self, mut new_entry: EntryBox<T, A>) {
    new_entry.next = self.head.take();

    if self.last.is_null() {
      self.last = &mut *new_entry as *mut _;
    }

    self.head = Some(new_entry);
  }

  pub fn rotate<F, R>(&mut self, mut f: F) -> Option<R> where
    F: FnMut(&mut T) -> (RotateResult, Option<R>)
  {
    while let Some(mut entry) = self.pop_front_entry() {
      let (action, result) = f(entry.get_mut());

      match action {
        RotateResult::Consume => { },
        RotateResult::Keep => { self.push_front_entry(entry); return result; },
        RotateResult::Rotate => { self.push_back_entry(entry); },
      }

      if result.is_some() {
        return result;
      }
    }

    None
  }
}

impl<T, A> Debug for LinkedList<T, A> where
  T: Debug, A: ReverseAlloc
{
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    f.debug_list().entries(self.iter()).finish()
  }
}

impl<T, A> Default for LinkedList<T, A> where
  A: Default + ReverseAlloc
{
  fn default() -> Self {
    Self { 
      head: None, 
      last: ptr::null_mut(),
      alloc: <_>::default(),
    }
  }
}

impl<'a, T, A> IntoIterator for &'a LinkedList<T, A> where
  A: ReverseAlloc
{
  type Item = &'a T;
  type IntoIter = Iter<'a, T, A>;

  fn into_iter(self) -> Self::IntoIter {
    Iter { head: &self.head }
  }
}

impl<'a, T, A> IntoIterator for &'a mut LinkedList<T, A> where
  A: ReverseAlloc
{
  type Item = &'a mut T;
  type IntoIter = IterMut<'a, T, A>;

  fn into_iter(self) -> Self::IntoIter {
    IterMut { head: &mut self.head }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Iter<'a, T, A> where
  T: 'a, A: 'a + ReverseAlloc
{
  head: &'a Option<EntryBox<T, A>>,
}

impl<'a, T, A> Iterator for Iter<'a, T, A> where
  A: ReverseAlloc
{
  type Item = &'a T;

  fn next(&mut self) -> Option<Self::Item> {
    match self.head {
      Some(head) => {
        let result = Some(&head.value);
        self.head = &head.next;
        result
      },
      None => None,
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct IterMut<'a, T, A> where
  T: 'a, A: 'a + ReverseAlloc
{
  head: &'a mut Option<EntryBox<T, A>>,
}

impl<'a, T, A> Iterator for IterMut<'a, T, A> where
  A: ReverseAlloc
{
  type Item = &'a mut T;

  fn next(&mut self) -> Option<Self::Item> {
    let head = unsafe { unbound_mut(self.head) };
    head.as_mut().map(|entry| {
      let Entry { next, value } = &mut **entry;
      self.head = next;
      value
    })
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub enum RotateResult {
  Consume,
  Keep,
  Rotate
}

////////////////////////////////////////////////////////////////////////////////////////////////

unsafe fn unbound_mut<'a, 'b, T>(ptr: &'a mut T) -> &'b mut T {
  &mut *(ptr as *mut T)
}

////////////////////////////////////////////////////////////////////////////////////////////////
