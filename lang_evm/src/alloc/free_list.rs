use {
  crate::{
    alloc::{Dealloc, ReverseAlloc},
  },
  std::{
    alloc::{Alloc, AllocErr, Layout},
    marker::{PhantomData},
    pin::Pin,
    ptr::{NonNull},
    slice::{from_raw_parts_mut},
  }
};

////////////////////////////////////////////////////////////////////////////////////////////////
const BLOCK_ALIGN_ALLOC: usize = 4096;
const BLOCK_ALIGN: usize = 4096;
const BLOCK_MASK: usize = !(BLOCK_ALIGN - 1);
const BLOCK_SIZE: usize = 4096 * 8;
////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Block<A: Alloc> {
  crate pool_ptr: Option<NonNull<FreeList<A>>>,
}

impl<A: Alloc> Block<A> {
  crate unsafe fn get<'a, T: ?Sized>(ptr: NonNull<T>) -> &'a mut Self {
    let ptr = ptr.as_ptr() as *mut u8 as usize;
    let block = (ptr & BLOCK_MASK) as *mut Self;
    &mut *block
  }

  crate fn init(&mut self, pool_ptr: NonNull<FreeList<A>>) {
    self.pool_ptr = Some(pool_ptr);
  }

  crate unsafe fn pool(&self) -> &mut FreeList<A> {
    let pool_ptr = self.pool_ptr.unwrap();
    &mut *pool_ptr.as_ptr()
  }

  crate unsafe fn split(&mut self, pool: NonNull<FreeList<A>>, size: usize, entries: &mut Entry) {
    let start = self as *mut Self as *mut u8;
    let superblock = from_raw_parts_mut(start, BLOCK_SIZE);
    Self::split_superblock(superblock, pool, size, entries);
  }

  crate unsafe fn split_block(block: &mut [u8], size: usize, entries: &mut Entry) {
    for entry in block.chunks_exact_mut(size) {
      let entry = NonNull::new_unchecked(&mut entry[0] as *mut u8);
      entries.push(entry);
    }
  }

  crate unsafe fn split_superblock(
    block: &mut [u8], pool: NonNull<FreeList<A>>, size: usize, entries: &mut Entry
  ) {
    for block in block.chunks_exact_mut(BLOCK_ALIGN) {
      let block_header = &mut block[0] as *mut _ as *mut Self;
      (*block_header).init(pool);
      Self::split_block(&mut block[8..], size, entries);
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Entry {
  crate next: Option<NonNull<Entry>>,
}

impl Entry {
  crate fn new() -> Self {
    Self { next: None }
  }

  crate fn is_empty(&self) -> bool {
    self.next.is_none()
  }

  crate fn pop(&mut self) -> Option<NonNull<u8>> {
    let head = self.next.take();
    head.map(|mut e| unsafe {
      self.next = e.as_mut().next.take();
      e.cast::<u8>() 
    })
  }

  crate fn push(&mut self, ptr: NonNull<u8>) {
    let mut ptr = ptr.cast::<Entry>();
    let new_head = Entry { next: self.next.take() };
    unsafe { *ptr.as_mut() = new_head; }
    self.next = Some(ptr);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct FreeList<A: Alloc> { // should be immovable
  blocks: Vec<NonNull<u8>>, // linked list
  entries: Entry,
  layout: Layout,
  parent_alloc: A,
}

impl<A: Alloc> FreeList<A> {
  pub fn new(elem_size: usize, parent_alloc: A) -> Pin<Box<Self>> {
    unsafe {
      Pin::new_unchecked(box Self { 
        blocks: Vec::new(),
        entries: Entry::new(),
        layout: Layout::from_size_align(elem_size, 8).unwrap(),
        parent_alloc
      })
    }
  }

  #[inline(never)]
  pub fn alloc_block(&mut self) {
    let layout = Layout::from_size_align(BLOCK_SIZE, BLOCK_ALIGN_ALLOC).unwrap();
    let ptr = unsafe { self.parent_alloc.alloc(layout).unwrap() };
    //println!("PTR {:?}", ptr);

    let align = (ptr.as_ptr() as usize) & (BLOCK_ALIGN - 1);
    assert_eq!(align, 0);

    self.blocks.push(ptr);
    let mut block = ptr.cast::<Block<A>>();

    unsafe { 
      let block = block.as_mut();
      let pool_ptr = NonNull::new(self as *mut Self).unwrap();
      let elem_size = self.layout.size();
      block.split(pool_ptr, elem_size, &mut self.entries);
    }
  }

  pub fn dump_info(&self) {
    println!("POOL_{:?}:", self.layout.size());
    println!("  num_blocks: {}", self.blocks.len());
  }
}

unsafe impl<A: Alloc> Alloc for FreeList<A> {
  #[inline]
  unsafe fn alloc(&mut self, _layout: Layout) -> Result<NonNull<u8>, AllocErr> {
    if self.entries.is_empty() { self.alloc_block(); }
    Ok(self.entries.pop().unwrap())
  }

  unsafe fn dealloc(&mut self, ptr: NonNull<u8>, _layout: Layout) {
    self.entries.push(ptr);
  }
}

impl<A: Alloc> Drop for FreeList<A> {
  fn drop(&mut self) {
    let layout = Layout::from_size_align(BLOCK_SIZE, BLOCK_ALIGN_ALLOC).unwrap();
    for block in self.blocks.drain(..) {
      unsafe { self.parent_alloc.dealloc(block, layout); }
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct FreeListDealloc<A: Alloc> { 
  alloc_type: PhantomData<A>
}

impl<A: Alloc> Dealloc for FreeListDealloc<A> {
  unsafe fn alloc_again(ptr: NonNull<u8>, layout: Layout) -> NonNull<u8> {
    let block = Block::<A>::get(ptr);
    let pool = block.pool();
    pool.alloc(layout).unwrap()
  }

  unsafe fn dealloc(ptr: NonNull<u8>, layout: Layout) {
    let block = Block::<A>::get(ptr);
    let pool = block.pool();
    pool.dealloc(ptr, layout);
  }
}

impl<A: Alloc> ReverseAlloc for FreeList<A> {
  type Dealloc = FreeListDealloc<A>;
}

////////////////////////////////////////////////////////////////////////////////////////////////
