/*use {
  libc::{c_char, c_void},
  std::ptr,
};

pub type Code = i32;
pub type GPR = i32;
pub enum Node { }
pub type NodeRef = *mut Node;
pub enum State { }
pub type StateRef = *mut State;
pub type Word = i32;

extern {
  pub fn finish_jit();
  pub fn init_jit(argv0: *mut c_char);
  pub fn jit_new_state() -> StateRef;

  pub fn _jit_addi(state: StateRef) -> NodeRef;
  pub fn _jit_arg(state: StateRef) -> NodeRef;
  pub fn _jit_clear_state(state: StateRef);
  pub fn _jit_destroy_state(state: StateRef);
  pub fn _jit_emit(state: StateRef) -> *const c_void;
  pub fn _jit_epilog(state: StateRef);
  pub fn _jit_getarg_i(state: StateRef, reg: GPR, arg: NodeRef);
  pub fn _jit_new_node_www(state: StateRef, code: Code, a: Word, b: Word, c: Word);
  pub fn _jit_prepare(state: StateRef);
  pub fn _jit_print(state: StateRef);
  pub fn _jit_prolog(state: StateRef);
  pub fn _jit_retr(state: StateRef, reg: GPR);
}

pub mod prelude {
  
}

#[test]
pub fn hai() {
  init_jit(ptr::null_mut());
  let state = jit_new_state();
  _jit_prolog(state);
  let arg = _jit_arg(state);
  _jit_getarg_i(state, 0, arg);
  _jit_new_node_www(state, 0, 0, 0, 1);
  _jit_retr(state, 0);
  let ptr = _jit_emit(state);
  
  finish_jit();

  println!("TSUPS");
}
*/