builtins
builtins_grammar
grammar

////////////////////////////////////////////////////////////////////////////////////////////////

rule_dyn expr();
rule_dyn item();
rule_dyn lit();
rule_dyn pat();
rule_dyn stmt();
rule_dyn type();
rule_dyn vis();

////////////////////////////////////////////////////////////////////////////////////////////////

// XXX: need !keyword
rule ident() {
  parse (s?, r"a-zA-Z_");
  parse (r"a-zA-Z_0-9"*, B);
  //parse_g (r"a-zA-Z_0-9"*);
}

rule s() {
  parse_g r" \t\n"+;
}

////////////////////////////////////////////////////////////////////////////////////////////////

rule kw_as()        { parse (s?, "as"); }
rule kw_break()     { parse (s?, "break"); }
rule kw_const()     { parse (s?, "const"); }
rule kw_continue()  { parse (s?, "continue"); }
rule kw_crate()     { parse (s?, "crate"); }
rule kw_else()      { parse (s?, "else"); }
rule kw_enum()      { parse (s?, "enum"); }
rule kw_extern()    { parse (s?, "extern"); }
rule kw_false()     { parse (s?, "false"); }
rule kw_fn()        { parse (s?, "fn"); }
rule kw_for()       { parse (s?, "for"); }
rule kw_if()        { parse (s?, "if"); }
rule kw_impl()      { parse (s?, "impl"); }
rule kw_in()        { parse (s?, "in"); }
rule kw_let()       { parse (s?, "let"); }
rule kw_loop()      { parse (s?, "loop"); }
rule kw_match()     { parse (s?, "match"); }
rule kw_mod()       { parse (s?, "mod"); }
rule kw_move()      { parse (s?, "move"); }
rule kw_mut()       { parse (s?, "mut"); }
rule kw_pub()       { parse (s?, "pub"); }
rule kw_ref()       { parse (s?, "ref"); }
rule kw_return()    { parse (s?, "return"); }
rule kw_self()      { parse (s?, "self"); }
rule kw_static()    { parse (s?, "static"); }
rule kw_struct()    { parse (s?, "struct"); }
rule kw_super()     { parse (s?, "super"); }
rule kw_trait()     { parse (s?, "trait"); }
rule kw_true()      { parse (s?, "true"); }
rule kw_type()      { parse (s?, "type"); }
rule kw_unsafe()    { parse (s?, "unsafe"); }
rule kw_use()       { parse (s?, "use"); }
rule kw_where()     { parse (s?, "where"); }
rule kw_while()     { parse (s?, "while"); }

rule kw_box()       { parse (s?, "box"); }
rule kw_macro()     { parse (s?, "macro"); }
rule kw_yield()     { parse (s?, "yield"); }

////////////////////////////////////////////////////////////////////////////////////////////////

#[part_of(lit, 100)]
rule lit_num() {
  //parse_g (s?, r"0-9"+);
  parse (s?, r"0-9"+, B);
}

////////////////////////////////////////////////////////////////////////////////////////////////

rule op_and()         { parse (s?, "&"); }
rule op_and_and()     { parse (s?, "&&"); }
rule op_brace_0()     { parse (s?, "{"); }
rule op_brace_1()     { parse (s?, "}"); }
rule op_bracket_0()   { parse (s?, "["); }
rule op_bracket_1()   { parse (s?, "]"); }
rule op_colon()       { parse (s?, ":"); }
rule op_colon_colon() { parse (s?, "::"); }
rule op_comma()       { parse (s?, ","); }
rule op_eq()          { parse (s?, "="); }
rule op_ex()          { parse (s?, "!"); }
rule op_dot()         { parse (s?, "."); }
rule op_minus()       { parse (s?, "-"); }
rule op_paren_0()     { parse (s?, "("); }
rule op_paren_1()     { parse (s?, ")"); }
rule op_plus()        { parse (s?, "+"); }
rule op_semi()        { parse (s?, ";"); }
rule op_star()        { parse (s?, "*"); }

////////////////////////////////////////////////////////////////////////////////////////////////

rule path() {
  //parse (op_colon_colon?, path_segment %+ op_colon_colon);
  parse (op_colon_colon?, path_segment);
}

rule path_segment() {
  parse ident;
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[part_of(expr, 100)]
rule expr_array() {
  parse (op_bracket_0, expr!0, op_bracket_1); // list
}

#[part_of(expr, 100)]
rule expr_block() {
  parse (op_brace_0, stmt*, op_brace_1);
}

#[part_of(expr, 100)]
rule expr_box() {
  parse (kw_box, expr!0);
}

#[part_of(expr, 100)]
rule expr_break() {
  parse (kw_break, (expr!0)?);
}

#[part_of(expr, 100)]
rule expr_continue() {
  parse (kw_continue);
}

#[part_of(expr, 100)]
rule expr_for() {
  parse (kw_for, pat, kw_in, expr!0, expr_block);
}

#[part_of(expr, 100)]
rule expr_if() {
  parse (kw_if, expr!0, expr_block);
}

#[part_of(expr, 100)]
rule expr_if_let() {
  parse (kw_if, kw_let, pat, op_eq, expr!0, expr_block);
}

#[part_of(expr, 100)]
rule expr_lit() {
  parse lit;
}

#[part_of(expr, 100)]
rule expr_loop() {
  parse (kw_loop, expr_block);
}

#[part_of(expr, 100)]
rule expr_match() {
  parse (kw_match, expr!0, expr_block); // FIX
}

#[part_of(expr, 100)]
rule expr_ref() {
  parse (op_and, kw_mut?, expr!0);
}

#[part_of(expr, 100)]
rule expr_return() {
  parse (kw_return, (expr!0)?);
}

#[part_of(expr, 100)]
rule expr_unsafe() {
  parse (kw_unsafe, expr_block);
}

#[part_of(expr, 100)]
rule expr_var() {
  parse ident;
}

#[part_of(expr, 100)]
rule expr_while() {
  parse (kw_while, expr!0, expr_block);
}

#[part_of(expr, 100)]
rule expr_while_let() {
  parse (kw_while, kw_let, pat, op_eq, expr!0, expr_block);
}

#[part_of(expr, 100)]
rule expr_yield() {
  parse (kw_yield, (expr!0)?);
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[part_of(expr, 100)]
rule expr_call() {
  parse (expr!, op_paren_0, expr!0, op_paren_1); // list
}

#[part_of(expr, 100)]
rule expr_field() {
  parse (expr!, op_dot, ident);
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[part_of(expr, 100)]
rule expr_op_paren() {
  parse (op_paren_0, expr!0, op_paren_1);
}

#[part_of(expr, 90)]
rule expr_op_mult() { parse (expr!, op_star, expr); }

#[part_of(expr, 80)]
rule expr_op_add() { parse (expr!, op_plus, expr); }

#[part_of(expr, 80)]
rule expr_op_sub() { parse (expr!, op_minus, expr); }

////////////////////////////////////////////////////////////////////////////////////////////////

#[part_of(item, 100)]
rule item_const() {
  parse (vis?, kw_const);
  parse (ident, op_colon, type);
  parse (op_eq, expr, op_semi);
}

#[part_of(item, 100)]
rule item_fn() {
  parse (vis?, kw_fn)
  parse (ident, op_paren_0, op_paren_1);
  parse (expr_block);
}

#[part_of(item, 100)]
rule item_mod() {
  parse (vis?, kw_mod, ident);
  parse (op_brace_0, item*, op_brace_1);
}

#[part_of(item, 100)]
rule item_static() {
  parse (vis?, kw_static);
  parse (ident, op_colon, type);
  parse (op_eq, expr, op_semi);
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[part_of(pat, 100)]
rule pat_box() {
  parse (kw_box, pat!); 
}

#[part_of(pat, 100)]
rule pat_ident() {
  parse (kw_ref?, kw_mut?, ident); // subpat
}

#[part_of(pat, 100)]
rule pat_lit() {
  parse (lit);
}

#[part_of(pat, 100)]
rule pat_ref() {
  parse (op_and, kw_mut?, pat!); 
}

#[part_of(pat, 100)]
rule pat_slice() {
  parse (op_bracket_0, op_bracket_1);
}

#[part_of(pat, 100)]
rule pat_struct() {
  parse (path, op_brace_0, op_brace_1); 
}

#[part_of(pat, 100)]
rule pat_tuple() {
  parse (op_paren_0, pat!, op_paren_1); // seq
}

#[part_of(pat, 100)]
rule pat_tuple_struct() {
  parse (path, pat_tuple); 
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[part_of(stmt, 100)]
rule stmt_expr() { parse (expr, op_semi); }

#[part_of(stmt, 100)]
rule stmt_item() { parse (item); }

////////////////////////////////////////////////////////////////////////////////////////////////

#[part_of(type, 100)]
rule type_array() {
  parse (op_bracket_0, type!, op_semi, expr, op_bracket_1);
}

#[part_of(type, 100)]
rule type_bare_fn() {
  parse (kw_unsafe?); // abi
  parse (kw_fn, op_paren_0, type!, op_paren_0); // seq + ret_type
}

#[part_of(type, 100)]
rule type_never() {
  parse (op_ex);
}

#[part_of(type, 100)]
rule type_paren() {
  parse (op_paren_0, type!, op_paren_1);
}

#[part_of(type, 100)]
rule type_path() {
  parse (path);
}

#[part_of(type, 100)]
rule type_ptr() {
  parse (op_star, (kw_const | kw_mut), type!);
}

#[part_of(type, 100)]
rule type_ref() {
  parse (op_and, kw_mut?, type!); // lifetime
}

#[part_of(type, 100)]
rule type_slice() {
  parse (op_bracket_0, type!, op_bracket_1);
}

// #[part_of(type, 100)]
// rule type_trait_object() {
//   parse (kw_dyn, type!, op_bracket_1);
// }

#[part_of(type, 100)]
rule type_tuple() {
  parse (op_paren_0, type!, op_paren_1); // seq
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[part_of(vis, 100)]
rule vis_crate() {
  parse kw_crate;
}

#[part_of(vis, 100)]
rule vis_pub() {
  parse kw_pub;
}

////////////////////////////////////////////////////////////////////////////////////////////////

rule main() -> i32 {
  parse (expr_block+);
  123
}

////////////////////////////////////////////////////////////////////////////////////////////////
