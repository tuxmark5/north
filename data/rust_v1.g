builtins
grammar

////////////////////////////////////////////////////////////////////////////////////////////////

rule_dyn expr();
rule_dyn gen_arg();
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
  parse_g (r"a-zA-Z_0-9"*);
}

rule lifetime() {
  parse (s?, "'", r"a-zA-Z_");
  parse_g (r"a-zA-Z_0-9"*);
}

rule s() {
  parse_g r" \t\n"+;
}

rule s0() {
  parse_g r" \t\n"*;
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
  parse_g (s?, r"0-9"+);
}

////////////////////////////////////////////////////////////////////////////////////////////////

rule op_and()         { parse (s?, "&"); }
rule op_and_and()     { parse (s?, "&&"); }
rule op_and_eq()      { parse (s?, "&="); }
rule op_brace_0()     { parse (s?, "{"); }
rule op_brace_1()     { parse (s?, "}"); }
rule op_bracket_0()   { parse (s?, "["); }
rule op_bracket_1()   { parse (s?, "]"); }
rule op_caret()       { parse (s?, "^"); }
rule op_caret_eq()    { parse (s?, "^="); }
rule op_colon()       { parse (s?, ":"); }
rule op_colon_colon() { parse (s?, "::"); }
rule op_comma()       { parse (s?, ","); }
rule op_div()         { parse (s?, "/"); }
rule op_div_eq()      { parse (s?, "/="); }
rule op_eq()          { parse (s?, "="); }
rule op_eq_eq()       { parse (s?, "=="); }
rule op_eq_gt()       { parse (s?, "=>"); }
rule op_ex()          { parse (s?, "!"); }
rule op_ex_eq()       { parse (s?, "!="); }
rule op_dot()         { parse (s?, "."); }
rule op_gt()          { parse (s?, ">"); }
rule op_gt_eq()       { parse (s?, ">="); }
rule op_gt_gt()       { parse (s?, ">>"); }
rule op_gt_gt_eq()    { parse (s?, ">>="); }
rule op_lt()          { parse (s?, "<"); }
rule op_lt_eq()       { parse (s?, "<="); }
rule op_lt_lt()       { parse (s?, "<<"); }
rule op_lt_lt_eq()    { parse (s?, "<<="); }
rule op_minus()       { parse (s?, "-"); }
rule op_minus_eq()    { parse (s?, "-="); }
rule op_or()          { parse (s?, "|"); }
rule op_or_eq()       { parse (s?, "|="); }
rule op_or_or()       { parse (s?, "||"); }
rule op_paren_0()     { parse (s?, "("); }
rule op_paren_1()     { parse (s?, ")"); }
rule op_plus()        { parse (s?, "+"); }
rule op_plus_eq()     { parse (s?, "+="); }
rule op_rem()         { parse (s?, "%"); }
rule op_rem_eq()      { parse (s?, "%="); }
rule op_semi()        { parse (s?, ";"); }
rule op_star()        { parse (s?, "*"); }
rule op_star_eq()     { parse (s?, "*="); }

////////////////////////////////////////////////////////////////////////////////////////////////

rule else_block() {
  parse expr_block | expr_if | expr_if_let;
}

rule expr_struct_field() {
  parse (ident, op_colon, expr) | (ident);
}

rule label() {
  parse (lifetime, op_colon);
}

rule match_arm() {
  parse (pat % op_or)+;
  parse (kw_if, expr)?;
  parse (op_eq_gt, expr);
  parse_g (op_comma?);
}

rule path() {
  parse (op_colon_colon?);
  parse (path_segment % op_colon_colon)+;
}

rule path_segment() {
  parse ident;
  parse (op_lt, (gen_arg % op_comma)+, op_gt)?;
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[part_of(expr, 100)]
rule expr_array() {
  parse (op_bracket_0);
  parse (expr!0 %? op_comma)*;
  parse (op_bracket_1);
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
  parse (kw_break, lifetime?, (expr!0)?);
}

#[part_of(expr, 100)]
rule expr_continue() {
  parse (kw_continue, lifetime?);
}

#[part_of(expr, 100)]
rule expr_for() {
  parse (label?);
  parse (kw_for, pat, kw_in, expr!0);
  parse (expr_block);
}

#[part_of(expr, 100)]
rule expr_if() {
  parse (kw_if, expr!0, expr_block);
  parse (kw_else, else_block)?;
}

#[part_of(expr, 100)]
rule expr_if_let() {
  parse (kw_if, kw_let, pat, op_eq, expr!0, expr_block); 
  parse (kw_else, else_block)?;
}

#[part_of(expr, 100)]
rule expr_lit() {
  parse lit;
}

#[part_of(expr, 100)]
rule expr_loop() {
  parse (label?);
  parse (kw_loop, expr_block);
}

#[part_of(expr, 100)]
rule expr_macro() {
  parse (path, op_ex, op_brace_0, op_brace_1); // INVALID
}

#[part_of(expr, 100)]
rule expr_match() {
  parse (kw_match, expr!0, op_brace_0);
  parse (match_arm)*;
  parse (op_brace_1);
}

#[part_of(expr, 100)]
rule expr_path() {
  parse path;
}

#[part_of(expr, 100)]
rule expr_repeat() {
  parse (op_bracket_0);
  parse (expr!0, op_semi, expr!0);
  parse (op_bracket_1);
}

#[part_of(expr, 100)]
rule expr_return() {
  parse_g (kw_return, (expr!0)?);
}

#[part_of(expr, 100)]
rule expr_struct() {
  parse (path, op_brace_0);
  parse (expr_struct_field %? op_comma)*;
  parse (op_brace_1);
}

#[part_of(expr, 100)]
rule expr_unsafe() {
  parse (kw_unsafe, expr_block);
}

#[part_of(expr, 100)]
rule expr_while() {
  parse (label?);
  parse (kw_while, expr!0);
  parse (expr_block);
}

#[part_of(expr, 100)]
rule expr_while_let() {
  parse (label?);
  parse (kw_while, kw_let, pat, op_eq, expr!0);
  parse (expr_block);
}

#[part_of(expr, 100)]
rule expr_yield() {
  parse (kw_yield, (expr!0)?);
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[part_of(expr, 100)]
rule expr_call() {
  parse (expr!, op_paren_0);
  parse (expr!0 % op_comma)*;
  parse (op_paren_1);
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

#[part_of(expr, 90)] rule expr_op_deref() { parse (op_star, expr!); }
#[part_of(expr, 90)] rule expr_op_neg()   { parse (op_minus, expr!); }
#[part_of(expr, 90)] rule expr_ref()      { parse (op_and, kw_mut?, expr!); }

#[part_of(expr, 85)] rule expr_cast()     { parse (expr!, kw_as, type); }
#[part_of(expr, 85)] rule expr_type()     { parse (expr!, op_colon, type); }

#[part_of(expr, 80)] rule expr_op_div()   { parse (expr!, op_div, expr); }
#[part_of(expr, 80)] rule expr_op_mult()  { parse (expr!, op_star, expr); }
#[part_of(expr, 80)] rule expr_op_rem()   { parse (expr!, op_rem, expr); }

#[part_of(expr, 75)] rule expr_op_add()   { parse (expr!, op_plus, expr); }
#[part_of(expr, 75)] rule expr_op_sub()   { parse (expr!, op_minus, expr); }

#[part_of(expr, 70)] rule expr_op_shl()   { parse (expr!, op_lt_lt, expr); }
#[part_of(expr, 70)] rule expr_op_shr()   { parse (expr!, op_gt_gt, expr); }

#[part_of(expr, 65)] rule expr_op_bit_and() { parse (expr!, op_and, expr); }
#[part_of(expr, 60)] rule expr_op_bit_xor() { parse (expr!, op_caret, expr); }
#[part_of(expr, 55)] rule expr_op_bit_or()  { parse (expr!, op_or, expr); }

#[part_of(expr, 50)] rule expr_op_eq()    { parse (expr, op_eq_eq, expr); }
#[part_of(expr, 50)] rule expr_op_neq()   { parse (expr, op_ex_eq, expr); }
#[part_of(expr, 50)] rule expr_op_gt()    { parse (expr, op_gt, expr); }
#[part_of(expr, 50)] rule expr_op_gte()   { parse (expr, op_gt_eq, expr); }
#[part_of(expr, 50)] rule expr_op_lt()    { parse (expr, op_lt, expr); }
#[part_of(expr, 50)] rule expr_op_lte()   { parse (expr, op_lt_eq, expr); }

#[part_of(expr, 45)] rule expr_op_and()   { parse (expr!, op_and_and, expr); }
#[part_of(expr, 40)] rule expr_op_or()    { parse (expr!, op_or_or, expr); }

// 35 .. ...

#[part_of(expr, 20)] rule expr_op_assign()      { parse (expr, op_eq, expr!); }
#[part_of(expr, 20)] rule expr_op_assign_add()  { parse (expr, op_plus_eq, expr!); }
#[part_of(expr, 20)] rule expr_op_assign_and()  { parse (expr, op_and_eq, expr!); }
#[part_of(expr, 20)] rule expr_op_assign_div()  { parse (expr, op_div_eq, expr!); }
#[part_of(expr, 20)] rule expr_op_assign_mult() { parse (expr, op_star_eq, expr!); }
#[part_of(expr, 20)] rule expr_op_assign_or()   { parse (expr, op_or_eq, expr!); }
#[part_of(expr, 20)] rule expr_op_assign_rem()  { parse (expr, op_rem_eq, expr!); }
#[part_of(expr, 20)] rule expr_op_assign_shl()  { parse (expr, op_lt_lt_eq, expr!); }
#[part_of(expr, 20)] rule expr_op_assign_shr()  { parse (expr, op_gt_gt_eq, expr!); }
#[part_of(expr, 20)] rule expr_op_assign_sub()  { parse (expr, op_minus_eq, expr!); }
#[part_of(expr, 20)] rule expr_op_assign_xor()  { parse (expr, op_caret_eq, expr!); }

////////////////////////////////////////////////////////////////////////////////////////////////

#[part_of(gen_arg, 100)] rule gen_arg_binding() { parse (ident, op_eq, type); }
#[part_of(gen_arg, 100)] rule gen_arg_lifetime() { parse lifetime; }
#[part_of(gen_arg, 100)] rule gen_arg_type() { parse type; }

////////////////////////////////////////////////////////////////////////////////////////////////

#[part_of(item, 100)]
rule item_const() {
  parse (vis?, kw_const);
  parse (ident, op_colon, type);
  parse (op_eq, expr, op_semi);
}

#[part_of(item, 100)]
rule item_fn() {
  parse (vis?, kw_fn, ident)
  parse (op_paren_0, op_paren_1); // generic, params, ret_type, where
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
  parse (kw_ref?, kw_mut?, ident); // subpat @
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
  parse (op_bracket_0, op_bracket_1); // slice
}

#[part_of(pat, 100)]
rule pat_struct() {
  parse (path, op_brace_0, op_brace_1); // members and ..
}

#[part_of(pat, 100)]
rule pat_tuple() {
  parse (op_paren_0, (pat! %? op_comma)*, op_paren_1);
}

#[part_of(pat, 100)]
rule pat_tuple_struct() {
  parse (path, pat_tuple); 
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[part_of(stmt, 100)] rule stmt_expr() { parse (expr, op_semi); }
#[part_of(stmt, 100)] rule stmt_item() { parse (item); }

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

#[part_of(vis, 100)] rule vis_crate() { parse kw_crate; }
#[part_of(vis, 100)] rule vis_pub() { parse kw_pub; }

////////////////////////////////////////////////////////////////////////////////////////////////

rule main() -> i32 {
  parse (expr_block);
  123
}

////////////////////////////////////////////////////////////////////////////////////////////////
