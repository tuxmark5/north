builtins
builtins_grammar
grammar

////////////////////////////////////////////////////////////////////////////////////////////////

rule ident() {
  reject kw;
  parse (s*, r"a-zA-Z_");
  parse (r"a-zA-Z_0-9"*, B);
}

// rule ident_any() {
//   parse (s*, r"a-zA-Z_");
//   parse (r"a-zA-Z_0-9"*, B);
// }

rule lifetime() {
  parse (s*, "'", r"a-zA-Z_");
  parse (r"a-zA-Z_0-9"*, B);
}

////////////////////////////////////////////////////////////////////////////////////////////////

rule_dyn delim();

group _: delim(100) {
  rule delim_braces() {
    parse_g (op_brace_0, token_tree*, op_brace_1);
  }

  rule delim_brackets() {
    parse_g (op_bracket_0, token_tree*, op_bracket_1);
  }

  rule delim_parens() {
    parse_g (op_paren_0, token_tree*, op_paren_1);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

rule_dyn expr();

group expr_semiless: expr(105) {
  // try and async missing

  rule expr_block() {
    parse (attr*, op_brace_0);
    parse (stmt*, expr!0?, op_brace_1);
    //parse (op_brace_0, stmt*, (expr:0..105)?, op_brace_1);
  }

  rule expr_closure() {
    parse (kw_static?, kw_move?);
    parse (op_or, (fn_param % op_comma)*, op_or);
    parse (op_minus_gt, type)?;
    parse (expr!0);
  }

  rule expr_for() {
    parse (label?);
    parse (kw_for, pat, kw_in, expr!0);
    parse (expr_block);
  }

  rule expr_if() {
    parse (kw_if, expr!0, expr_block);
    parse (kw_else, else_block)?;
  }

  rule expr_if_let() {
    parse (kw_if, kw_let, pat, op_eq, expr!0, expr_block); 
    parse (kw_else, else_block)?;
  }

  rule expr_loop() {
    parse (label?);
    parse (kw_loop, expr_block);
  }

  rule expr_match() {
    parse (kw_match, expr!0, op_brace_0);
    parse (match_arm)*;
    parse (op_brace_1);
  }

  rule expr_unsafe() {
    parse (kw_unsafe, expr_block);
  }

  rule expr_while() {
    parse (label?);
    parse (kw_while, expr!0);
    parse (expr_block);
  }

  rule expr_while_let() {
    parse (label?);
    parse (kw_while, kw_let, pat, op_eq, expr!0);
    parse (expr_block);
  }
}

group expr_primary: expr(100) {
  rule expr_array() {
    parse (op_bracket_0);
    parse (expr!0 %? op_comma)*;
    parse (op_bracket_1);
  }

  rule expr_box() {
    parse (kw_box, expr!0);
  }

  rule expr_break() {
    parse (kw_break, lifetime?, (expr!0)?);
  }

  rule expr_call() {
    parse (expr!, turbofish?);
    parse (op_paren_0, (expr!0 %? op_comma)*, op_paren_1);
  }

  rule expr_continue() {
    parse (kw_continue, lifetime?);
  }

  rule expr_lit() {
    parse lit;
  }

  rule expr_macro() {
    parse macro;
  }

  // rule expr_op_paren() {
  //   parse (op_paren_0, expr!0, op_paren_1);
  // }

  rule expr_path() {
    parse path;
  }

  rule expr_repeat() {
    parse (op_bracket_0);
    parse (expr!0, op_semi, expr!0);
    parse (op_bracket_1);
  }

  rule expr_return() {
    parse (kw_return, (expr!0)?);
  }

  rule expr_struct() {
    parse (path, op_brace_0);
    parse (expr_struct_field %? op_comma)*;
    parse (op_dot_dot, expr!0)?;
    parse (op_brace_1);
  }

  rule expr_tuple() {
    parse (op_paren_0, (expr!0 %? op_comma)*, op_paren_1);
  }

  rule expr_yield() {
    parse (kw_yield, (expr!0)?);
  }
}

group _: expr(95) {
  rule expr_field() {
    parse (expr!, op_dot, ident);
  }

  rule expr_field_idx() { 
    parse (expr!, op_dot, lit_int10); 
  }

  rule expr_index() {
    parse (expr!, op_bracket_0, expr!0, op_bracket_1);
  }
  
  rule expr_method_call() {
    parse (expr!, op_dot, ident, turbofish?);
    parse (op_paren_0, (expr!0 %? op_comma)*, op_paren_1);
  }

  rule expr_op_try() { 
    parse (expr!, op_qm);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[part_of(expr, 90)] rule expr_op_deref()   { parse (op_star, expr!); }
#[part_of(expr, 90)] rule expr_op_neg()     { parse (op_minus, expr!); }
#[part_of(expr, 90)] rule expr_op_not()     { parse (op_ex, expr!); }
#[part_of(expr, 90)] rule expr_ref()        { parse (op_and, kw_mut?, expr!); }

#[part_of(expr, 85)] rule expr_cast()       { parse (expr!, kw_as, type); }
#[part_of(expr, 85)] rule expr_type()       { parse (expr!, op_colon, type); }

#[part_of(expr, 80)] rule expr_op_div()     { parse (expr!, op_div, expr); }
#[part_of(expr, 80)] rule expr_op_mult()    { parse (expr!, op_star, expr); }
#[part_of(expr, 80)] rule expr_op_rem()     { parse (expr!, op_rem, expr); }

#[part_of(expr, 75)] rule expr_op_add()     { parse (expr!, op_plus, expr); }
#[part_of(expr, 75)] rule expr_op_sub()     { parse (expr!, op_minus, expr); }

#[part_of(expr, 70)] rule expr_op_shl()     { parse (expr!, op_lt_lt, expr); }
#[part_of(expr, 70)] rule expr_op_shr()     { parse (expr!, op_gt_gt, expr); }

#[part_of(expr, 65)] rule expr_op_bit_and() { parse (expr!, op_and, expr); }
#[part_of(expr, 60)] rule expr_op_bit_xor() { parse (expr!, op_caret, expr); }
#[part_of(expr, 55)] rule expr_op_bit_or()  { parse (expr!, op_or, expr); }

#[part_of(expr, 50)] rule expr_op_eq()      { parse (expr, op_eq_eq, expr); }
#[part_of(expr, 50)] rule expr_op_neq()     { parse (expr, op_ex_eq, expr); }
#[part_of(expr, 50)] rule expr_op_gt()      { parse (expr, op_gt, expr); }
#[part_of(expr, 50)] rule expr_op_gte()     { parse (expr, op_gt_eq, expr); }
#[part_of(expr, 50)] rule expr_op_lt()      { parse (expr, op_lt, expr); }
#[part_of(expr, 50)] rule expr_op_lte()     { parse (expr, op_lt_eq, expr); }

#[part_of(expr, 45)] rule expr_op_and()     { parse (expr!, op_and_and, expr); }
#[part_of(expr, 40)] rule expr_op_or()      { parse (expr!, op_or_or, expr); }

#[part_of(expr, 35)] rule expr_op_range()   { parse (expr?, op_dot_dot, expr?); }

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

rule_dyn fields();

group _: fields(100) {
  rule fields_indexed() {
    parse (op_paren_0, (field_indexed % op_comma)*, op_paren_1);
  }

  rule fields_named() {
    parse (op_brace_0, (field_named %? op_comma)*, op_brace_1);
  }

  // rule fields_unit() {
  //   parse (op_semi);
  // }
}

////////////////////////////////////////////////////////////////////////////////////////////////

rule_dyn fn_param();

group _: fn_param(100) {
  rule fn_param_captured() {
    parse (pat, op_colon, type);
  }

  rule fn_param_inferred() {
    parse (pat);
  }

  rule fn_param_self() {
    parse (kw_mut?, kw_self);
  }

  rule fn_param_self_ref() {
    parse (op_and, lifetime?, kw_mut?, kw_self);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

rule_dyn foreign_item();

group _: foreign_item(100) {
  rule foreign_item_fn() {
    parse (attr*, vis?, kw_fn);
    parse (fn_sig, op_semi);
  }

  rule foreign_item_static() {
    parse (attr*, vis?, kw_static, kw_mut?);
    parse (ident, op_colon, type, op_semi);
  }

  rule foreign_item_type() {
    parse (attr*, vis?, kw_type);
    parse (ident, op_semi);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

rule_dyn gen_arg();

group _: gen_arg(100) {
  rule gen_arg_binding() { parse (ident, op_eq, type); }
  rule gen_arg_lifetime() { parse lifetime; }
  rule gen_arg_type() { parse type; }
}

////////////////////////////////////////////////////////////////////////////////////////////////

rule_dyn gen_met_arg();

group _: gen_met_arg(100) {
  rule gen_met_arg_type() { parse type; }
}

////////////////////////////////////////////////////////////////////////////////////////////////

rule_dyn gen_param();

group _: gen_param(100) {
  //rule gen_param_const() { parse (ident, op_eq, type); }

  rule gen_param_lifetime() { 
    parse lifetime;
    parse (op_colon, (lifetime % op_plus)+)?;
  }

  rule gen_param_type() { 
    parse (attr*, ident); 
    parse (op_colon, (type_param_bound % op_plus)+)?;
    parse (op_eq, type)?;
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

rule_dyn item();

group _: item(100) {
  rule item_const() {
    parse (attr*, vis?, kw_const);
    parse (ident, op_colon, type);
    parse (op_eq, expr, op_semi);
  }

  rule item_enum() {
    parse (attr*, vis?, kw_enum);
    parse (ident, generics?, where_clause?);
    parse (op_brace_0, (variant %? op_comma)*, op_brace_1);
  }

  rule item_extern_crate() {
    parse (attr*, vis?, kw_extern);
    parse (kw_crate, ident);
    parse (kw_as, ident)?;
    parse (op_semi);
  }

  rule item_fn() {
    parse (attr*, vis?, kw_const?, kw_unsafe?, abi?, kw_fn);
    parse (fn_sig, expr_block);
  }

  rule item_foreign_mod() {
    parse (attr*, abi);
    parse (op_brace_0, foreign_item*, op_brace_1);
  }

  rule item_impl() {
    parse (attr*, kw_unsafe?, kw_impl, generics?);
    parse (op_ex?, path, kw_for)?;
    parse (type, where_clause?);
    parse (op_brace_0, impl_item*, op_brace_1);
  }

  rule item_macro() {
    parse (attr*, macro, op_semi?);
  }

  rule item_mod() {
    parse (attr*, vis?, kw_mod, ident);
    parse (op_brace_0, item!*, op_brace_1) | op_semi;
  }

  rule item_static() {
    parse (attr*, vis?, kw_static, kw_mut?);
    parse (ident, op_colon, type);
    parse (op_eq, expr, op_semi);
  }

  rule item_struct() {
    parse (attr*, vis?, kw_struct);
    parse (ident, generics?, where_clause?);
    parse (fields ~ op_semi);
  }

  rule item_trait() {
    parse (attr*, vis?, kw_unsafe?, kw_auto?);
    parse (kw_trait, ident, generics?);
    parse (op_colon, (type_param_bound % op_plus)+)?;
    parse (where_clause?);
    parse (op_brace_0, trait_item*, op_brace_1);
  }

  rule item_type() {
    parse (attr*, vis?, kw_type);
    parse (ident, generics?);
    parse (op_eq, type, op_semi);
  }

  rule item_use() {
    parse (attr*, vis?, kw_use, use);
    parse (op_semi);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

rule_dyn impl_item();

group _: impl_item(100) {
  rule impl_item_const() {
    parse (attr*, vis?, kw_default?, kw_const);
    parse (ident, op_colon, type);
    parse (op_eq, expr, op_semi);
  }

  rule impl_item_method() {
    parse (attr*, vis?);
    parse (kw_default?, kw_const?, kw_unsafe?, abi?);
    parse (kw_fn, fn_sig, expr_block);
  }

  rule impl_item_type() {
    parse (attr*, vis?, kw_default?, kw_type);
    parse (ident, generics?);
    parse (op_eq, type, op_semi);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

rule_dyn kw();

rule kw_default()   { parse (s*, "default", B); }
rule kw_self()      { parse (s*, "self", B); }
rule kw_super()     { parse (s*, "super", B); }

group _: kw {
  rule kw_as()        { parse (s*, "as", B); }
  rule kw_auto()      { parse (s*, "auto", B); }
  rule kw_break()     { parse (s*, "break", B); }
  rule kw_const()     { parse (s*, "const", B); }
  rule kw_continue()  { parse (s*, "continue", B); }
  rule kw_crate()     { parse (s*, "crate", B); }
  rule kw_dyn()       { parse (s*, "dyn", B); }
  rule kw_else()      { parse (s*, "else", B); }
  rule kw_enum()      { parse (s*, "enum", B); }
  rule kw_extern()    { parse (s*, "extern", B); }
  rule kw_false()     { parse (s*, "false", B); }
  rule kw_fn()        { parse (s*, "fn", B); }
  rule kw_for()       { parse (s*, "for", B); }
  rule kw_if()        { parse (s*, "if", B); }
  rule kw_impl()      { parse (s*, "impl", B); }
  rule kw_in()        { parse (s*, "in", B); }
  rule kw_let()       { parse (s*, "let", B); }
  rule kw_loop()      { parse (s*, "loop", B); }
  rule kw_match()     { parse (s*, "match", B); }
  rule kw_mod()       { parse (s*, "mod", B); }
  rule kw_move()      { parse (s*, "move", B); }
  rule kw_mut()       { parse (s*, "mut", B); }
  rule kw_pub()       { parse (s*, "pub", B); }
  rule kw_ref()       { parse (s*, "ref", B); }
  rule kw_return()    { parse (s*, "return", B); }
  
  rule kw_static()    { parse (s*, "static", B); }
  rule kw_struct()    { parse (s*, "struct", B); }
  rule kw_trait()     { parse (s*, "trait", B); }
  rule kw_true()      { parse (s*, "true", B); }
  rule kw_type()      { parse (s*, "type", B); }
  rule kw_unsafe()    { parse (s*, "unsafe", B); }
  rule kw_use()       { parse (s*, "use", B); }
  rule kw_where()     { parse (s*, "where", B); }
  rule kw_while()     { parse (s*, "while", B); }

  rule kw_box()       { parse (s*, "box", B); }
  rule kw_macro()     { parse (s*, "macro", B); }
  rule kw_yield()     { parse (s*, "yield", B); }
}

////////////////////////////////////////////////////////////////////////////////////////////////

rule escape_unicode() {
  parse ("{", r"0-9a-fA-F"+, "}");
}

rule_dyn lit();

group _: lit(100) {
  rule lit_bool() {
    parse (kw_false | kw_true);
  }

  rule lit_char() {
    parse (s*, "b"?, "'", 
      ANY_UTF8 | (dom_g "\\", 
        (r"." | (dom_g "u", escape_unicode) | (dom_g "x", r"0-9a-fA-F", r"0-9a-fA-F"))
      ),
      dom_g "'"
    );
  }

  rule lit_int10() {
    parse (s*, r"0-9", r"0-9_"*);
    parse (r"a-zA-Z", r"a-zA-Z0-9_"*)?;
    parse (B);
  }

  rule lit_int16() {
    parse (s*, "0x", r"0-9a-fA-F", r"0-9a-fA-F_"*, B);
  }

  rule lit_str() {
    parse (s*, "b"?, "\"", (r"^\"\\\n" | ("\\", r".") | ("\\"?, "\n"))*, "\"");
  }

  rule lit_str_raw() {
    parse (s*, "b"?, "r\"", ANY*, dom_g "\"");
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

rule_dyn op();

rule op_brace_0()     { parse (s*, "{"); }
rule op_brace_1()     { parse (s*, "}"); }
rule op_bracket_0()   { parse (s*, "["); }
rule op_bracket_1()   { parse (s*, "]"); }
rule op_paren_0()     { parse (s*, "("); }
rule op_paren_1()     { parse (s*, ")"); }

rule op_and_and()     { parse (s*, "&&"); }
rule op_and_eq()      { parse (s*, "&="); }
rule op_caret_eq()    { parse (s*, "^="); }
rule op_colon_colon() { parse (s*, "::"); }
rule op_div_eq()      { parse (s*, "/="); }
rule op_dot_dot()     { parse (s*, ".."); }
rule op_dot_dot_dot() { parse (s*, "..."); }
rule op_dot_dot_eq()  { parse (s*, "..="); }
rule op_eq_eq()       { parse (s*, "=="); }
rule op_eq_gt()       { parse (s*, "=>"); }
rule op_ex_eq()       { parse (s*, "!="); }
rule op_gt_eq()       { parse (s*, ">="); }
rule op_gt_gt()       { parse (s*, ">>"); }
rule op_gt_gt_eq()    { parse (s*, ">>="); }
rule op_lt_eq()       { parse (s*, "<="); }
rule op_lt_lt()       { parse (s*, "<<"); }
rule op_lt_lt_eq()    { parse (s*, "<<="); }
rule op_minus_eq()    { parse (s*, "-="); }
rule op_minus_gt()    { parse (s*, "->"); }
rule op_or_eq()       { parse (s*, "|="); }
rule op_or_or()       { parse (s*, "||"); }
rule op_plus_eq()     { parse (s*, "+="); }
rule op_rem_eq()      { parse (s*, "%="); }
rule op_star_eq()     { parse (s*, "*="); }

group _: op(100) {
  rule op_and()         { parse (s*, "&"); }
  rule op_at()          { parse (s*, "@"); }
  rule op_caret()       { parse (s*, "^"); }
  rule op_colon()       { parse (s*, ":"); }
  rule op_comma()       { parse (s*, ","); }
  rule op_div()         { parse (s*, "/"); }
  rule op_dollar()      { parse (s*, "$"); }
  rule op_dot()         { parse (s*, "."); }
  rule op_eq()          { parse (s*, "="); }
  rule op_ex()          { parse (s*, "!"); }
  rule op_gt()          { parse (s*, ">"); }
  rule op_lt()          { parse (s*, "<"); }
  rule op_minus()       { parse (s*, "-"); }
  rule op_or()          { parse (s*, "|"); }
  rule op_plus()        { parse (s*, "+"); }
  rule op_pound()       { parse (s*, "#"); }
  rule op_qm()          { parse (s*, "?"); }
  rule op_rem()         { parse (s*, "%"); }
  rule op_semi()        { parse (s*, ";"); }
  rule op_star()        { parse (s*, "*"); }
}

////////////////////////////////////////////////////////////////////////////////////////////////

rule_dyn pat();

group _: pat(100) {
  rule pat_box() {
    parse (kw_box, pat!); 
  }

  // #[prefer]
  rule pat_ident() {
    parse (kw_ref?, kw_mut?, ident);
    parse (op_at, pat!)?;
  }

  rule pat_lit() {
    parse (lit);
  }

  rule pat_macro() {
    parse macro;
  }

  // AMBIGUOUS with pat_ident
  rule pat_path() {
    parse path; 
  }

  rule pat_range() {
    parse (pat!?, range_limits, pat!?);
  }

  rule pat_ref() {
    parse (op_and, kw_mut?, pat!); 
  }

  rule pat_slice() {
    parse (op_bracket_0);
    parse (pat! %? op_comma)*;
    parse (op_bracket_1);
  }

  rule pat_struct() {
    parse (path, op_brace_0);
    parse ((field_pat %? op_comma)*, op_dot_dot?);
    parse (op_brace_1);
  }

  rule pat_tuple() {
    parse (op_paren_0, (pat! %? op_comma)*, op_paren_1);
  }

  rule pat_tuple_struct() {
    parse (path, pat_tuple); 
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

rule_dyn path();

group _: path(100) {
  rule path_normal() {
    parse (op_colon_colon?);
    parse (path_segment % op_colon_colon)+;
  }

  rule path_qualified() {
    parse (op_lt, type, (kw_as, path_normal)?, op_gt);
    parse (op_colon_colon, path_segment)*;
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

rule_dyn path_arguments();

group _: path_arguments(100) {
  rule pa_angle_bracketed() {
    parse (op_colon_colon)?;
    parse (op_lt, (gen_arg %? op_comma)+, op_gt);
  }

  rule pa_parenthesized() {
    parse (op_paren_0, (type %? op_comma)*, op_paren_1);
    parse (op_minus_gt, type)?;
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

rule_dyn path_segment();

group _: path_segment(100) {
  rule path_segment_crate() {
    parse (kw_crate);
  }

  rule path_segment_extern() {
    parse (kw_extern);
  }

  rule path_segment_ident() {
    parse (ident, path_arguments?);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

rule_dyn s();

group _: s(0) {
  rule s_comment_multi() {
    parse ("/*", ANY*, dom_g "*/");
  }

  rule s_comment_single() {
    parse ("//", ANY*, dom_g "\n");
  }

  rule s_whitespace() {
    parse (r" \t\n"+, S1);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

rule_dyn stmt();

group _: stmt(100) {
  rule stmt_expr() { 
    parse (expr_semiless); 
  }

  rule stmt_expr_semi() { 
    parse (expr, op_semi); 
  }

  rule stmt_item() { 
    parse (item); 
  }

  rule stmt_let() { 
    parse (kw_let, pat); 
    parse (op_colon, type)?;
    parse (op_eq, expr)?;
    parse op_semi;
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

rule_dyn token_tree();

group _: token_tree(100) {
  rule tt_delim() { parse delim; }
  rule tt_ident() { parse ident; }
  rule tt_lifetime() { parse lifetime; }
  rule tt_lit() { parse lit; }
  rule tt_kw() { parse kw; }
  rule tt_op() { parse op; }
}

////////////////////////////////////////////////////////////////////////////////////////////////

rule_dyn trait_item();

group _: trait_item(100) {
  rule trait_item_const() {
    parse (attr*, kw_const);
    parse (ident, op_colon, type);
    parse (op_eq, expr)?;
    parse (op_semi);
  }

  rule trait_item_method() {
    parse (attr*, kw_const?, kw_unsafe?, abi?, kw_fn);
    parse (fn_sig, expr_block | op_semi);
  }

  rule trait_item_type() {
    parse (attr*, kw_type);
    parse (ident, generics?);
    parse (op_colon, (type_param_bound % op_plus)+)?;
    parse (op_eq, type)?;
    parse (op_semi);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

rule_dyn type();

group _: type(100) {
  rule type_array() {
    parse (op_bracket_0, type!, op_semi, expr, op_bracket_1);
  }

  rule type_bare_fn() {
    parse (kw_unsafe?, abi?, kw_fn);
    parse (op_paren_0, (bare_fn_param % op_comma)*, op_paren_1); 
    parse (op_minus_gt, type!)?;
  }

  rule type_impl_trait() {
    parse (kw_impl, (type_param_bound % op_plus)+);
  }

  rule type_macro() {
    parse macro;
  }

  rule type_never() {
    parse (op_ex);
  }

  // rule type_paren() {
  //   parse (op_paren_0, type!, op_paren_1);
  // }

  rule type_path() {
    parse (path);
  }

  rule type_ptr() {
    parse (op_star, (kw_const | kw_mut), type!);
  }

  rule type_ref() {
    parse (op_and, lifetime?, kw_mut?, type!);
  }

  rule type_slice() {
    parse (op_bracket_0, type!, op_bracket_1);
  }

  rule type_trait_object() {
    parse (kw_dyn, (type_param_bound % op_plus)+);
  }

  rule type_tuple() {
    parse (op_paren_0, (type! %? op_comma)*, op_paren_1); 
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

rule_dyn type_param_bound();

group _: type_param_bound(100) {
  rule tpb_lifetime() {
    parse lifetime;
  }

  rule tpb_trait() {
    parse (op_qm?, bound_lifetimes?, path);
  }

  // rule tpb_trait_paren() {
  //   parse (op_paren_0, op_qm?, bound_lifetimes?, path, op_paren_1);
  // }
}

////////////////////////////////////////////////////////////////////////////////////////////////

rule_dyn use();

group _: use(100) {
  rule use_glob() {
    parse op_star;
  }

  rule use_group() {
    parse (op_brace_0, (use! %? op_comma)*, op_brace_1);
  }

  rule use_name() {
    parse (ident);
  }

  rule use_path() {
    parse (ident, op_colon_colon, use!);
  }

  rule use_rename() {
    parse (ident, kw_as, ident);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

rule_dyn vis();

group _: vis(100) {
  rule vis_crate() { 
    parse kw_crate; 
  }

  rule vis_pub() { 
    parse kw_pub; 
  }

  rule vis_restricted() { 
    parse (kw_pub, op_paren_0, path, op_paren_1); 
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

rule_dyn where_predicate();

group _: where_predicate(100) {
  rule wp_lifetime() { 
    parse lifetime;
    parse (op_colon, (lifetime % op_plus)+)?;
  }

  rule wp_type() { 
    parse (bound_lifetimes?, type); 
    parse (op_colon, (type_param_bound % op_plus)+)?;
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

rule abi() {
  parse (kw_extern, lit_str?);
}

rule attr() {
  parse (op_pound, op_bracket_0, token_tree+, op_bracket_1);
}

rule bare_fn_param() {
  parse ((ident, op_colon)?, type);
}

rule bound_lifetimes() {
  parse (kw_for, op_lt, (lifetime_def % op_comma)+, op_gt);
}

rule else_block() {
  parse expr_block | expr_if | expr_if_let;
}

rule expr_struct_field() {
  parse (ident, op_colon, expr) | (ident);
}

rule field_indexed() {
  parse (attr*, vis?, type);
}

rule field_named() {
  parse (attr*, vis?, ident);
  parse (op_colon, type);
}

rule field_pat() {
  parse (attr*);
  parse (ident, op_colon)?;
  parse (pat);
}

rule fn_sig() {
  parse (ident, generics?, op_paren_0);
  parse ((fn_param %? op_comma)*, op_paren_1);
  parse (op_minus_gt, type)?;
  parse (where_clause)?;
}

rule generics() {
  parse (op_lt, (gen_param % op_comma)+, op_gt);
}

rule label() {
  parse (lifetime, op_colon);
}

rule lifetime_def() {
  parse lifetime;
  parse (op_colon, (lifetime % op_plus)+)?;
}

rule macro() {
  parse (path, op_ex, ident?, delim);
}

rule match_arm() {
  parse (op_or?, (pat % op_or)+);
  parse (kw_if, expr)?;
  parse (op_eq_gt, expr);
  parse_g (op_comma?);
}

rule range_limits() {
  parse op_dot_dot | op_dot_dot_dot | op_dot_dot_eq;
}

rule turbofish() {
  parse (op_colon_colon);
  parse (op_lt, (gen_met_arg % op_comma)+, op_gt);
}

rule variant() {
  parse (attr*, ident, fields?);
  parse (op_eq, expr)?;
}

rule where_clause() {
  parse (kw_where, (where_predicate %? op_comma)+);
}

////////////////////////////////////////////////////////////////////////////////////////////////

rule main() -> i32 {
  parse_g (item*);
  123
}

// ambs:
// Command::new(real_llvm_config);   CALL + path with paren args
// comment // vs div in macros

////////////////////////////////////////////////////////////////////////////////////////////////
