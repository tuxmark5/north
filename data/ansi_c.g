builtins
builtins_grammar
grammar

////////////////////////////////////////////////////////////////////////////////////////////////

rule_dyn expr();

////////////////////////////////////////////////////////////////////////////////////////////////

rule s() {
  parse (r" \t\n"+, S1);
}

////////////////////////////////////////////////////////////////////////////////////////////////
// Keywords
////////////////////////////////////////////////////////////////////////////////////////////////

rule_dyn kw();

#[token_group]
group _: kw {
  rule ident() {
    parse (s?, r"a-zA-Z_");
    parse (r"a-zA-Z_0-9"*, B);
  }

  rule kw_auto()        { parse (s?, "auto", dom_g B); }
  rule kw_bool()        { parse (s?, "bool", dom_g B); }
  rule kw_break()       { parse (s?, "break", dom_g B); }
  rule kw_case()        { parse (s?, "case", dom_g B); }
  rule kw_char()        { parse (s?, "char", dom_g B); }
  rule kw_const()       { parse (s?, "const", dom_g B); }
  rule kw_continue()    { parse (s?, "continue", dom_g B); }
  rule kw_default()     { parse (s?, "default", dom_g B); }
  rule kw_do()          { parse (s?, "do", dom_g B); }
  rule kw_double()      { parse (s?, "double", dom_g B); }
  rule kw_else()        { parse (s?, "else", dom_g B); }
  rule kw_enum()        { parse (s?, "enum", dom_g B); }
  rule kw_extern()      { parse (s?, "extern", dom_g B); }
  rule kw_float()       { parse (s?, "float", dom_g B); }
  rule kw_for()         { parse (s?, "for", dom_g B); }
  rule kw_goto()        { parse (s?, "goto", dom_g B); }
  rule kw_if()          { parse (s?, "if", dom_g B); }
  rule kw_inline()      { parse (s?, "inline", dom_g B); }
  rule kw_int()         { parse (s?, "int", dom_g B); }
  rule kw_long()        { parse (s?, "long", dom_g B); }
  rule kw_register()    { parse (s?, "register", dom_g B); }
  rule kw_restrict()    { parse (s?, "restrict", dom_g B); }
  rule kw_return()      { parse (s?, "return", dom_g B); }
  rule kw_short()       { parse (s?, "short", dom_g B); }
  rule kw_signed()      { parse (s?, "signed", dom_g B); }
  rule kw_sizeof()      { parse (s?, "sizeof", dom_g B); }
  rule kw_static()      { parse (s?, "static", dom_g B); }
  rule kw_struct()      { parse (s?, "struct", dom_g B); }
  rule kw_switch()      { parse (s?, "switch", dom_g B); }
  rule kw_typedef()     { parse (s?, "typedef", dom_g B); }
  rule kw_union()       { parse (s?, "union", dom_g B); }
  rule kw_unsigned()    { parse (s?, "unsigned", dom_g B); }
  rule kw_while()       { parse (s?, "while", dom_g B); }
  rule kw_void()        { parse (s?, "void", dom_g B); }
  rule kw_volatile()    { parse (s?, "volatile", dom_g B); }
}

////////////////////////////////////////////////////////////////////////////////////////////////

rule_dyn op();

#[token_group]
group _: op(0) {
  rule op_colon()       { parse (s?, shift_p ":"); }
  rule op_comma()       { parse (s?, shift_p ","); }
  rule op_dot()         { parse (s?, shift_p "."); }
  rule op_dot_dot_dot() { parse (s?, shift_p "..."); }
  rule op_semi()        { parse (s?, shift_p ";"); }

  rule op_brace_0()     { parse (s?, shift_p "{"); }
  rule op_brace_1()     { parse (s?, shift_p "}"); }
  rule op_bracket_0()   { parse (s?, shift_p "["); }
  rule op_bracket_1()   { parse (s?, shift_p "]"); }
  rule op_paren_0()     { parse (s?, shift_p "("); }
  rule op_paren_1()     { parse (s?, shift_p ")"); }

  rule op_and()         { parse (s?, shift_p "&"); }
  rule op_and_and()     { parse (s?, shift_p "&&"); }
  rule op_and_eq()      { parse (s?, shift_p "&="); }
  rule op_caret()       { parse (s?, shift_p "^"); }
  rule op_caret_eq()    { parse (s?, shift_p "^="); }
  rule op_eq()          { parse (s?, shift_p "="); }
  rule op_eq_eq()       { parse (s?, shift_p "=="); }
  rule op_ex()          { parse (s?, shift_p "!"); }
  rule op_ex_eq()       { parse (s?, shift_p "!="); }
  rule op_div()         { parse (s?, shift_p "/"); }
  rule op_div_eq()      { parse (s?, shift_p "/="); }
  rule op_gt()          { parse (s?, shift_p ">"); }
  rule op_gt_eq()       { parse (s?, shift_p ">="); }
  rule op_gt_gt()       { parse (s?, shift_p ">>"); }
  rule op_gt_gt_eq()    { parse (s?, shift_p ">>="); }
  rule op_lt()          { parse (s?, shift_p "<"); }
  rule op_lt_eq()       { parse (s?, shift_p "<="); }
  rule op_lt_lt()       { parse (s?, shift_p "<<"); }
  rule op_lt_lt_eq()    { parse (s?, shift_p "<<="); }
  rule op_minus()       { parse (s?, shift_p "-"); }
  rule op_minus_eq()    { parse (s?, shift_p "-="); }
  rule op_minus_gt()    { parse (s?, shift_p "->"); }
  rule op_minus_minus() { parse (s?, shift_p "--"); }
  rule op_or()          { parse (s?, shift_p "|"); }
  rule op_or_eq()       { parse (s?, shift_p "|="); }
  rule op_or_or()       { parse (s?, shift_p "||"); }
  rule op_plus()        { parse (s?, shift_p "+"); }
  rule op_plus_eq()     { parse (s?, shift_p "+="); }
  rule op_plus_plus()   { parse (s?, shift_p "++"); }
  rule op_qm()          { parse (s?, shift_p "?"); }
  rule op_rem()         { parse (s?, shift_p "%"); }
  rule op_rem_eq()      { parse (s?, shift_p "%="); }
  rule op_star()        { parse (s?, shift_p "*"); }
  rule op_star_eq()     { parse (s?, shift_p "*="); }
  rule op_tilde()       { parse (s?, shift_p "~"); }
}

////////////////////////////////////////////////////////////////////////////////////////////////

rule_dyn lit();

group _: lit(0) {
  rule lit_char() {
    parse (s?, "'", (r"^\'\\\n" | ("\\", r"."))+, "'");
  }

  rule lit_float() {
    parse (s?, r"0-9"*, ".", r"0-9"+, r"f"*, B);
  }

  rule lit_float_exp() {
    parse (s?, r"0-9"+ | (r"0-9"+, ".", r"0-9"*) | (r"0-9"*, ".", r"0-9"+));
    parse (r"eE", r"-+"?, r"0-9"+);
    parse (r"f"*, B)
  }

  rule lit_int() {
    parse (s?, r"0-9"+, r"luLU"*, B);
  }

  rule lit_int_hex() {
    parse (s?, "0x", r"0-9a-fA-F"+, r"luLU"*, B);
  }

  rule lit_str() {
    parse (s?, "\"", (r"^\"\\\n" | ("\\", r"."))*, "\"");
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
// Expressions
////////////////////////////////////////////////////////////////////////////////////////////////

// Primary Expressions
group expr_primary: expr(100) {
  rule expr_paren() { parse (op_paren_0, expr!0, op_paren_1); }
  rule expr_ident() { parse ident; }
  rule expr_lit()   { parse lit; }
}

// Postfix Expressions
group expr_postfix: expr(95) {
  rule expr_field()       { parse (expr!, op_dot, ident); }
  rule expr_field_ptr()   { parse (expr!, op_minus_gt, ident); }
  rule expr_index()       { parse (expr!, op_bracket_0, expr!0, op_bracket_1); }
  rule expr_op_dec_post() { parse (expr!, op_minus_minus); }
  rule expr_op_inc_post() { parse (expr!, op_plus_plus); }

  rule expr_call() { 
    parse (expr!, op_paren_0);
    parse (expr_assignment % op_comma)*;
    parse (op_paren_1);
  } 

  // rule expr_struct() // XXX: initializer_list
  // { parse (op_paren_0, type, op_paren_1); }
}

// Unary Expressions
group expr_unary: expr(90) {
  rule expr_dec_pre()   { parse (op_minus_minus, expr!); }
  rule expr_inc_pre()   { parse (op_plus_plus, expr!); }
  rule expr_op_addr()   { parse (op_and, expr_cast); }
  rule expr_op_deref()  { parse (op_star, expr_cast); }
  rule expr_op_inv()    { parse (op_tilde, expr_cast); }
  rule expr_op_neg()    { parse (op_minus, expr_cast); }
  rule expr_op_not()    { parse (op_ex, expr_cast); }
  rule expr_op_pos()    { parse (op_plus, expr_cast); }
  rule expr_sizeof()    { parse (kw_sizeof, expr!); }

  rule expr_sizeof_paren() { 
    parse (kw_sizeof, op_paren_0, type_name, op_paren_1); 
  }
}

// Cast Expression
group expr_cast: expr(85) {
  rule expr_cast_value() { 
    parse (op_paren_0, type_name, op_paren_1, expr!); 
  }
}

// Multiplicative Expressions
group expr_multiplicative: expr(80) {
  rule expr_op_div() { parse (expr!, op_div, expr); }
  rule expr_op_mul() { parse (expr!, op_star, expr); }
  rule expr_op_rem() { parse (expr!, op_rem, expr); }
}

// Additive Expressions
group expr_additive: expr(75) {
  rule expr_op_add() { parse (expr!, op_plus, expr); }
  rule expr_op_sub() { parse (expr!, op_minus, expr); }
}

// Shift Expressions
group expr_shift: expr(70) {
  rule expr_op_shl() { parse (expr!, op_lt_lt, expr); }
  rule expr_op_shr() { parse (expr!, op_gt_gt, expr); }
}

// Relational Expressions
group expr_relational: expr(65) {
  rule expr_op_gt()  { parse (expr!, op_gt, expr); }
  rule expr_op_gte() { parse (expr!, op_gt_eq, expr); }
  rule expr_op_lt()  { parse (expr!, op_lt, expr); }
  rule expr_op_lte() { parse (expr!, op_lt_eq, expr); }
}

// Equality Expressions
group expr_equality: expr(60) {
  rule expr_op_eq()  { parse (expr!, op_eq_eq, expr); }
  rule expr_op_neq() { parse (expr!, op_ex_eq, expr); }
}

// Binary And, Exlusive Or, Or Expressions
#[part_of(expr, 55)] rule expr_op_bit_and() { parse (expr!, op_and, expr); }
#[part_of(expr, 50)] rule expr_op_bit_xor() { parse (expr!, op_caret, expr); }
#[part_of(expr, 45)] rule expr_op_bit_or()  { parse (expr!, op_or, expr); }

// Logical And, Or Expressions
#[part_of(expr, 40)] rule expr_op_log_and() { parse (expr!, op_and_and, expr); }
#[part_of(expr, 35)] rule expr_op_log_or()  { parse (expr!, op_or_or, expr); }

// Conditional Expression
group expr_constant: expr(30) {
  rule expr_op_cond() { 
    parse (expr, op_qm, expr!0, op_colon, expr!); 
  }
}

// Assignment Expressions
group expr_assignment: expr(25) {
  rule expr_op_assign()     { parse (expr_unary, op_eq, expr!); }
  rule expr_op_assign_add() { parse (expr_unary, op_plus_eq, expr!); }
  rule expr_op_assign_and() { parse (expr_unary, op_and_eq, expr!); }
  rule expr_op_assign_div() { parse (expr_unary, op_div_eq, expr!); }
  rule expr_op_assign_mod() { parse (expr_unary, op_rem_eq, expr!); }
  rule expr_op_assign_mul() { parse (expr_unary, op_star_eq, expr!); }
  rule expr_op_assign_or()  { parse (expr_unary, op_or_eq, expr!); }
  rule expr_op_assign_shl() { parse (expr_unary, op_lt_lt_eq, expr!); }
  rule expr_op_assign_shr() { parse (expr_unary, op_gt_gt_eq, expr!); }
  rule expr_op_assign_sub() { parse (expr_unary, op_minus_eq, expr!); }
  rule expr_op_assign_xor() { parse (expr_unary, op_caret_eq, expr!); }
}

// Comma Expression
#[part_of(expr, 10)] rule expr_op_comma() { 
  parse (expr!, op_comma, expr); 
}

////////////////////////////////////////////////////////////////////////////////////////////////
// Statements
////////////////////////////////////////////////////////////////////////////////////////////////

rule_dyn stmt();

group _: stmt(100) {
  rule stmt_break() {
    parse (kw_break, op_semi);
  }

  rule stmt_case() {
    parse (kw_case, expr_constant, op_colon, stmt!);
  }

  rule stmt_case_default() {
    parse (kw_default, op_colon, stmt!);
  }

  rule stmt_compound() {
    parse (op_brace_0, (declaration | stmt!0)*, op_brace_1);
  }

  rule stmt_continue() {
    parse (kw_continue, op_semi);
  }

  rule stmt_empty() {
    parse (op_semi);
  }

  rule stmt_expr() {
    parse (expr, op_semi);
  }

  rule stmt_do_while() {
    parse (kw_do, stmt!);
    parse (kw_while, op_paren_0, expr, op_paren_1, op_semi);
  }

  rule stmt_for() {
    parse (kw_for, op_paren_0);
    parse (expr?, op_semi, expr?, op_semi, expr?);
    parse (op_paren_1, stmt!);
  }

  rule stmt_goto() {
    parse (kw_goto, ident, op_semi);
  }

  // rule stmt_if() { // XXX: dangling else
  //   parse (kw_if, op_paren_0, expr, op_paren_1);
  //   parse (stmt!);
  //   parse (kw_else, stmt!)?;
  // }

  rule stmt_if() { // XXX: dangling else
    parse (kw_else?, kw_if, op_paren_0, expr, op_paren_1);
    parse (stmt!);
    // parse (kw_else, stmt!)?;
  }

  rule stmt_else() {
    parse (kw_else, stmt!);
  }

  rule stmt_label() {
    parse (ident, op_colon, stmt!);
  }

  rule stmt_return() {
    parse (kw_return, expr?, op_semi);
  }

  rule stmt_switch() {
    parse (kw_switch, op_paren_0, expr, op_paren_1);
    parse (stmt!);
  }

  rule stmt_while() {
    parse (kw_while, op_paren_0, expr, op_paren_1);
    parse (stmt!);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
// Function Specifiers
////////////////////////////////////////////////////////////////////////////////////////////////

rule_dyn fn_spec();

#[part_of(fn_spec, 0)]
rule fs_inline() { parse kw_inline; }

////////////////////////////////////////////////////////////////////////////////////////////////
// Storage Class Specifiers
////////////////////////////////////////////////////////////////////////////////////////////////

rule_dyn storage_class_spec();

group _: storage_class_spec(0) {
  rule scs_auto()      { parse kw_auto; }
  rule scs_extern()    { parse kw_extern; }
  rule scs_register()  { parse kw_register; }
  rule scs_static()    { parse kw_static; }
  rule scs_typedef()   { parse kw_typedef; }
}

////////////////////////////////////////////////////////////////////////////////////////////////
// Type Qualifiers
////////////////////////////////////////////////////////////////////////////////////////////////

rule_dyn type_qual();

group _: type_qual(0) {
  rule tq_const()     { parse kw_const; }
  rule tq_restrict()  { parse kw_restrict; }
  rule tq_volatile()  { parse kw_volatile; }
}

////////////////////////////////////////////////////////////////////////////////////////////////
// Type Specifiers
////////////////////////////////////////////////////////////////////////////////////////////////

rule_dyn type_spec();

group _: type_spec(0) {
  rule ts_char()      { parse kw_char; }
  rule ts_float()     { parse kw_float; }
  rule ts_double()    { parse kw_double; }
  rule ts_int()       { parse kw_int; }
  rule ts_long()      { parse kw_long; }
  rule ts_signed()    { parse kw_signed; }
  rule ts_short()     { parse kw_short; }
  rule ts_type_name() { parse ident; } // the problem case
  rule ts_unsigned()  { parse kw_unsigned; }
  rule ts_void()      { parse kw_void; }

  rule ts_enum() {
    parse (kw_enum, ident ~ (op_brace_0,
      (enumerator %? op_comma)+,
      op_brace_1
    ));
  }

  rule ts_struct() {
    parse (kw_struct, ident ~ (op_brace_0,
      struct_declaration+,
      op_brace_1
    ));
  }

  rule ts_union() {
    parse (kw_union, ident ~ (op_brace_0,
      struct_declaration+,
      op_brace_1
    ));
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
// Designator
////////////////////////////////////////////////////////////////////////////////////////////////

rule_dyn designator();

group _: designator(0) {
  rule desig_index() {
    parse (op_bracket_0, expr_constant, op_bracket_1);
  }

  rule desig_named() {
    parse (op_dot, ident);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
// Initializer
////////////////////////////////////////////////////////////////////////////////////////////////

rule_dyn initializer();

group _: initializer(0) {
  rule init_expr() {
    parse expr_assignment;
  }

  rule init_list() {
    parse (op_brace_0);
    parse (initializer_elem %? op_comma)+;
    parse (op_brace_1);
  }  
}

////////////////////////////////////////////////////////////////////////////////////////////////
// Abstract Declarator
////////////////////////////////////////////////////////////////////////////////////////////////

rule_dyn abstract_declarator_elem();

rule abstract_declarator() {
  parse (pointer ~ abstract_declarator_direct);
}

rule abstract_declarator_direct() {
  parse (op_paren_0, abstract_declarator!0, op_paren_1)?;
  parse abstract_declarator_elem+;
}

group _: abstract_declarator_elem(0) {
  rule adecl_array() { parse (op_bracket_0, expr_assignment, op_bracket_1); }
  rule adecl_array_2() { parse (op_bracket_0, op_star?, op_bracket_1); }
  rule adecl_fn() { parse (op_paren_0, param_list?, op_paren_1); }
}

////////////////////////////////////////////////////////////////////////////////////////////////
// Declarator
////////////////////////////////////////////////////////////////////////////////////////////////

rule_dyn declarator_direct();

rule declarator() {
  parse (pointer*, declarator_direct);
}

rule declarator_fn() {
  parse (pointer*, decl_fn);
}

group _: declarator_direct(0) {
  rule decl_array() {
    parse (declarator_direct!, op_bracket_0);
    parse (kw_static?, type_qual*, expr_assignment);
    parse (op_bracket_1);
  }

  rule decl_array_2() {
    parse (declarator_direct!, op_bracket_0);
    parse (type_qual*, op_star?);
    parse (op_bracket_1);
  }

  rule decl_fn() {
    parse (declarator_direct!, op_paren_0, param_list?, op_paren_1);
  }

  rule decl_ident() {
    parse (ident);
  }

  rule decl_paren() {
    parse (op_paren_0, declarator, op_paren_1);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
// Other
////////////////////////////////////////////////////////////////////////////////////////////////

rule declaration() {
  parse decl_spec+;
  parse ((declarator, (op_eq, initializer)?) % op_comma)*;
  parse op_semi;
}

// rule declarator_init() {
//   parse_g (declarator, (op_eq, initializer)?);
// }

rule decl_spec() {
  parse (fn_spec | storage_class_spec | type_qual | type_spec);
}

rule designation() {
  parse (designator+, op_eq);
}

rule enumerator() {
  // parse_g (ident, (op_eq, expr_constant)?);
  parse (ident, (op_eq, expr_constant)?);
}

rule fn_def() {
  parse decl_spec+;
  parse (declarator_fn, declaration*);
  parse stmt_compound;
}

rule initializer_elem() {
  parse (designation?, initializer);
}

rule param_declaration() {
  parse (decl_spec+, (declarator | abstract_declarator)?);
}

rule param_list() {
  parse (param_declaration % op_comma)+;
  parse (op_comma, op_dot_dot_dot)?;
}

rule pointer() {
  // parse_g (op_star, type_qual*, pointer?);
  parse (op_star, type_qual*, pointer?);
}

rule struct_declaration() {
  parse (type_qual | type_spec)+;
  parse (struct_declarator % op_comma)+;
  parse op_semi;
}

rule struct_declarator() {
  parse (declarator ~ (op_colon, expr_constant));
}

rule type_name() {
  parse (type_qual | type_spec)+;
  parse abstract_declarator?;
}

////////////////////////////////////////////////////////////////////////////////////////////////
// Main
////////////////////////////////////////////////////////////////////////////////////////////////

rule main() {
  parse_g main_elem+; 
}

rule main_elem() {
  parse (fn_def | declaration);
}

////////////////////////////////////////////////////////////////////////////////////////////////
