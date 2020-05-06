builtins
builtins_grammar
grammar

rule s() {
  parse (r" \t\n"+, S1);
}

rule ident() {
  parse (s?, r"a-zA-Z_");
  parse (r"a-zA-Z_0-9"*, B);
}

rule main() {
  parse ident;
}
