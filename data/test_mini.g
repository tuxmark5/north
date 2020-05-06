grammar

rule ident() {
  //parse ("XX");
  parse (r"a-zA-Z_", r"a-zA-Z_0-9"*);
}

rule main() {
  parse (ident, ":", ident);
}
