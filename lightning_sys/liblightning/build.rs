use {
  std::{
    env,
    path::Path,
  },
};

fn main() {
  let root = env::var("CARGO_MANIFEST_DIR").unwrap();
  let root = Path::new(&root).join("liblightning");

  gcc::Build::new()
    .define("HAVE_FFSL", None)
    .include(&root.join("include"))
    .warnings(false)
    .file("liblightning/lib/lightning.c")
    .compile("liblightning.a");
}
