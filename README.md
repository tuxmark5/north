# North

North is an experimental SEVM (Scannerless Earley Virtual Machine) parser implementation. 

It is a research tool and not indented to be production ready.

## Usage

Parse 20 MB rust file with north:
```bash
cargo run --release -p test -- parse -g data/rust_v3.g -i data/input_rust_650k.rs -p
```

Parse similarly sized C file with north:
```bash
cargo run --release -p test -- parse -g data/ansi_c.g -i data/input_gcc_470k.i -p
```
