# Downstream Rust/Rowan language-pack fixture

This fixture owns the same `grammar.ss`, `parser.ss`, `fixtures.ss`,
`parser-test.ss`, and native corpus layout as a real downstream DSL package.
It deliberately has no `gerbil.pkg` or Gerbil installation contract. Upstream
generation CI drives its public
`:gerbil-parser/rust-rowan-grammar-support` declaration through the standalone
`gerbil-parser-rowan-aot` product and verifies the committed Rust module. The
downstream acceptance step is then a standalone Cargo build: it needs only
`gerbil-parser-rowan` and the generated module, never Gerbil, a private engine
module, or a handwritten Rowan sink.
