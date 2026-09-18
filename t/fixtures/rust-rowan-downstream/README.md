# Downstream Rust/Rowan language-pack fixture

This fixture owns the same `grammar.ss`, `parser.ss`, `fixtures.ss`,
`parser-test.ss`, and native corpus layout as a real downstream DSL package.
Native CI generates its Rust module through the public
`:gerbil-parser/rust-rowan-support` facade after compiling the language pack,
then compiles and tests this standalone Cargo consumer. The fixture never
imports a private Rust engine module and never implements a Rowan sink.
