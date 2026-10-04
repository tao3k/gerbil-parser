# Language pack boundary

Each language exposes one public `parser.ss` module. Its entry is defined with
`deflanguage-loader` from `:gerbil-parser/language-support`. Grammar rules, lexical
roles and language-specific syntax remain in the pack; validation, compilation,
artifact admission and parser assembly belong to the engine.

```scheme
(import (only-in :gerbil-parser/language-support
                 deflanguage-loader LanguageLoader.))

(deflanguage-loader example-language
  (grammar example-language-grammar)
  (parse parse-example))
```

The loader is a POO object, admitted when the module loads. `parse-example` binds
the engine method once. Parsing neither rebuilds the loader nor repeats POO
contract admission. Engine slots bind schema, descriptor, language, version,
syntax contract, capabilities and dispatch; extension declarations cannot
replace them. Parent prototypes cannot replace the bound engine slots either.

## Extension slots and contracts

Use ordinary POO inheritance for language options and tooling services. Supply
POO contracts when an extension needs additional constraints. These contracts
check the effective inherited object before its parse entry is published.

```scheme
(import (only-in :clan/poo/object .ref))

(deflanguage-loader (example-language :: self ExampleDialect.)
  (descriptor example-language-grammar)
  (parse parse-example)
  (slots display-name: "Example"
         (summary (string-append (.ref self 'display-name) " dialect")))
  (contracts ExampleDialectContract))
```

The explicit `self` name binds computations in the extension slots. The shorter
`(name @ prototype)` form is available when slots do not need a self binding.

The grammar descriptor remains the compiler's admission boundary. Slots do not
make arbitrary parser callbacks into a portable Scanner/Parser IR, or qualify a
backend automatically. A source parser such as Bash uses the same loader but
advertises `scheme-source`; generated grammar entries advertise
`grammar-ir`, `parser-ir` and `scheme`. Rust AOT and C ABI products retain their
own artifact/capability checks.

## Grammar DSL

Native grammar packs use `deflanguage`: `identity`, `root`, `lex` and `rules`
define the syntax. The engine infers syntax kinds, fields, terminals and lexical
rules. Alternatives of the same named node contribute to one field catalog.
Use explicit `(token name)` or `(reference name)` when both namespaces contain
the same name; an ambiguous bare identifier is rejected.

`node-fields` declares optional public fields required by an existing syntax
contract, without repeating the complete inferred catalog. `flow` can retain a
pack's phase names; the default is source → lexical → parser → CST. The engine
validates both through the same canonical Grammar IR compiler.

Imported ANTLR/ISO-BNF grammars keep their source-admission DSL; they produce the
same generated descriptor and use the same loader. Bash's handwritten source
parser and TLA+'s multiple grammar scopes remain explicit implementation
boundaries. Their shared-IR migration is not implied by entry unification.

## Ownership

- `grammar.ss`: declarative syntax and pinned syntax identity.
- `parser.ss`: the public loader entry and deliberately exported tooling APIs.
- `scanner.ss`, word/source/projection modules: only syntax-specific behavior
  required by the language; no VM lifecycle, FFI transport or build orchestration.
- Corpus and tests: semantics and lossless source conformance.
- Generated backends: engine-owned IR derivations. Existing checked-in HCL and
  arithmetic specializations still require digest checks and deterministic
  regeneration; they are not an authoring requirement for new packs.
