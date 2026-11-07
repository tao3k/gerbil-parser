;;; Shared corpus and development loader declarations; no Suite execution.
(import ./parser
        (only-in :clan/poo/object .o)
        (only-in :gerbil-parser/language-support/fixture defsyntax-corpus)
        (only-in :gerbil-parser/language-support/development deflanguage-development-loader LanguageDevelopmentLoader.))
(export #t)

(defsyntax-corpus opencypher-fixtures
  (identity "opencypher" "2024.1" "opencypher-2024.1-syntax.v1")
  (accepted
   ("opencypher/2024.1/match-return" opencypher-match-return
    "corpus/representative/match-return.cypher" program
    (|match statement| |return statement|))
   ("opencypher/2024.1/create-return" opencypher-create-return
    "corpus/representative/create-return.cypher" program
    (|create statement| |return statement|))
   ("opencypher/2024.1/path-filter" opencypher-path-filter
    "corpus/representative/path-filter.cypher" program
    (|match statement| |where clause| |return statement|))
   ("opencypher/2024.1/unwind" opencypher-unwind
    "corpus/representative/unwind.cypher" program
    (|unwind statement| |return statement|)))
  (rejected
   ("opencypher/2024.1/invalid/match-only" opencypher-match-only
    "corpus/invalid/match-only.cypher")))

(deflanguage-development-loader opencypher-test-language
  (grammar opencypher-language-grammar)
  (parse parse-opencypher-test)
  (slots metadata: `((grammar-format . iso-bnf) (reference-commit . ,+opencypher-commit+) (source-digest . ,+opencypher-bnf-digest+))
         fixtures: opencypher-fixtures))
