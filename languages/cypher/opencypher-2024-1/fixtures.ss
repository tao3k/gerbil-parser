;;; -*- Gerbil -*-
;;; Digest-bound openCypher 2024.1 representative acceptance corpus.

(import (only-in :gerbil-parser/language-support
                 defsyntax-corpus
                 syntax-fixture-expected-status))
(export opencypher-2024-1-fixtures
        opencypher-2024-1-accepted-fixtures)

(defsyntax-corpus opencypher-2024-1-fixtures
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

(def opencypher-2024-1-accepted-fixtures
  (filter (lambda (fixture)
            (eq? (syntax-fixture-expected-status fixture) 'accepted))
          opencypher-2024-1-fixtures))
