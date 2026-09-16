;;; -*- Gerbil -*-
;;; Authoring-only typed source catalog for openCypher 2024.1.

(import (only-in :gerbil-parser/src/language-support/grammar-source
                 defsyntax-iso-bnf-source)
        (only-in ./grammar
                 +opencypher-version+
                 +opencypher-commit+
                 +opencypher-bnf-digest+))
(export opencypher-2024-1-bnf)

(defsyntax-iso-bnf-source opencypher-2024-1-bnf
  (identity "opencypher" +opencypher-version+ +opencypher-commit+)
  (digest +opencypher-bnf-digest+)
  (source "grammar-source/openCypher.bnf"))
