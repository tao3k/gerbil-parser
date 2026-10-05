;;; -*- Gerbil -*-
;;; Authoring-only typed source catalog for openCypher 2024.1.

(import (only-in :gerbil-parser/language-support/grammar-source
                 defsyntax-iso-bnf-source)
        (only-in ./grammar
                 +opencypher-version+
                 +opencypher-commit+
                 +opencypher-bnf-digest+))
(export opencypher-bnf)

(defsyntax-iso-bnf-source opencypher-bnf
  (identity "opencypher" +opencypher-version+ +opencypher-commit+)
  (digest +opencypher-bnf-digest+)
  (source "grammar-source/openCypher.bnf"))
