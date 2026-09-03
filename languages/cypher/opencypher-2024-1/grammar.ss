;;; -*- Gerbil -*-
;;; openCypher 2024.1 official ISO WG3 BNF grammar-source owner.

(import :gerbil-parser/languages/support/grammar-source)
(export +opencypher-version+
        +opencypher-commit+
        +opencypher-bnf-digest+
        +opencypher-syntax-contract-v1+
        opencypher-2024-1-bnf)

(def +opencypher-version+ "2024.1")
(def +opencypher-commit+
  "30b451d3b7c94ee5a84a0fdc223947a442dd9493")
(def +opencypher-bnf-digest+
  "sha256:c0b5454f001b59b401756158bf88e27847c8ace71f1abc8df1e05f8b710b9f50")
(def +opencypher-syntax-contract-v1+ "opencypher-2024.1-syntax.v1")

(defsyntax-iso-bnf-source opencypher-2024-1-bnf
  (identity "opencypher" +opencypher-version+ +opencypher-commit+)
  (digest +opencypher-bnf-digest+)
  (source "grammar-source/openCypher.bnf"))
