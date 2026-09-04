;;; -*- Gerbil -*-
;;; OpenGQL 1.9.0 immutable grammar-source admission for ISO GQL 2024.

(import :gerbil-parser/languages/support/grammar-source)
(export +gql-antlr4-digest+
        gql-iso-antlr4-source)

(def +gql-antlr4-digest+
  "sha256:e1b4a24c6b88dedddc0a1fff97df0fc30bf118cea51539e26d71c717cb737bbf")

(defsyntax-antlr4-source gql-iso-antlr4-source
  (identity "gql" "1.9.0"
            "16ea71bd320ad07fd2c46a3066afbaef7d226922")
  (digest
   "sha256:e1b4a24c6b88dedddc0a1fff97df0fc30bf118cea51539e26d71c717cb737bbf")
  (source "grammar-source/GQL.g4"))
