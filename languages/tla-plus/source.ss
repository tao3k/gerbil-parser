;;; -*- Gerbil -*-
;;; Official SANY JavaCC grammar admitted only as native syntax evidence.

(import (only-in :gerbil-parser/language-support/grammar-source
                 defsyntax-javacc-source))
(export +tla-plus-syntax-source+
        +tla-plus-examples-commit+
        +tla-plus-sany-release+
        +tla-plus-sany-commit+
        +tla-plus-sany-grammar-blob+
        +tla-plus-sany-grammar-digest+
        tla-plus-sany-source)

(def +tla-plus-syntax-source+ "Specifying Systems, Chapter 15: TLAPlusGrammar")
(def +tla-plus-examples-commit+ "ceeaa904140e3e03781cb2a79cd6c6d8b8b08e10")
(def +tla-plus-sany-release+ "v1.7.4")
(def +tla-plus-sany-commit+ "5a47802b5c391f59ecdd44117981f4ff8c0656ba")
(def +tla-plus-sany-grammar-blob+ "bf9e7acb5337f4b6c2a4d6a973a1a65c95e72f56")
(def +tla-plus-sany-grammar-digest+
  "sha256:15edd079cf16cf91556ba66b496c9ffc16f26ab30856753ed58e60b0d54f2d07")

(defsyntax-javacc-source tla-plus-sany-source
  (identity "tla-plus" "v1.7.4"
            "5a47802b5c391f59ecdd44117981f4ff8c0656ba")
  (digest
   "sha256:15edd079cf16cf91556ba66b496c9ffc16f26ab30856753ed58e60b0d54f2d07")
  (source "grammar-source/tla+.jj"))
