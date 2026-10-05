;;; -*- Gerbil -*-
;;; Native source fixtures for the arithmetic v1 reference language.

(import (only-in :gerbil-parser/language-support defsyntax-fixture))
(export arithmetic-basic-fixture)

(defsyntax-fixture arithmetic-basic-fixture
  (identity "arithmetic/v1/basic"
            "arithmetic"
            "v1"
            "arithmetic-expression.v1")
  (source "corpus/basic.expr")
  (expect accepted SourceFile (Expression)))
