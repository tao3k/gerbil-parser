;;; -*- Gerbil -*-
;;; Native source fixtures for the arithmetic v1 reference language.

(import ../../support/fixture)
(export arithmetic-v1-basic-fixture)

(defsyntax-fixture arithmetic-v1-basic-fixture
  (identity "arithmetic/v1/basic"
            "arithmetic reference language"
            "v1"
            "arithmetic-expression.v1")
  (source "corpus/basic.expr")
  (expect accepted SourceFile (Expression)))
