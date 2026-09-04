;;; -*- Gerbil -*-
;;; Versioned TLA+ sources embedded at expansion time.

(import :gerbil-parser/languages/tla-plus/1-5/parser
        ../../support/fixture)
(export tla-plus-1-5-fixtures)

(defsyntax-corpus tla-plus-1-5-fixtures
  (identity "tla-plus" +tla-plus-language-version+ +tla-plus-syntax-contract-v1+)
  (accepted
   ("tla-plus/examples/hour-clock" tla-plus-hour-clock
    "corpus/tlaplus-examples/HourClock.tla" SourceFile
    (Module ExtendsDeclaration VariableDeclaration OperatorDefinition
            TheoremDeclaration))
   ("tla-plus/core/counter" tla-plus-counter
    "corpus/core/Counter.tla" SourceFile
    (Module ExtendsDeclaration ConstantDeclaration VariableDeclaration
            OperatorDefinition TheoremDeclaration))
   ("tla-plus/core/nested-comment" tla-plus-nested-comment
    "corpus/core/NestedComment.tla" SourceFile
    (Module VariableDeclaration OperatorDefinition)))
  (rejected))
