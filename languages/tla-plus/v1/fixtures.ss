;;; -*- Gerbil -*-
;;; Versioned TLA+ sources embedded at expansion time.

(import :gerbil-parser/languages/tla-plus/v1/parser
        (only-in :gerbil-parser/language-support
                 defsyntax-corpus syntax-fixture-expected-status))
(export tla-plus-v1-fixtures tla-plus-v1-accepted-fixtures
        tla-plus-v1-rejected-fixtures)

(defsyntax-corpus tla-plus-v1-fixtures
  (identity "tla-plus" +tla-plus-contract-version+ +tla-plus-syntax-contract+)
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
    (Module VariableDeclaration OperatorDefinition))
   ("tla-plus/core/structured-expressions" tla-plus-structured-expressions
    "corpus/core/StructuredExpressions.tla" SourceFile
    (Module OperatorDefinition Expression IfExpression ChooseExpression
            QuantifiedExpression TupleExpression SetExpression
            FunctionConstructor OperatorApplication LetExpression
            CaseExpression RecordExpression SetFilterExpression))
   ("tla-plus/core/module-forms" tla-plus-module-forms
    "corpus/core/ModuleForms.tla" SourceFile
    (Module RecursiveDeclaration InstanceDeclaration AssumptionDeclaration
            TheoremDeclaration OperatorDefinition)))
  (rejected
   ("tla-plus/core/malformed-if" tla-plus-malformed-if
    "corpus/core/MalformedIf.tla")))

(def tla-plus-v1-accepted-fixtures
  (filter (lambda (fixture)
            (eq? (syntax-fixture-expected-status fixture) 'accepted))
          tla-plus-v1-fixtures))

(def tla-plus-v1-rejected-fixtures
  (filter (lambda (fixture)
            (eq? (syntax-fixture-expected-status fixture) 'rejected))
          tla-plus-v1-fixtures))
