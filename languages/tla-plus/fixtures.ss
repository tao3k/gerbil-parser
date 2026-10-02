;;; -*- Gerbil -*-
;;; Versioned TLA+ sources embedded at expansion time.

(import (only-in ./grammars/core tla-plus-core-language-grammar)
        (only-in :gerbil-parser/src/language/descriptor
                 language-grammar-language
                 language-grammar-version
                 language-grammar-contract)
        (only-in :gerbil-parser/language-support
                 defsyntax-corpus syntax-fixture-expected-status))
(export tla-plus-core-fixtures tla-plus-core-accepted-fixtures
        tla-plus-core-rejected-fixtures)

(defsyntax-corpus tla-plus-core-fixtures
  (identity (language-grammar-language tla-plus-core-language-grammar)
            (language-grammar-version tla-plus-core-language-grammar)
            (language-grammar-contract tla-plus-core-language-grammar))
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

(def tla-plus-core-accepted-fixtures
  (filter (lambda (fixture)
            (eq? (syntax-fixture-expected-status fixture) 'accepted))
          tla-plus-core-fixtures))

(def tla-plus-core-rejected-fixtures
  (filter (lambda (fixture)
            (eq? (syntax-fixture-expected-status fixture) 'rejected))
          tla-plus-core-fixtures))
