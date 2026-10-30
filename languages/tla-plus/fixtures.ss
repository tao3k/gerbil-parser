;;; Shared corpus and development loader declarations; no Suite execution.
(import ./parser
        (only-in :clan/poo/object .o)
        (only-in :gerbil-parser/language-support/fixture defsyntax-corpus)
        (only-in :gerbil-parser/language-support/development deflanguage-development-loader LanguageDevelopmentLoader.)
        (only-in :gerbil-parser/src/language/descriptor language-grammar-language language-grammar-version language-grammar-contract))
(export #t)

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

(defsyntax-corpus tla-plus-layout-fixtures
  (identity "tla-plus" "v2" "tla-plus.native-layout.v2")
  (accepted
   ("tla-plus/layout/aligned" tla-layout-aligned (text "---- MODULE J ----\nInit ==\n  /\\ TRUE\n  /\\ FALSE\n====\n") SourceFile (JunctionExpression)))
  (rejected
   ("tla-plus/layout/incomplete" tla-layout-incomplete (text "---- MODULE J ----\nInit ==\n  /\\\n====\n"))))

(defsyntax-corpus tla-plus-sany-candidate-fixtures
  (identity "tla-plus" "p4-draft" "tla-plus.native-sany-candidate.p4")
  (accepted
   ("tla-plus/candidate/proof" tla-candidate-proof (text "---- MODULE P ----\nTHEOREM TRUE\nBY TRUE\n====\n") SourceFile (TerminalProof)))
  (rejected
   ("tla-plus/candidate/missing-qed" tla-candidate-missing-qed (text "---- MODULE P ----\nTHEOREM TRUE\n<1>1. TRUE OBVIOUS\n====\n"))))

(deflanguage-development-loader tla-plus-core-test-language
  (grammar tla-plus-core-language-grammar)
  (parse parse-tla-plus-core-test)
  (slots metadata: '((grammar-format . concise-dsl))
         fixtures: tla-plus-core-fixtures))

(deflanguage-development-loader tla-plus-layout-test-language
  (grammar tla-plus-layout-language-grammar)
  (parse parse-tla-plus-layout-test)
  (slots metadata: '((grammar-format . concise-dsl))
         fixtures: tla-plus-layout-fixtures))

(deflanguage-development-loader tla-plus-sany-candidate-test-language
  (grammar tla-plus-sany-candidate-language-grammar)
  (parse parse-tla-plus-sany-candidate-test)
  (slots metadata: '((grammar-format . concise-dsl))
         fixtures: tla-plus-sany-candidate-fixtures))
