;;; Independent language recipes exercise the same closed structured engine.
(import :std/test
        (only-in :clan/poo/object .o .cc)
        :gerbil-parser/language-support/structured
        (only-in :gerbil-parser/src/runtime/structured bind-structured-scanners)
        (only-in :gerbil-parser/language-support deflanguage)
        (only-in :gerbil-parser/src/language/entry parse-language-source)
        (only-in :gerbil-parser/src/runtime/cst parse-artifact->cst)
        (only-in :gerbil-parser/src/runtime/artifact parse-artifact-ref))
(export structured-profile-test)
(deflanguage-structured-scanners
 (profile (.o (:: self StructuredLexemeProfile.) open: #\[ close: #\]
              header-border: "~~~" header-word: "UNIT" end-border: "!!!"))
 (identifier square-identifier) (proof-step square-step)
 (proof-reference square-reference) (proof-start square-start) (module-text unit-text))
(deflanguage checklist
 (identity "checklist" "v1" "checklist.v1")
 (root checklist)
 (lex (space Space (whitespace+))
      (step StepName (external checklist-step-v1 square-start))
      (word Word (identifier)))
 (rules (checklist (node Checklist (repeat1 (field item (reference item)))))
        (item (node Item (field heading step) (field command word))))
 (extras space))
(def checklist-policy
 (bind-structured-proof-policy
  (.o (:: self StructuredProofPolicy.) proof-kind: 'Checklist step-field: 'item
      name-field: 'heading body-field: 'command close: #\] terminal-command: "END")))
(def (rejects? thunk)
 (with-catch (lambda (_) #t) (lambda () (thunk) #f)))
(def structured-profile-test
 (test-suite "inherited structured engine recipes"
  (test-case "different proof spelling and module frames use the same scanners"
   (check (square-start "[1]1. END" 0) => 5)
   (check (square-reference "[1]label" 0) => 8)
   (check (square-identifier "123" 0) => #f)
   (check (square-identifier "1_name" 0) => 6)
   (check (unit-text "preamble\n~~~ UNIT Demo ~~~" 0) => 9)
   (check (square-start "<1>1. END" 0) => #f))
  (test-case "renamed CST fields and terminal command reuse the proof stack"
   (check (checklist-policy (parse-artifact->cst (parse-language-source checklist-language-grammar "[1]1. END"))) => #f)
   (let (diagnostic (checklist-policy (parse-artifact->cst (parse-language-source checklist-language-grammar "[1]1. OPEN"))))
    (check (cdr (assq 'failureKind diagnostic)) => 'proof-level-rejected)))
  (test-case "malformed overrides are rejected before scanner binding"
   (check (rejects? (lambda () (bind-structured-scanners (.cc StructuredLexemeProfile. 'header-border "-x")))) => #t)
   (check (rejects? (lambda () (bind-structured-proof-policy (.cc StructuredProofPolicy. 'denied-kinds '("wrong"))))) => #t))))
