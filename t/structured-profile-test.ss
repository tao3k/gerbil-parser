(import (only-in :gerbil-parser/t/fixtures/fixture-release bind-fixture-grammar-release))
;;; Independent language recipes exercise the same closed structured engine.
(import :std/test
        (only-in :clan/poo/object .o .cc)
        :gerbil-parser/language-support/structured
        (only-in :gerbil-parser/language-support deflanguage deftext-profile)
        (only-in :gerbil-parser/src/runtime/scan make-text-profile-scanner)
        (only-in :gerbil-parser/src/runtime/lexical-source scan-module-text call-with-lexical-source prepare-lexical-source-plan)
        (only-in :gerbil-parser/src/language/entry parse-language-source)
        (only-in :gerbil-parser/src/runtime/cst parse-artifact->cst)
        (only-in :gerbil-parser/src/runtime/artifact parse-artifact-ref))
(export structured-profile-test)
(def unit-expression '(module-text "~~~" "UNIT" "!!!" "(*" "*)" "\\*" "_"))
(def unit-plan (prepare-lexical-source-plan (list (list 'text unit-expression))))
(def (unit-text source start)
 (call-with-lexical-source unit-plan source (lambda () (scan-module-text source start unit-expression))))
(deftext-profile square-proof
 (seq (literal "[") (run (numeric) 1 #f) (literal "]")
      (run (union (alphabetic) (numeric) (characters "_")) 0 #f)
      (run (characters ".") 0 #f)))
(def square-start (make-text-profile-scanner
 '(ends-not-in (union (alphabetic) (numeric) (characters "_"))
   (seq (literal "[") (run (numeric) 1 #f) (literal "]")
        (run (union (alphabetic) (numeric) (characters "_")) 0 #f)
        (run (characters ".") 0 #f)))))
(def square-reference (make-text-profile-scanner
 '(ends-in (union (alphabetic) (numeric) (characters "_"))
   (seq (literal "[") (run (numeric) 1 #f) (literal "]")
        (run (union (alphabetic) (numeric) (characters "_")) 0 #f)
        (run (characters ".") 0 #f)))))
(def square-identifier (make-text-profile-scanner
 '(run-containing (union (alphabetic) (numeric) (characters "_"))
                 (union (alphabetic) (characters "_")) 1 #f)))
(begin
 (deflanguage checklist
  (syntax
   (lexical
    (root checklist)
    (lex (space Space (whitespace+))
      (step StepName (text-profile (ends-not-in (union (alphabetic) (numeric) (characters "_")) (ref square-proof))))
      (word Word (identifier)))
    (extras space)))
  (rules (checklist (node Checklist (repeat1 (field item (reference item)))))
        (item (node Item (field heading step) (field command word)))))
 (bind-fixture-grammar-release checklist "checklist" "v1" "checklist.v1") )
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
   (check (rejects? (lambda () (bind-structured-proof-policy (.cc StructuredProofPolicy. 'denied-kinds '("wrong"))))) => #t))))
