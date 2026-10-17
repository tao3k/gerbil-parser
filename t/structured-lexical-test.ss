(import :gerbil-parser/language-test-support)
(import (only-in :gerbil-parser/t/fixtures/fixture-release bind-fixture-grammar-release))
;;; Real language profiles and an independently generated portable grammar.
(import :std/test
        (only-in :gerbil-parser/language-support deflanguage)
        (only-in :gerbil-parser/languages/tla-plus/grammar
                 tla-proof-start tla-proof-reference tla-identifier)
        (only-in :gerbil-parser/languages/tla-plus/parser tla-plus-sany-candidate-language-grammar)
        (only-in :gerbil-parser/src/language/descriptor language-grammar-ir)
        (only-in :gerbil-parser/src/runtime/scan make-text-profile-scanner)
        (only-in :gerbil-parser/src/grammar/lexical-algebra text-profile?)
        (only-in "structured-lexical-cases.ss" structured-lexical-cases))
(export  structured-lexical-language-grammar)
(begin
 (deflanguage structured-lexical
  (syntax
   (lexical
    (root entry)
    (lex (space Space (whitespace+))
      (identifier Identifier (text-profile (ref tla-identifier)))
      (proof-step-name ProofStep (text-profile (ref tla-proof-start)))
      (proof-reference ProofReference (text-profile (ref tla-proof-reference))))
    (extras space)))
  (rules (entry (node Entry (choice identifier proof-step-name proof-reference)))))
 (bind-fixture-grammar-release structured-lexical "structured-lexical" "v1" "structured-lexical.v1") )
(deflanguage-parser-tests structured-lexical-test "portable structured lexical IR"
  (engine)
  (scenario "real TLA+ rules use closed IR and character endpoints at arbitrary offsets"
   (scope ((rules (cdr (assq 'lexical-rules (language-grammar-ir tla-plus-sany-candidate-language-grammar)))))
    (each (group) (structured-lexical-cases)
     (sequential ((expression (cadr (assq (car group) rules)))
            (scan (make-text-profile-scanner (cadr expression))))
      (equal (car expression)  'text-profile)
      (each (row) ((cdr group))
       (equal (scan (car row) 0)  (cadr row))
       (equal (scan (string-append "字" (car row)) 1)
               (and (cadr row) (+ 1 (cadr row)))))))))
  (scenario "guards reject malformed classes, empty spans and nullable children"
   (each (value) ('((ends-in (numeric) (optional (literal "x")))
      (ends-not-in (unknown) (literal "x"))
      (run-containing (numeric) (unknown) 1 #f)
      (run-containing (numeric) (numeric) 2 1))) (equal (text-profile? value)  #f))
   (scope ((scan (make-text-profile-scanner '(run-containing (numeric) (numeric) 0 #f))))
    (equal (scan "" 0)  #f)
    (equal (scan "١" 0)  1))))
