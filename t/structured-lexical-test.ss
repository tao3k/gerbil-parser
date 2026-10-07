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
(export structured-lexical-test structured-lexical-language-grammar)
(deflanguage structured-lexical
 (identity "structured-lexical" "v1" "structured-lexical.v1")
 (root entry)
 (lex (space Space (whitespace+))
      (identifier Identifier (text-profile (ref tla-identifier)))
      (proof-step-name ProofStep (text-profile (ref tla-proof-start)))
      (proof-reference ProofReference (text-profile (ref tla-proof-reference))))
 (rules (entry (node Entry (choice identifier proof-step-name proof-reference))))
 (extras space))
(def structured-lexical-test
 (test-suite "portable structured lexical IR"
  (test-case "real TLA+ rules use closed IR and character endpoints at arbitrary offsets"
   (let (rules (cdr (assq 'lexical-rules (language-grammar-ir tla-plus-sany-candidate-language-grammar))))
    (for-each (lambda (group)
     (let* ((expression (cadr (assq (car group) rules)))
            (scan (make-text-profile-scanner (cadr expression))))
      (check (car expression) => 'text-profile)
      (for-each (lambda (row)
       (check (scan (car row) 0) => (cadr row))
       (check (scan (string-append "字" (car row)) 1)
              => (and (cadr row) (+ 1 (cadr row))))) (cdr group)))) structured-lexical-cases)))
  (test-case "guards reject malformed classes, empty spans and nullable children"
   (for-each (lambda (value) (check (text-profile? value) => #f))
    '((ends-in (numeric) (optional (literal "x")))
      (ends-not-in (unknown) (literal "x"))
      (run-containing (numeric) (unknown) 1 #f)
      (run-containing (numeric) (numeric) 2 1)))
   (let (scan (make-text-profile-scanner '(run-containing (numeric) (numeric) 0 #f)))
    (check (scan "" 0) => #f)
    (check (scan "١" 0) => 1)))))
