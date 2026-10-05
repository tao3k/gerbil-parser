;;; Closed profile execution consumes the real FHIRPath declaration data.
(import (only-in "text-profile-cases.ss" fhirpath-profile-cases)
        :std/test
        (only-in :gerbil-parser/src/grammar/lexical-algebra text-profile? lexical-expression?)
        (only-in :gerbil-parser/src/runtime/scan make-text-profile-scanner)
        (only-in :gerbil-parser/languages/fhirpath/grammar fhirpath-parser-ir)
        (only-in :gerbil-parser/languages/hl7/grammar hl7-parser-ir))
(export text-profile-test)

(def text-profile-test
  (test-suite "closed text profile IR"
    (test-case "real language profiles preserve rollback, commitment and Unicode endpoints"
      (let (rules (cdr (assq 'lexical-rules fhirpath-parser-ir)))
        (for-each
         (lambda (group)
           (let* ((expression (cadr (assq (car group) rules)))
                  (scan (make-text-profile-scanner (cadr expression))))
             (check (car expression) => 'text-profile)
             (for-each (lambda (row)
                         (check (scan (car row) 0) => (cadr row))
                         (check (scan (string-append "α" (car row)) 1)
                                => (and (cadr row) (+ 1 (cadr row))))) (cdr group))))
         fhirpath-profile-cases)))
    (test-case "HL7 segment ids keep maximal-run rejection and CRLF stays one terminator"
      (let (rules (cdr (assq 'lexical-rules hl7-parser-ir)))
        (for-each
         (lambda (group)
           (let* ((expression (cadr (assq (car group) rules)))
                  (scan (make-text-profile-scanner (cadr expression))))
             (for-each (lambda (row) (check (scan (car row) 0) => (cadr row))) (cdr group))))
         '((segment-id ("MSH|" 3) ("AB٣|" 3) ("AB４|" 3) ("MSHA|" #f)
                       ("MSH3|" #f) ("MS|" #f) ("msH|" #f) ("MSH²" 3))
           (segment-terminator ("\r\nx" 2) ("\rx" 1) ("\nx" 1) ("x" #f) ("" #f))))))
    (test-case "invalid, callback and nullable roots reject before scanner construction"
      (for-each
       (lambda (profile)
         (check (text-profile? profile) => #f)
         (check (lexical-expression? (list 'text-profile profile)) => #f)
         (check (with-catch (lambda (_) #t)
                  (lambda () (make-text-profile-scanner profile) #f)) => #t))
       '(() callback (seq) (literal "") (run (numeric) -1 #f)
         (run (numeric) 2 1) (run (numeric) 4294967296 #f)
         (run (numeric) 1 4294967296)
         (seq (run (numeric) 4294967295 #f) (literal "x")) (run (unknown) 1 #f) (run (characters "") 1 #f)
         (optional (literal "x")) (not-next (numeric))
         (if-next (numeric) (literal "x"))
         (seq (literal "x") (if-next (numeric) invalid)))))
    (test-case "prepared profiles retain source and offset bounds"
      (let (scan (make-text-profile-scanner '(run (numeric) 1 #f)))
        (check (scan "1" -1) => #f)
        (check (scan "1" 2) => #f)
        (check (scan "1" 1) => #f)
        (check (scan "1" 0.5) => #f)
        (check (scan "١２x" 0) => 2)))))
