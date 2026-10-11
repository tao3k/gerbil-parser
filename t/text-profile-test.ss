;;; Closed profile execution consumes the real FHIRPath declaration data.
(import (only-in "text-profile-cases.ss" fhirpath-profile-cases)
        :std/test
        (only-in :gerbil-parser/src/grammar/lexical-algebra text-profile? lexical-expression?)
        (only-in :gerbil-parser/src/runtime/scan make-text-profile-scanner)
        (only-in :gerbil-parser/languages/fhirpath/grammar fhirpath-parser-ir)
        (only-in :gerbil-parser/languages/hl7/grammar hl7-parser-ir))
(export text-profile-test)

;;; Independent declaration interpreter, never prepared engine predicates.
(def (class-oracle expression character)
  (case (car expression)
    ((numeric) (char-numeric? character))
    ((alphabetic) (char-alphabetic? character))
    ((ascii-letter)
     (or (and (char>=? character #\A) (char<=? character #\Z))
         (and (char>=? character #\a) (char<=? character #\z))))
    ((characters) (and (memv character (string->list (cadr expression))) #t))
    ((union) (ormap (lambda (part) (class-oracle part character)) (cdr expression)))))

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
    (test-case "closed union classes agree over ASCII and Unicode boundaries"
      (let ((observations 0)
            (characters (append (map integer->char (iota 256))
                                (string->list "α中😀٣４²é𝟡"))))
        (for-each
         (lambda (class)
           (let (scan (make-text-profile-scanner (list 'run class 1 #f)))
             (for-each
              (lambda (character)
                (let* ((source (string character))
                       (expected (and (class-oracle class character) 1)))
                  (unless (equal? (scan source 0) expected)
                    (error "text class declaration changed" class character expected))
                  (set! observations (+ observations 1)))) characters)))
         '((union (ascii-letter) (characters "_") (numeric))
           (union (characters "ABCDEFGHIJKLMNOPQRSTUVWXYZ") (numeric))
           (union (alphabetic) (numeric) (characters "_"))
           (union (characters "éα😀_") (numeric))
           (union (union (characters "_") (numeric)) (ascii-letter))
           (union (characters "aaé") (characters "é_"))))
        (check observations => 1584)))
    (test-case "prepared character unions isolate declaration string mutation"
      (let* ((characters (string-copy "_α"))
             (scan (make-text-profile-scanner
                    (list 'run (list 'union (list 'characters characters) '(numeric)) 1 #f))))
        (string-set! characters 0 #\x)
        (string-set! characters 1 #\β)
        (check (scan "_α٣" 0) => 3)
        (check (scan "x" 0) => #f)
        (check (scan "β" 0) => #f)))
    (test-case "prepared profiles retain source and offset bounds"
      (let (scan (make-text-profile-scanner '(run (numeric) 1 #f)))
        (check (scan "1" -1) => #f)
        (check (scan "1" 2) => #f)
        (check (scan "1" 1) => #f)
        (check (scan "1" 0.5) => #f)
        (check (scan "١２x" 0) => 2)))))
