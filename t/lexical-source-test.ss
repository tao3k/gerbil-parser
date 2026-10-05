;;; Prepared captures belong to a request, including nested and parallel parses.
(import :std/test
        (only-in :gerbil-parser/src/runtime/lexical-source
                 prepare-lexical-source-plan call-with-lexical-source
                 scan-header-delimiter scan-header-data))
(export lexical-source-test)
(def capture-plan
 (prepare-lexical-source-plan
  '((field (header-delimiter "記" 2 0))
    (body (choice (header-data "記" 2 ";") (precedence 1 (header-delimiter "記" 2 1))))
    (other (header-data "REC" 2 "\n")))))
(def (scan-record source)
 (call-with-lexical-source capture-plan source
  (lambda ()
   (list (scan-header-delimiter source 1 "記" 2 0)
         (scan-header-delimiter source 2 "記" 2 1)
         (scan-header-data source 3 "記" 2 ";")))))
(def lexical-source-test
 (test-suite "request-owned lexical captures"
  (test-case "nested lexical choices deduplicate capture preparation"
   (check capture-plan => '(("記" 2) ("REC" 2)))
   (check (scan-record "記|§α|") => '(2 3 4))
   (check (scan-record "記*$α*") => '(2 3 4))
   (check (scan-record "記||α|") => '(#f #f #f)))
  (test-case "nested foreign sources and failed scopes restore the parent capture"
   (let (source "記|§α|")
    (call-with-lexical-source capture-plan source
     (lambda ()
      (check (scan-record "記*$α*") => '(2 3 4))
      (check (with-catch (lambda (_) 'caught)
               (lambda () (call-with-lexical-source capture-plan "記||α|"
                (lambda () (error "nested scope failed"))))) => 'caught)
      (check (scan-header-delimiter source 1 "記" 2 0) => 2)
      (check (scan-header-data source 3 "記" 2 ";") => 4)
      ;; A raw scanner on a different source never reads a parent capture.
      (check (scan-header-delimiter "記*$α*" 1 "記" 2 0) => 2)))))
  (test-case "parallel source requests retain independent captures"
   (let ((left (spawn (lambda () (let loop ((n 100) (rows '()))
                  (if (zero? n) rows (loop (- n 1) (cons (scan-record "記|§α|") rows)))))))
         (right (spawn (lambda () (let loop ((n 100) (rows '()))
                  (if (zero? n) rows (loop (- n 1) (cons (scan-record "記*$α*") rows))))))))
    (check (andmap (cut equal? <> '(2 3 4)) (thread-join! left)) => #t)
    (check (andmap (cut equal? <> '(2 3 4)) (thread-join! right)) => #t)))))
