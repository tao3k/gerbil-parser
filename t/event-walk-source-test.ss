;;; Real source admission controls; no parser performance claims.
(import (only-in :std/test check test-case test-suite)
        (only-in :gerbil-parser/t/fixtures/event-walk-source
                 read-event-walk-source read-event-walk-source-port
                 write-event-walk-source-witness event-walk-witness-current?
                 read-event-iterative-source read-source-definition source-mutation-sites))
(export event-walk-source-test)

(def (walker-macro)
  (call-with-input-file "src/runtime/event-program.ss"
    (lambda (port)
      (let loop ()
        (let (form (read port))
          (cond ((eof-object? form) (error "missing real source macro"))
                ((and (pair? form) (eq? (car form) 'defsyntax)
                      (pair? (cadr form)) (eq? (caadr form) 'event-program-walk/inline)) form)
                (else (loop))))))))
(def (rejected? thunk)
  (with-catch (lambda (_) #t) (lambda () (thunk) #f)))
(def (read-forms forms)
  (call-with-input-string
    (call-with-output-string
      (lambda (port) (for-each (lambda (form) (write form port)) forms)))
    read-event-walk-source-port))

(def (source-procedure declaration)
  (eval (list 'lambda (cdr (cadr declaration)) (caddr declaration))))
(def (row-fixture count width)
  (let loop ((i 0) (rows '()))
    (if (= i width) (reverse rows)
      (loop (+ i 1) (cons (list (modulo i count) (modulo i 3) i) rows)))))
(def (expected-rows rows state)
  (map (lambda (row) (cons (cadr row) (caddr row)))
       (filter (lambda (row) (= (car row) state)) rows)))

(def event-walk-source-test
  (test-suite "actual Scheme source witness admission"
    (test-case "reader retains complete native declaration and all clauses"
      (let (source (read-event-walk-source "src/runtime/event-program.ss"))
        (check (car source) => 'def)
        (check (cadr source) => '(walk code delta moved? depth))
        (check (length source) => 3)
        (check (length (cdr (caddr source))) => 10)
        (check (read-forms (list (walker-macro))) => source)))
    (test-case "missing duplicate and enlarged macro scopes fail closed"
      (let (macro (walker-macro))
        (check (rejected? (lambda () (read-forms '()))) => #t)
        (check (rejected? (lambda () (read-forms (list macro macro)))) => #t)
        (check (rejected? (lambda () (read-forms (list (append macro '((void))))))) => #t)
        (let* ((syntax-case (caddr macro))
               (clause (cadddr syntax-case))
               (template (cadr clause))
               (body (cadr template))
               (changed (list 'defsyntax (cadr macro)
                          (append (list (car syntax-case) (cadr syntax-case) (caddr syntax-case))
                            (list (list (car clause)
                                    (list 'syntax (append body '((unexpected-effect))))))))))
          (check (rejected? (lambda () (read-forms (list changed)))) => #t))))
    (test-case "generated proof input must match current syntax and generator"
      (let (source (read-event-walk-source "src/runtime/event-program.ss"))
        (check (event-walk-witness-current? source "formal/ParserEngine/WalkWitness.lean") => #t)
        (check (event-walk-witness-current? (append source '((unexpected-effect)))
                 "formal/ParserEngine/WalkWitness.lean") => #f)))
    (test-case "real extracted vector write preserves aliases and other slots"
      (let* ((source (read-event-iterative-source "src/runtime/event-program.ss"))
             (sites (source-mutation-sites source))
             (write-site (find (lambda (site) (eq? (car site) 'vector-set!)) sites))
             (apply-write! (eval (list 'lambda '(frame i) write-site))))
        (check (length sites) => 2)
        (for-each
         (lambda (index)
           (let* ((frame (vector 7 index -9 #t)) (alias frame))
             (apply-write! frame index)
             (check (eq? frame alias) => #t)
             (check alias => (vector 7 (+ index 1) -9 #t))
             (check (vector-length frame) => 4))) '(0 1 63 64 129))))
    (test-case "real extracted local assignment preserves block order and tail"
      (let* ((sites (source-mutation-sites (read-event-iterative-source "src/runtime/event-program.ss")))
             (assignment (find (lambda (site) (eq? (car site) 'set!)) sites))
             (apply-assignment
              (eval (list 'lambda '(block delta moved? initial)
                      (list 'let '((frames initial)) assignment 'frames))))
             (tail (list (vector 99 #f -9 #t)))
             (actual (foldl (lambda (block frames) (apply-assignment block -9 #t frames)) tail '(1 2 3))))
        (check actual => (list (vector 3 #f -9 #t) (vector 2 #f -9 #t) (vector 1 #f -9 #t) (vector 99 #f -9 #t)))
        (check (eq? (list-tail actual 3) tail) => #t)
        (check tail => (list (vector 99 #f -9 #t)))))
    (test-case "actual LR source groups arbitrary rows without losing duplicates"
      (let* ((declaration (read-source-definition "src/compiler/lr-compiler.ss" 'index-state-rows))
             (index-rows (source-procedure declaration)))
        (check (length (source-mutation-sites declaration)) => 2)
        (check (index-rows '() 0 cadr caddr) => '#())
        (for-each
         (lambda (count)
           (for-each
            (lambda (width)
              (let* ((rows (row-fixture count width)) (actual (index-rows rows count cadr caddr)))
                (check (vector-length actual) => count)
                (let loop ((state 0))
                  (when (< state count)
                    (check (vector-ref actual state) => (expected-rows rows state))
                    (loop (+ state 1)))))) '(0 1 2 17 65 129))) '(1 2 7))
        (check (rejected? (lambda () (index-rows '((1 key value)) 1 cadr caddr))) => #t)))
    (test-case "unsupported reader datums cannot disappear during serialization"
      (check (rejected? (lambda ()
                         (call-with-output-string
                           (lambda (port) (write-event-walk-source-witness '(#\x) port))))) => #t))))
