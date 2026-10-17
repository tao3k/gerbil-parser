;;; Real source admission controls; no parser performance claims.
(import (only-in :std/test check test-case test-suite)
        (only-in :gerbil-parser/t/fixtures/event-walk-source
                 read-event-walk-source read-event-walk-source-port
                 write-event-walk-source-witness event-walk-witness-current?))
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
    (test-case "unsupported reader datums cannot disappear during serialization"
      (check (rejected? (lambda ()
                         (call-with-output-string
                           (lambda (port) (write-event-walk-source-witness '(#\x) port))))) => #t))))
