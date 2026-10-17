;;; Source correspondence controls use the shared constrained parser-test DSL.
(import :gerbil-parser/language-test-support
        (only-in :gerbil-parser/t/fixtures/event-walk-source
                 read-event-walk-source read-event-walk-source-port
                 write-event-walk-source-data event-walk-data-current?
                 read-event-iterative-source read-source-definition source-mutation-sites))

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
    (call-with-output-string (lambda (port) (for-each (lambda (form) (write form port)) forms)))
    read-event-walk-source-port))
(def (enlarged-scope macro)
  (let* ((syntax-case (caddr macro)) (clause (cadddr syntax-case)) (template (cadr clause)) (body (cadr template)))
    (list 'defsyntax (cadr macro)
      (append (list (car syntax-case) (cadr syntax-case) (caddr syntax-case))
        (list (list (car clause) (list 'syntax (append body '((unexpected-effect))))))))))
(def (source-procedure declaration)
  (eval (list 'lambda (cdr (cadr declaration)) (caddr declaration))))
(def (row-fixture count width)
  (let loop ((i 0) (rows '()))
    (if (= i width) (reverse rows)
      (loop (+ i 1) (cons (list (modulo i count) (modulo i 3) i) rows)))))
(def (expected-row-vector rows count)
  (let loop ((state 0) (slots '()))
    (if (= state count) (list->vector (reverse slots))
      (loop (+ state 1)
        (cons (map (lambda (row) (cons (cadr row) (caddr row)))
                   (filter (lambda (row) (= (car row) state)) rows)) slots)))))
(def (site-procedure kind parameters)
  (let* ((sites (source-mutation-sites (read-event-iterative-source "src/runtime/event-program.ss")))
         (site (find (lambda (site) (eq? (car site) kind)) sites)))
    (eval (list 'lambda parameters site))))
(def (assignment-procedure)
  (let* ((sites (source-mutation-sites (read-event-iterative-source "src/runtime/event-program.ss")))
         (site (find (lambda (site) (eq? (car site) 'set!)) sites)))
    (eval (list 'lambda '(block delta moved? initial) (list 'let '((frames initial)) site 'frames)))))
(def (unsupported-datum?)
  (rejected? (lambda () (call-with-output-string
                         (lambda (port) (write-event-walk-source-data '(#\x) port))))))

(deflanguage-parser-tests event-walk-source-test "actual Scheme source witness admission"
  (engine)
  (property "reader retains complete declaration and all clauses"
    (bindings (source (read-event-walk-source "src/runtime/event-program.ss")))
    (equal (car source) 'def)
    (equal (cadr source) '(walk code delta moved? depth))
    (equal (length source) 3)
    (equal (length (cdr (caddr source))) 10)
    (equal (read-forms (list (walker-macro))) source))
  (property "missing duplicate and enlarged scopes fail closed"
    (bindings (macro (walker-macro)))
    (equal (rejected? (lambda () (read-forms '()))) #t)
    (equal (rejected? (lambda () (read-forms (list macro macro)))) #t)
    (equal (rejected? (lambda () (read-forms (list (append macro '((void))))))) #t)
    (equal (rejected? (lambda () (read-forms (list (enlarged-scope macro))))) #t))
  (property "generated data matches current syntax and renderer"
    (bindings (source (read-event-walk-source "src/runtime/event-program.ss")))
    (equal (event-walk-data-current? source "formal/ParserEngine/SourceData.lean") #t)
    (equal (event-walk-data-current? (append source '((unexpected-effect))) "formal/ParserEngine/SourceData.lean") #f))
  (matrix "real vector write preserves aliases and other slots"
    (setup (apply-write! (site-procedure 'vector-set! '(frame i))))
    ((index '(0 1 63 64 129)))
    (bindings (frame (vector 7 index -9 #t)) (alias frame) (effect (apply-write! frame index)))
    (equal (eq? frame alias) #t)
    (equal alias (vector 7 (+ index 1) -9 #t))
    (equal (vector-length frame) 4))
  (property "real local assignment preserves block order and tail"
    (bindings (assign (assignment-procedure)) (tail (list (vector 99 #f -9 #t)))
              (actual (foldl (lambda (block frames) (assign block -9 #t frames)) tail '(1 2 3))))
    (equal actual (list (vector 3 #f -9 #t) (vector 2 #f -9 #t) (vector 1 #f -9 #t) (vector 99 #f -9 #t)))
    (equal (eq? (list-tail actual 3) tail) #t)
    (equal tail (list (vector 99 #f -9 #t))))
  (matrix "actual LR source groups rows without losing duplicates"
    (setup (declaration (read-source-definition "src/compiler/lr-compiler.ss" 'index-state-rows))
           (index-rows (source-procedure declaration)))
    ((count '(1 2 7)) (width '(0 1 2 17 65 129)))
    (bindings (rows (row-fixture count width)))
    (equal (length (source-mutation-sites declaration)) 2)
    (equal (index-rows rows count cadr caddr) (expected-row-vector rows count)))
  (property "empty LR table and out-of-bounds admission"
    (bindings (index-rows (source-procedure (read-source-definition "src/compiler/lr-compiler.ss" 'index-state-rows))))
    (equal (index-rows '() 0 cadr caddr) '#())
    (equal (rejected? (lambda () (index-rows '((1 key value)) 1 cadr caddr))) #t))
  (property "unsupported reader data cannot disappear"
    (bindings) (equal (unsupported-datum?) #t)))
