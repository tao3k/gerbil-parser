;;; Source correspondence controls use the shared constrained parser-test DSL.
(import :gerbil-parser/language-test-support
        (only-in :gerbil-parser/src/testing/source-witness parser-source-data write-parser-source-data)
        (only-in :gerbil-parser/t/fixtures/event-walk-source
                 read-event-walk-source read-event-walk-source-port
                 write-event-walk-source-data event-walk-data-current?
                 read-event-iterative-source read-source-definition source-mutation-sites)
        (only-in :gerbil-parser/src/compiler/lr production-rhs)
        (only-in :gerbil-parser/src/compiler/lr-automaton make-item-layout make-core-item
                 core-item-count core-item-production-id core-item-dot))

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

(def (publication-observation construction state-count publication-count valid?)
  (let* ((source (read-source-definition "src/compiler/lr-compiler.ss" 'compile-lr-spec/selected))
         (tail (foldl (lambda (index form) (list-ref form index)) source '(3 3 6 3 3)))
         (procedure
           (eval (list 'lambda
                   '(construction state-count build-actions build-gotos validate-lr-tables trace-lr-phase)
                   (list 'let
                     (map (lambda (name) (list name #f))
                          '(states lookaheads lookahead-offsets transitions table terminal-values
                            layout core-symbols conflict-policy rules root started
                            case-insensitive? productions declarations nullable first lr0-state-visit-count lookahead-item-visit-count))
                     tail))))
         (events '()) (actions (make-vector state-count '())) (gotos (make-vector state-count '()))
         (spec (procedure construction state-count
                 (lambda ignored (set! events (cons 'actions events)) (values actions publication-count))
                 (lambda ignored (set! events (cons 'gotos events)) gotos)
                 (lambda (productions actual-actions actual-gotos)
                   (set! events (cons (list 'validate (eq? actual-actions actions) (eq? actual-gotos gotos)) events))
                   (unless valid? (error "publication validation rejected")))
                 (lambda ignored (void)))))
    (list spec (reverse events) actions gotos)))

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
  (property "reader retains the SDK width value declaration"
    (bindings (declaration (read-source-definition "src/compiler/funcs.ss" 'compiler-index-field-width)))
    (equal declaration
      '(def compiler-index-field-width (integer-length (##greatest-fixnum)))))
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
    (bindings) (equal (unsupported-datum?) #t))
  (scenario "uninterned names fail before source data publication"
    (scope ((name (##string->uninterned-symbol "root")))
      (using (call-with-output-string) (port)
        (raises (write-parser-source-data
          (parser-source-data (ParserEngine.SourceIdentityFixture value name)) port) true)
        (equal (get-output-string port) ""))))
  (matrix "complete extracted layout agrees with native coordinates and physical slots"
    (setup (declaration (read-source-definition "src/compiler/lr-automaton.ss" 'make-item-layout))
           (construct (eval (list 'lambda
                              (append (cdr (cadr declaration)) '(production-rhs make-lr-item-layout))
                              (caddr declaration)))))
    ((widths '(() (0) (0 0) (1 3 0 1) (65 0 1) (0 129)))
     (terminal-count '(0 1 3)))
    (bindings (table (list->vector
                      (map (lambda (id width) (list id 'root (make-list width '(terminal token word)) 'concat #f))
                           (iota (length widths)) widths)))
              (terminals (make-vector terminal-count 'word))
              (slots (construct table terminals production-rhs vector))
              (native (make-item-layout table terminals))
              (expected-offsets
                (let loop ((remaining widths) (offset 0) (out '()))
                  (if (null? remaining) (reverse (cons offset out))
                    (loop (cdr remaining) (+ offset 1 (car remaining)) (cons offset out)))))
              (expected-owners
                (apply append (map (lambda (id width) (make-list (+ width 1) id))
                                   (iota (length widths)) widths))))
    (equal (length (source-mutation-sites declaration)) 2)
    (equal (vector-ref slots 0) terminal-count)
    (equal (vector->list (vector-ref slots 1)) expected-offsets)
    (equal (vector->list (vector-ref slots 2)) expected-owners)
    (equal (core-item-count native) (length expected-owners))
    (equal (map (lambda (item) (core-item-production-id item native)) (iota (length expected-owners))) expected-owners)
    (equal (map (lambda (item)
                  (make-core-item (core-item-production-id item native) (core-item-dot item native) native))
                (iota (length expected-owners)))
           (iota (length expected-owners))))
  (matrix "actual final LR suffix preserves fields and validation order"
    (setup)
    ((construction '(lalr canonical-lr1 partitioned-lr1 follow-partition-lr1)) (count '(1 2 7)))
    (bindings (observed (publication-observation construction count count #t))
              (spec (car observed)))
    (equal (cadr observed) '(actions gotos (validate #t #t)))
    (equal (cdr (assq 'state-count spec)) count)
    (equal (eq? (cdr (assq 'actions spec)) (caddr observed)) #t)
    (equal (eq? (cdr (assq 'gotos spec)) (cadddr observed)) #t)
    (equal (cdr (assq 'schema spec)) "gerbil-parser.lr-spec.v1")
    (equal (cdr (assq 'algorithm spec))
           (case construction ((lalr) 'lalr1-lr0-fixed-point-v1)
             ((canonical-lr1) 'canonical-lr1-reference-v1)
             ((partitioned-lr1) 'conflict-partitioned-lr1-v1)
             (else 'follow-partition-lr1-v1)))
    (equal (map car spec)
      (append (case construction ((lalr) '())
                ((follow-partition-lr1) '(follow-block-count output-item-count))
                (else '(canonical-state-count output-item-count)))
              '(schema case-insensitive? productions declarations nullable first algorithm state-count
                lr0-state-visit-count lookahead-item-visit-count actions gotos))))
  (matrix "publication count or validator failure prevents a spec"
    (setup)
    ((construction '(lalr canonical-lr1 partitioned-lr1 follow-partition-lr1)))
    (bindings)
    (equal (rejected? (lambda () (publication-observation construction 2 1 #t))) #t)
    (equal (rejected? (lambda () (publication-observation construction 2 3 #t))) #t)
    (equal (rejected? (lambda () (publication-observation construction 2 2 #f))) #t)))
