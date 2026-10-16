#!/usr/bin/env gxi
(import (only-in :gerbil-parser/t/fixtures/fixture-release bind-fixture-grammar-release))
;;; -*- Gerbil -*-
;;; Explicit closing boundaries belong to the declaring language grammar.

(import (only-in :gerbil-parser/t/fixtures/layout-columns wide-frame-roots closure-frame-roots
                 host-slot-control host-mutex-control host-spin-backend host-max-processors)
        :std/test
        :gerbil-parser/src/grammar/algebra
        (only-in :gerbil-parser/src/language/grammar deflanguage)
        (only-in :gerbil-parser/src/runtime/parser
                 parse-source parse-source/checkpoints
                 prepare-contextual-parser parse-source/contextual)
        (only-in :gerbil-parser/src/compiler/machine
                 parser-machine-runtime parser-machine-grammar-digest parser-machine-ir)
        (only-in :gerbil-parser/src/modules/parser/contextual-objects
                 make-contextual-method make-contextual-role make-contextual-scan-rule)
        (only-in :gerbil-parser/src/compiler/contextual-parser-ir compile-contextual-parser)
        (only-in :gerbil-parser/src/compiler/lr-compiler compile-lr-spec)
        (only-in :gerbil-parser/src/compiler/lr lr-spec-ref production-rhs)
        (only-in :gerbil-parser/src/runtime/lr-parser
                 lr-prepare lr-parse/prepared lr-parse/prepared/receipt
                 current-lr-event-program-enabled? lr-runtime-for-current-semantic-backend
                 lr-initial-checkpoint lr-checkpoint-drive lr-runtime-with-action-selector)
        (only-in :gerbil-parser/src/runtime/lr-parser/prepare make-lr-action-selector)
        (only-in :gerbil-parser/src/runtime/lr-action-index index-action-row)
        (only-in :gerbil-parser/src/runtime/artifact
                 make-success-parse-artifact
                 parse-artifact-success? parse-artifact-valid? parse-artifact-roundtrip)
        (only-in :gerbil-parser/src/runtime/token make-token)
        (only-in :gerbil-parser/src/runtime/recognition recognition-node-kind)
        (only-in :gerbil-parser/src/runtime/layout
                 make-layout-columns current-layout-columns current-layout-frames
                 layout-after-end layout-current-action-row prepare-layout-action-selector))
(export layout-boundary-test)

(begin
 (deflanguage closing-boundary-probe
  (syntax
   (lexical
    (root source-file)
    (lex (word Word (identifier))
       (punctuation Punctuation (literals "(" "|>"))
       (space Space (whitespace+)))
    (extras space)
    (keywords)
    (recoveries)
    (conflicts selective-glr)
    (case-insensitive #f)))
  (rules
   (source-file (node SourceFile (seq "(" (field items (reference items)) "END")))
   (items
    (node Items
      (seq (layout-start "|>") (field item word)
           (repeat (seq (layout-next "|>") (field item word)))
           (layout-end "END"))))))
 (bind-fixture-grammar-release closing-boundary-probe "closing-boundary-probe" "v1" "closing-boundary-probe.v1") )

(def (layout-contextual-product machine)
  (compile-contextual-parser
   (parser-machine-ir machine) (parser-machine-grammar-digest machine)
   (list (make-contextual-role
          'layout-probe
          (list (make-contextual-method 'word 'any 'any 'word 'word))))
   '(normal) '(body) '(word)
   (list (make-contextual-scan-rule 'word 'normal 'word '(identifier) 0 'keep))
   'normal '((body () ()))))

(def (native-layout-fallback-origin-case)
  ;; Precedence selects the recursive shift, but indentation may need one or
  ;; both suppressed empty-body reductions in their original emission order.
  (for-each
   (lambda (additional)
     (let ((rules `((root (alias Root (sequence (reference block) (token word))))
                    (block (sequence (layout-start "|>") (reference body) (layout-end)))
                    (body (choice (precedence right 1 (sequence (token word) (reference body)))
                                  (precedence left 0 (empty)) ,@additional)))))
       (for-each
        (lambda (route)
          (let* ((spec (compile-lr-spec rules 'root 'selective-glr #f route))
                 (rows (lr-spec-ref spec 'actions))
                 (indexed (list->vector (map index-action-row (vector->list rows))))
                 (select (prepare-layout-action-selector rows indexed #f))
                 (productions (lr-spec-ref spec 'productions)) (guards 0)
                 (saved (map (lambda (row) (map (lambda (cell) (cons (car cell) (cdr cell))) row))
                             (vector->list rows))))
            (for-each
             (lambda (state)
               (for-each
                (lambda (cell)
                  (when (and (equal? (car cell) '(terminal token word)) (eq? (cadr cell) 'layout-guard))
                    (set! guards (+ guards 1))
                    (let* ((primary (caddr cell)) (fallback (cadddr cell))
                           (reductions (if (eq? (car fallback) 'fork) (cdr fallback) (list fallback)))
                           (token (make-token 'word "x" 0 1)))
                      (check (car primary) => 'shift)
                      (check (length reductions) => (+ 1 (length additional)))
                      (for-each (lambda (reduction)
                        (check (car reduction) => 'reduce)
                        (check (> (cadr reduction) 0) => #t)
                        (check (production-rhs (list-ref productions (cadr reduction))) => '())) reductions)
                      (when (pair? (cdr reductions))
                        (check (< (cadr (car reductions)) (cadr (cadr reductions))) => #t))
                      (parameterize ((current-layout-columns (make-layout-columns "x")))
                        (for-each
                         (lambda (frames expected)
                           (parameterize ((current-layout-frames frames))
                             (check (cdr (layout-current-action-row (vector-ref indexed state) token #f)) => expected)
                             (check (cdr (select state token)) => expected)))
                         '(() ((100 . "outer")) ()) (list primary fallback primary))))))
                (vector-ref rows state)))
             (iota (vector-length rows)))
            (check (> guards 0) => #t)
            ;; A blocked body shift must close layout and leave x to the root.
            (parameterize ((current-layout-columns (make-layout-columns "|>\nx"))
                           (current-layout-frames '()))
              (let-values (((_root rest)
                            (lr-parse/prepared (lr-prepare spec)
                              (list (make-token 'punctuation "|>" 0 2) (make-token 'word "x" 3 4)))))
                (check rest => '())))
            (check (vector->list rows) => saved)))
        '(lalr canonical-lr1 partitioned-lr1 follow-partition-lr1))))
   '(() ((precedence left -1 (empty))))))

(def (layout-unwind-exception-case)
  (let* ((rules '((root (alias Root (sequence (reference block) (token word))))
                 (block (sequence (layout-start "|>") (reference body) (layout-end)))
                 (body (choice (precedence right 1 (sequence (token word) (reference body)))
                               (precedence left 0 (empty))))))
         (base (lr-prepare (compile-lr-spec rules 'root 'selective-glr)))
         (outer-columns (make-layout-columns "outer")) (outer-frames '((100 . "outer")))
         (outer-column-values (u32vector->list outer-columns))
         (inner-columns (make-layout-columns "|>\nx")) (sentinel (cons 'cancelled 'request))
         (saw-owned-frame? #f)
         (runtime (lr-runtime-with-action-selector base
           (lambda (rows index insensitive? layout?)
             (let (select (make-lr-action-selector rows index insensitive? layout?))
               (lambda (state token)
                 (when (pair? (current-layout-frames))
                   (set! saw-owned-frame? #t)
                   ;; A branch-local throw must unwind its pushed marker.
                   (raise sentinel))
                 (select state token)))))))
    (parameterize ((current-layout-columns outer-columns) (current-layout-frames outer-frames))
      (check (with-catch (lambda (condition) (eq? condition sentinel))
               (lambda ()
                 (parameterize ((current-layout-columns inner-columns) (current-layout-frames '()))
                   (lr-parse/prepared runtime
                     (list (make-token 'punctuation "|>" 0 2) (make-token 'word "x" 3 4))))
                 #f)) => #t)
      (check saw-owned-frame? => #t)
      (check (eq? (current-layout-columns) outer-columns) => #t)
      (check (eq? (current-layout-frames) outer-frames) => #t)
      (check (u32vector->list (current-layout-columns)) => outer-column-values)
      ;; Successful empty-list close is distinct from the #f failure result.
      (parameterize ((current-layout-columns inner-columns) (current-layout-frames '((1 . "|>"))))
        (check (layout-after-end #f) => '())
        (check (layout-after-end (make-token 'punctuation "|>" 0 2) '("|>")) => #f))
      (parameterize ((current-layout-frames '()))
        (check (layout-after-end #f) => #f)))))

(def (closure-frame-gc-case)
  (for-each
   (lambda (order)
     (let* ((seed (vector order))
            (readers (closure-frame-roots seed (lambda () (thread-yield!) (##gc))))
            (first (if order (car readers) (cdr readers)))
            (second (if order (cdr readers) (car readers))))
       (##gc)
       (check (car (first)) => (if order 'left 'right))
       (check (car (second)) => (if order 'right 'left))
       (check (eq? (cdr (first)) seed) => #t)
       (check (eq? (cdr (second)) seed) => #t)))
   '(#t #f)))

(def (wide-frame-gc-case)
  (def (exercise worker)
    (for-each
     (lambda (round)
       (let* ((seed (vector worker round))
              (roots (wide-frame-roots seed (lambda () (thread-yield!) (##gc)))))
         (check (map car roots) => (iota 96))
         (check (andmap (lambda (root) (eq? (cdr root) seed)) roots) => #t)))
     '(0 1))
    #t)
  (let ((left (spawn (lambda () (exercise 'left))))
        (right (spawn (lambda () (exercise 'right)))))
    (check (thread-join! left) => #t)
    (check (thread-join! right) => #t)))

(def (layout-continuation-gc-case)
  (def (copy-pairs value)
    (if (pair? value) (cons (copy-pairs (car value)) (copy-pairs (cdr value))) value))
  ;; Capture inside the actual GLR/layout driver after its marker shift. The
  ;; suspended parameter binding, not an extra root variable, owns inner columns.
  (let* ((rules '((root (alias Root (sequence (reference block) (token word))))
                 (block (sequence (layout-start "|>") (reference body) (layout-end)))
                 (body (choice (precedence right 1 (sequence (token word) (reference body)))
                               (precedence left 0 (empty))))))
         (spec (compile-lr-spec rules 'root 'selective-glr))
         (base (lr-prepare spec)) (published (copy-pairs (vector->list (lr-spec-ref spec 'actions))))
         (outer-columns (make-layout-columns "outer")) (outer-frames '((100 . "outer")))
         (outer-values (u32vector->list outer-columns))
         (resume #f) (escape #f) (resumes 0) (visits 0) (completed 0)
         (outer-before 0) (outer-after 0) (inner-before 0) (inner-after 0)
         (runtime (lr-runtime-with-action-selector base
           (lambda (rows index insensitive? layout?)
             (let (select (make-lr-action-selector rows index insensitive? layout?))
               (lambda (state token)
                 (when (and (not resume) (pair? (current-layout-frames)))
                   (call/cc (lambda (continuation) (set! resume continuation) (escape 'suspended)))
                   (set! visits (+ visits 1))
                   (check (current-layout-frames) => '((1 . "|>")))
                   (check (u32vector->list (current-layout-columns)) => '(0 1 2 0 1))
                   (##gc)
                   (check (vector->list (lr-spec-ref spec 'actions)) => published))
                 (select state token)))))))
    (dynamic-wind
     (lambda () (set! outer-before (+ outer-before 1)))
     (lambda ()
       (parameterize ((current-layout-columns outer-columns) (current-layout-frames outer-frames))
         (let ((result
                (call/cc
                 (lambda (exit)
                   (set! escape exit)
                   (dynamic-wind
                    (lambda ()
                      (set! inner-before (+ inner-before 1))
                      (check (eq? (current-layout-columns) outer-columns) => #t))
                    (lambda ()
                      (parameterize ((current-layout-columns (make-layout-columns "|>\nx"))
                                     (current-layout-frames '()))
                        (let-values (((root rest) (lr-parse/prepared runtime
                                      (list (make-token 'punctuation "|>" 0 2) (make-token 'word "x" 3 4)))))
                          (check rest => '())
                          (check (recognition-node-kind root) => 'Root)
                          'accepted)))
                    (lambda ()
                      (set! inner-after (+ inner-after 1))
                      (check (eq? (current-layout-columns) outer-columns) => #t)))))))
           (check (eq? (current-layout-columns) outer-columns) => #t)
           (check (eq? (current-layout-frames) outer-frames) => #t)
           (check (u32vector->list outer-columns) => outer-values)
           (when (eq? result 'accepted) (set! completed (+ completed 1)))
           (when (< resumes 2)
             (set! resumes (+ resumes 1))
             (##gc)
             (resume 'again)))))
     (lambda () (set! outer-after (+ outer-after 1))))
    (check (list visits completed inner-before inner-after outer-before outer-after) => '(2 2 3 3 1 1))))

(def layout-boundary-test
  (test-suite "Grammar-owned layout closing boundaries"
    (test-case "compiled host slot addresses agree after frame pointer adjustment"
      (check (host-slot-control) => 0))
    (test-case "host mutex imports writes made after OS thread creation"
      (check (host-mutex-control) => 0)
      (let ((backend (host-spin-backend)) (processors (host-max-processors)))
        (check (and (memv backend '(0 1 2)) (> processors 0)) => #t)
        (when (= backend 0) (check processors => 1))))
    (test-case "compiled closure environments retain future uses across collection"
      (closure-frame-gc-case))
    (test-case "generated wide live frames retain every slot across worker collections"
      (wide-frame-gc-case))
    (test-case "actual layout continuations survive GC and repeated dynamic reentry"
      (layout-continuation-gc-case))
    (test-case "layout branch exceptions restore owned outer columns and frames"
      (layout-unwind-exception-case))
    (test-case "native layout fallback retains suppressed reduce origins and immutable rows"
      (native-layout-fallback-origin-case))
    (test-case "layout continuations are rejected before contextual preparation"
      (let* ((machine closing-boundary-probe-parser)
             (runtime (parser-machine-runtime machine))
             (product (layout-contextual-product machine)))
        (for-each
         (lambda (events?)
           (parameterize ((current-lr-event-program-enabled? events?))
             (for-each
              (lambda (create)
                (check (with-catch (lambda (condition) (error-message condition)) create)
                       => "LR checkpoints require a runtime without layout"))
              (list (lambda () (lr-initial-checkpoint runtime '()) #f)
                    (lambda () (prepare-contextual-parser machine product) #f)
                    (lambda () (parse-source/contextual machine product "( |> x END") #f)))))
         '(#f #t))))
    (test-case "layout checkpoint source requests retain complete GLR artifacts"
      (for-each
       (lambda (source)
         (let-values (((artifact tokens modes snapshots)
                       (parse-source/checkpoints closing-boundary-probe-parser source 1)))
           (check artifact => (parse-source closing-boundary-probe-parser source))
           (check (parse-artifact-valid? artifact) => #t)
           (check (parse-artifact-roundtrip artifact) => source)
           (check tokens => #f)
           (check modes => #f)
           (check snapshots => '#())))
       '("( |> x END" "( |> x\n  |> y END" "( |>")))
    (test-case "plain prepared runtimes ignore outer layout columns and frames"
      (for-each
       (lambda (rules)
         (let ((runtime (lr-prepare (compile-lr-spec rules 'root 'selective-glr)))
               (tokens (list (make-token 'word "x" 0 1)))
               (columns (make-layout-columns "x"))
               (frames '((100 . "outer"))))
           (for-each
            (lambda (events?)
              (parameterize ((current-lr-event-program-enabled? events?))
                (let (selected (lr-runtime-for-current-semantic-backend runtime))
                  (let-values (((expected rest) (lr-parse/prepared selected tokens)))
                    (check rest => '())
                    (def (artifact root)
                      (make-success-parse-artifact
                       (string-append "sha256:" (make-string 64 #\0)) "x" tokens root #f))
                    (parameterize ((current-layout-columns columns) (current-layout-frames frames))
                      (let-values (((root rest) (lr-parse/prepared selected tokens)))
                        (check rest => '())
                        (check (artifact root) => (artifact expected)))
                      ;; The independent materialized GLR entry has its own
                      ;; fast and memoized action lookups, even for plain IR.
                      (let-values (((root rest receipt) (lr-parse/prepared/receipt selected tokens)))
                        (check rest => '())
                        (check (artifact root) => (artifact expected)))
                      (check (eq? columns (current-layout-columns)) => #t)
                      (check (eq? frames (current-layout-frames)) => #t))))))
            '(#f #t))))
       '(((root (alias Root (token word))))
         ((root (alias Root (choice (reference first) (reference second))))
          (first (precedence dynamic 2 (token word)))
          (second (precedence dynamic 1 (token word)))))))
    (test-case "streaming LR callbacks can parse inside an active outer layout request"
      (let* ((runtime (lr-prepare (compile-lr-spec '((root (alias Root (token word)))) 'root)))
             (input (make-token 'word "x" 0 1))
             (pending (list input)) (shifted '())
             (columns (make-layout-columns "x")) (frames '((100 . "outer"))))
        (parameterize ((current-layout-columns columns) (current-layout-frames frames))
          (let-values (((status payload)
                        (lr-checkpoint-drive
                         (lr-initial-checkpoint runtime '())
                         (lambda (_mode)
                           (and (pair? pending)
                                (let (token (car pending))
                                  (set! pending (cdr pending)) token)))
                         (lambda (token _states _values _actions _shifts)
                           (set! shifted (cons token shifted))))))
            (check status => 'accepted)
            (check (cadr payload) => '())
            (check shifted => (list input)))
          (check (eq? columns (current-layout-columns)) => #t)
          (check (eq? frames (current-layout-frames)) => #t))))
    (test-case "layout casefold lookup retains the declared row and guard decision"
      (def entries
        '(((terminal literal "END") layout-guard (shift 1 #f) (reduce 0))
          ((terminal literal "É") shift 2 #f)
          ((terminal token word) shift 3 #f)))
      (def row (index-action-row entries))
      (parameterize ((current-layout-columns (make-layout-columns "end é"))
                     (current-layout-frames '()))
        (check (layout-current-action-row row (make-token 'word "end" 0 3) #t)
               => '((terminal literal "END") shift 1 #f))
        (check (eq? (layout-current-action-row row (make-token 'word "é" 4 6) #t)
                    (cadr entries)) => #t)
        (check (eq? (layout-current-action-row row (make-token 'word "end" 0 3) #f)
                    (caddr entries)) => #t)
        (parameterize ((current-layout-frames '((100 . "outer"))))
          (check (layout-current-action-row row (make-token 'word "end" 0 3) #t)
                 => '((terminal literal "END") reduce 0)))))
    (test-case "nested source success and rejection restore the outer request context"
      (let ((columns (make-layout-columns "outer")) (frames '((100 . "outer"))))
        (parameterize ((current-layout-columns columns) (current-layout-frames frames))
          (for-each
           (lambda (source)
             (let ((expected (equal? source "( |> x END"))
                   (artifact (parse-source closing-boundary-probe-parser source)))
               (check (parse-artifact-success? artifact) => expected)
               (check (parse-artifact-valid? artifact) => #t)
               (check (parse-artifact-roundtrip artifact) => source)
               (check (eq? columns (current-layout-columns)) => #t)
               (check (eq? frames (current-layout-frames)) => #t)))
           '("( |> x END" "( |>" "( |> x END")))))
    (test-case "closing boundaries are validated nullable grammar values"
      (check (grammar-expression? '(layout-end ")" "END")) => #t)
      (check (grammar-expression-nullable? (grammar-expression (layout-end "END"))) => #t)
      (check (grammar-expression? '(layout-end "")) => #f)
      (check (grammar-expression? '(layout-end END)) => #f))
    (test-case "an arbitrary declared word closes a list to the right of its marker"
      (for-each
       (lambda (source)
         (let (artifact (parse-source closing-boundary-probe-parser source))
           (check (parse-artifact-success? artifact) => #t)
           (check (parse-artifact-valid? artifact) => #t)
           (check (parse-artifact-roundtrip artifact) => source)))
       '("( |> x END" "( |> x\n  |> y END")))
    (test-case "undeclared right-hand boundaries retain the column restriction"
      (parameterize ((current-layout-columns (make-layout-columns "( |> x END"))
                     (current-layout-frames '((3 . "|>"))))
        (let (end (make-token 'word "END" 7 10))
          (check (layout-after-end end) => #f)
          (check (layout-after-end end '("END")) => '()))))
    (test-case "closing a nested list pops exactly one reference"
      (parameterize ((current-layout-columns (make-layout-columns "( |> x END"))
                     (current-layout-frames '((3 . "|>") (1 . "outer"))))
        (check (layout-after-end (make-token 'word "END" 7 10) '("END"))
               => '((1 . "outer")))))
    (test-case "an aligned continuation remains in its list"
      (parameterize ((current-layout-columns (make-layout-columns "  |> x\n  |> y"))
                     (current-layout-frames '((3 . "|>"))))
        (check (layout-after-end (make-token 'punctuation "|>" 9 11) '("|>"))
               => #f)))))
