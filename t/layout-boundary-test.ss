#!/usr/bin/env gxi
(import :gerbil-parser/language-test-support)
(import (only-in :gerbil-parser/t/fixtures/fixture-release bind-fixture-grammar-release))
;;; -*- Gerbil -*-
;;; Explicit closing boundaries belong to the declaring language grammar.

(import (only-in :gerbil-parser/t/fixtures/layout-columns wide-frame-roots cyclic-pointer-roots mutual-closure-frame-roots closure-frame-roots
                 cache-copy-flonum-values host-set-f64-bits host-flonum-bits host-flonum-address host-boxing-control host-forwarding-control host-classification-control host-object-kind host-object-subtype host-ordering-litmus host-ordering-layout-control host-chunk-control host-contended-control cache-flonum-values host-ticket-control host-slot-control host-mutex-control host-spin-backend host-max-processors)
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

(defparser-check (native-layout-fallback-origin-case)
  ;; Precedence selects the recursive shift, but indentation may need one or
  ;; both suppressed empty-body reductions in their original emission order.
  (each (additional) ('(() ((precedence left -1 (empty)))))
     (scope ((rules `((root (alias Root (sequence (reference block) (token word))))
                    (block (sequence (layout-start "|>") (reference body) (layout-end)))
                    (body (choice (precedence right 1 (sequence (token word) (reference body)))
                                  (precedence left 0 (empty)) ,@additional)))))
       (each (route) ('(lalr canonical-lr1 partitioned-lr1 follow-partition-lr1))
          (sequential ((spec (compile-lr-spec rules 'root 'selective-glr #f route))
                 (rows (lr-spec-ref spec 'actions))
                 (indexed (list->vector (map index-action-row (vector->list rows))))
                 (select (prepare-layout-action-selector rows indexed #f))
                 (productions (lr-spec-ref spec 'productions)) (guards 0)
                 (saved (map (lambda (row) (map (lambda (cell) (cons (car cell) (cdr cell))) row))
                             (vector->list rows))))
            (each (state) ((iota (vector-length rows)))
               (each (cell) ((vector-ref rows state))
                  (when (and (equal? (car cell) '(terminal token word)) (eq? (cadr cell) 'layout-guard))
                    (act (set! guards (+ guards 1)))
                    (sequential ((primary (caddr cell)) (fallback (cadddr cell))
                           (reductions (if (eq? (car fallback) 'fork) (cdr fallback) (list fallback)))
                           (token (make-token 'word "x" 0 1)))
                      (equal (car primary)  'shift)
                      (equal (length reductions)  (+ 1 (length additional)))
                      (each (reduction) (reductions)
                        (equal (car reduction)  'reduce)
                        (equal (> (cadr reduction) 0)  #t)
                        (equal (production-rhs (list-ref productions (cadr reduction)))  '()))
                      (when (pair? (cdr reductions))
                        (equal (< (cadr (car reductions)) (cadr (cadr reductions)))  #t))
                      (parameters ((current-layout-columns (make-layout-columns "x")))
                        (each (frames expected) ('(() ((100 . "outer")) ()) (list primary fallback primary))
                           (parameters ((current-layout-frames frames))
                             (equal (cdr (layout-current-action-row (vector-ref indexed state) token #f))  expected)
                             (equal (cdr (select state token))  expected))))))))
            (equal (> guards 0)  #t)
            ;; A blocked body shift must close layout and leave x to the root.
            (parameters ((current-layout-columns (make-layout-columns "|>\nx"))
                           (current-layout-frames '()))
              (scope-values (((_root rest)
                            (lr-parse/prepared (lr-prepare spec)
                              (list (make-token 'punctuation "|>" 0 2) (make-token 'word "x" 3 4)))))
                (equal rest  '())))
            (equal (vector->list rows)  saved))))))

(defparser-check (layout-unwind-exception-case)
  (sequential ((rules '((root (alias Root (sequence (reference block) (token word))))
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
    (parameters ((current-layout-columns outer-columns) (current-layout-frames outer-frames))
      (equal (with-catch (lambda (condition) (eq? condition sentinel))
               (lambda ()
                 (parameterize ((current-layout-columns inner-columns) (current-layout-frames '()))
                   (lr-parse/prepared runtime
                     (list (make-token 'punctuation "|>" 0 2) (make-token 'word "x" 3 4))))
                 #f))  #t)
      (equal saw-owned-frame?  #t)
      (equal (eq? (current-layout-columns) outer-columns)  #t)
      (equal (eq? (current-layout-frames) outer-frames)  #t)
      (equal (u32vector->list (current-layout-columns))  outer-column-values)
      ;; Successful empty-list close is distinct from the #f failure result.
      (parameters ((current-layout-columns inner-columns) (current-layout-frames '((1 . "|>"))))
        (equal (layout-after-end #f)  '())
        (equal (layout-after-end (make-token 'punctuation "|>" 0 2) '("|>"))  #f))
      (parameters ((current-layout-frames '()))
        (equal (layout-after-end #f)  #f)))))

(defparser-check (closure-frame-gc-case)
  (each (order) ('(#t #f))
     (sequential ((seed (vector order))
            (readers (closure-frame-roots seed (lambda () (thread-yield!) (##gc))))
            (first (if order (car readers) (cdr readers)))
            (second (if order (cdr readers) (car readers))))
       (act (##gc))
       (equal (car (first))  (if order 'left 'right))
       (equal (car (second))  (if order 'right 'left))
       (equal (eq? (cdr (first)) seed)  #t)
       (equal (eq? (cdr (second)) seed)  #t))))

(defparser-check (wide-frame-gc-case)
(locals ((exercise (parser-check ( worker)
    (each (round) ('(0 1))
       (sequential ((seed (vector worker round))
              (roots (wide-frame-roots seed (lambda () (thread-yield!) (##gc)))))
         (equal (map car roots)  (iota 96))
         (equal (andmap (lambda (root) (eq? (cdr root) seed)) roots)  #t)))
    (result #t))))

  (scope ((left (spawn (parser-check-procedure (parser-check () (verify exercise 'left)))))
        (right (spawn (parser-check-procedure (parser-check () (verify exercise 'right))))))
    (equal (thread-join! left)  #t)
    (equal (thread-join! right)  #t))))

(defparser-check (layout-continuation-gc-case)
(locals ((copy-pairs (lambda ( value)
    (if (pair? value) (cons (copy-pairs (car value)) (copy-pairs (cdr value))) value))))

  ;; Capture inside the actual GLR/layout driver after its marker shift. The
  ;; suspended parameter binding, not an extra root variable, owns inner columns.
  (sequential ((rules '((root (alias Root (sequence (reference block) (token word))))
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
             (let ((select (make-lr-action-selector rows index insensitive? layout?)))
               (parser-check-procedure (parser-check (state token)
                 (when (and (not resume) (pair? (current-layout-frames)))
                   (act (call/cc (lambda (continuation) (set! resume continuation) (escape 'suspended))))
                   (act (set! visits (+ visits 1)))
                   (equal (current-layout-frames)  '((1 . "|>")))
                   (equal (u32vector->list (current-layout-columns))  '(0 1 2 0 1))
                   (act (##gc))
                   (equal (vector->list (lr-spec-ref spec 'actions))  published))
                 (act (select state token)))))))))
    (wind (before (act (set! outer-before (+ outer-before 1)))) (body
       (parameters ((current-layout-columns outer-columns) (current-layout-frames outer-frames))
         (scope ((result
                (call/cc
                 (parser-check-procedure (parser-check (exit)
                   (act (set! escape exit))
                   (wind (before
                      (act (set! inner-before (+ inner-before 1)))
                      (equal (eq? (current-layout-columns) outer-columns)  #t)) (body
                      (parameters ((current-layout-columns (make-layout-columns "|>\nx"))
                                     (current-layout-frames '()))
                        (scope-values (((root rest) (lr-parse/prepared runtime
                                      (list (make-token 'punctuation "|>" 0 2) (make-token 'word "x" 3 4)))))
                          (equal rest  '())
                          (equal (recognition-node-kind root)  'Root)
                          (act 'accepted)))) (after
                      (act (set! inner-after (+ inner-after 1)))
                      (equal (eq? (current-layout-columns) outer-columns)  #t))))))))
           (equal (eq? (current-layout-columns) outer-columns)  #t)
           (equal (eq? (current-layout-frames) outer-frames)  #t)
           (equal (u32vector->list outer-columns)  outer-values)
           (when (eq? result 'accepted) (act (set! completed (+ completed 1))))
           (when (< resumes 2)
             (act (set! resumes (+ resumes 1)))
             (act (##gc))
             (act (resume 'again)))))) (after (act (set! outer-after (+ outer-after 1)))))
    (equal (list visits completed inner-before inner-after outer-before outer-after)  '(2 2 3 3 1 1)))))

(deflanguage-parser-tests layout-boundary-test "Grammar-owned layout closing boundaries"
  (engine)
    (property "native ticket and queue byte offsets match the selected ISA model"
      (bindings)
      (equal (host-ordering-layout-control)  0))
    (scenario "herd inputs preserve ticket allocation release and LDP link decoding"
      (each (kind) ('(payload link))
        (scope ((plain (host-ordering-litmus kind #f)) (acquire (host-ordering-litmus kind #t)))
          (equal (not (not (string-contains plain "LDADDAL")))  #t)
          (equal (not (string-contains plain "LDAR "))  #t)
          (equal (not (not (string-contains acquire "LDAR ")))  #t)
          (equal (not (not (string-contains plain "#16")))  #t)))
      (scope ((link (host-ordering-litmus 'link #f)))
        (equal (not (not (string-contains link "LDP X8,X9,[X2]")))  #t)
        (equal (not (not (string-contains link "LDR X10,[X8] ;\nMOV X5,#8 | SUB X12,X10,#3 ;\nLDADDAL X5,X11,[X1] | STR X12,[X2] ;\nNOP | DMB ISH")))  #t)))
    (property "normal ticket handoff reads payload written after waiter allocation"
      (bindings)
      (equal (host-contended-control)  0))
    (property "contended chunk handoff retains marker link and copied body"
      (bindings)
      (equal (host-chunk-control)  0))
    (property "native raw headers classify every admitted tag and subtype"
      (bindings)
      (equal (host-classification-control)  0))
    (scenario "rooted managed values retain their classification through collections"
      (each (value) ('(#f #t 0 42 #\a))
        (equal (host-object-kind value)  0)
        (equal (host-object-subtype value)  -1))
      (scope ((values (list (cons 'classification 17) (make-vector 3 'payload)
                          (make-string 13 #\x) (make-f64vector 3 1.25)))
            (subtypes '(1 0 19 29)))
        (iterate collect ((cycle 0))
          (when (< cycle 4)
            (scope ((before (f64vector-ref (##process-statistics) 6)))
              (act (##gc))
              (equal (> (f64vector-ref (##process-statistics) 6) before)  #t))
            (each (value subtype) (values subtypes)
              (equal (not (not (memv (host-object-kind value) '(1 2 4))))  #t)
              (equal (host-object-subtype value)  subtype))
            (equal (car values)  '(classification . 17))
            (equal (vector->list (cadr values))  '(payload payload payload))
            (equal (caddr values)  (make-string 13 #\x))
            (equal (f64vector->list (cadddr values))  '(1.25 1.25 1.25))
            (continue collect (+ cycle 1))))))
    (property "actual forwarding tags and losing CAS preserve owned storage"
      (bindings)
      (equal (host-forwarding-control)  0))
    (property "actual heap flonum boxing retains bit patterns and previous allocations"
      (bindings)
      (equal (host-boxing-control)  0))
    (scenario "compiled flonum copies retain both branch values through allocation and collection"
      (each (select-right?) ('(#f #t))
         (sequential ((before (f64vector-ref (##process-statistics) 6))
                (actual
                 (cache-copy-flonum-values
                  -4.5 select-right?
                  (parser-check-procedure (parser-check ()
                    (scope ((roots (let loop ((remaining 32768) (rows '()))
                                   (if (zero? remaining) rows
                                     (loop (- remaining 1) (cons remaining rows))))))
                      (act (##gc))
                      (equal (length roots)  32768))))))
                (after (f64vector-ref (##process-statistics) 6)))
           (equal actual  (if select-right? '(-8.5 -8.5) '(-4.25 -8.5)))
           (equal (> after before)  #t))))
    (scenario "actual moving collections retain exact flonum bits and shared aliases"
      (sequential ((patterns '(#x0 #x8000000000000000 #x3ff0000000000000
                         #xbff0000000000000 #x1 #x7ff0000000000000
                         #xfff0000000000000 #x7ff8000000000042 #x7fefffffffffffff))
             (source (make-f64vector (length patterns))))
        (iterate fill ((rest patterns) (index 0))
          (unless (null? rest)
            (act (host-set-f64-bits source index (car rest)))
            (continue fill (cdr rest) (+ index 1))))
        (sequential ((roots (let loop ((index 0))
                        (if (= index (length patterns)) '()
                          (let (value (f64vector-ref source index))
                            (cons (vector value value) (loop (+ index 1)))))))
               (moved? #f))
          (iterate collect ((cycle 0))
            (when (< cycle 4)
              (scope ((addresses (map (lambda (pair) (host-flonum-address (vector-ref pair 0))) roots))
                    (before (f64vector-ref (##process-statistics) 6)))
                (act (##gc))
                (equal (> (f64vector-ref (##process-statistics) 6) before)  #t)
                (iterate verify ((rest roots) (expected patterns) (old addresses))
                  (unless (null? rest)
                    (sequential ((pair (car rest)) (value (vector-ref pair 0)))
                      (equal (host-flonum-bits value)  (car expected))
                      (equal (##eq? value (vector-ref pair 1))  #t)
                      (when (not (= (host-flonum-address value) (car old))) (act (set! moved? #t))))
                    (continue verify (cdr rest) (cdr expected) (cdr old)))))
              (continue collect (+ cycle 1))))
          (equal moved?  #t))))
    (property "native ticket CAS distinguishes success, busy and unlock"
      (bindings)
      (equal (host-ticket-control)  0))
    (scenario "compiled flonum cache writes retain values across opaque collection"
      (each (seed) ('(0.5 -4.5 16777216.5))
         (equal (cache-flonum-values seed (lambda () (##gc)))
                 (list (+ seed 0.25) (* (+ seed 0.25) 2.0)
                         (- (* (+ seed 0.25) 2.0) seed)))))
    (property "compiled host slot addresses agree after frame pointer adjustment"
      (bindings)
      (equal (host-slot-control)  0))
    (scenario "host mutex imports writes made after OS thread creation"
      (equal (host-mutex-control)  0)
      (scope ((backend (host-spin-backend)) (processors (host-max-processors)))
        (equal (and (memv backend '(0 1 2)) (> processors 0))  #t)
        (when (= backend 0) (equal processors  1))))
    (scenario "compiled closure environments retain future uses across collection"
      (verify closure-frame-gc-case ))
    (scenario "mutually recursive escaping closure environments survive repeated collections"
      (scope ((roots (mutual-closure-frame-roots 37 (lambda () (##gc)))))
        (iterate collect ((cycle 0))
          (when (< cycle 4)
            (act (##gc))
            (equal ((car roots) 8)  '(left . 37))
            (equal ((car roots) 9)  '(right . 37))
            (equal ((cdr roots) 8)  '(right . 37))
            (equal ((cdr roots) 9)  '(left . 37))
            (continue collect (+ cycle 1))))))
    (scenario "pair tail marking and later scanning retain cyclic shared pointer graphs"
      (scope ((root (cyclic-pointer-roots 53 (lambda () (##gc)))))
        (iterate collect ((cycle 0))
          (when (< cycle 4)
            (scope ((before (f64vector-ref (##process-statistics) 6)))
              (act (##gc))
              (equal (> (f64vector-ref (##process-statistics) 6) before)  #t))
            (scope ((slots (car root)) (tail (cdr root)))
              (equal (##eq? (vector-ref slots 0) root)  #t)
              (equal (##eq? (cdr tail) root)  #t)
              (equal (##eq? (vector-ref slots 1) (vector-ref slots 2))  #t)
              (equal (vector-ref slots 1)  '(shared . 53))
              (equal (car tail)  '(cdr . 53)))
            (continue collect (+ cycle 1))))))
    (scenario "generated wide live frames retain every slot across worker collections"
      (verify wide-frame-gc-case ))
    (scenario "actual layout continuations survive GC and repeated dynamic reentry"
      (verify layout-continuation-gc-case ))
    (scenario "layout branch exceptions restore owned outer columns and frames"
      (verify layout-unwind-exception-case ))
    (scenario "native layout fallback retains suppressed reduce origins and immutable rows"
      (verify native-layout-fallback-origin-case ))
    (scenario "layout continuations are rejected before contextual preparation"
      (sequential ((machine closing-boundary-probe-parser)
             (runtime (parser-machine-runtime machine))
             (product (layout-contextual-product machine)))
        (each (events?) ('(#f #t))
           (parameters ((current-lr-event-program-enabled? events?))
             (each (create) ((list (lambda () (lr-initial-checkpoint runtime '()) #f)
                    (lambda () (prepare-contextual-parser machine product) #f)
                    (lambda () (parse-source/contextual machine product "( |> x END") #f)))
                (equal (with-catch (lambda (condition) (error-message condition)) create)
                        "LR checkpoints require a runtime without layout"))))))
    (scenario "layout checkpoint source requests retain complete GLR artifacts"
      (each (source) ('("( |> x END" "( |> x\n  |> y END" "( |>"))
         (scope-values (((artifact tokens modes snapshots)
                       (parse-source/checkpoints closing-boundary-probe-parser source 1)))
           (equal artifact  (parse-source closing-boundary-probe-parser source))
           (equal (parse-artifact-valid? artifact)  #t)
           (equal (parse-artifact-roundtrip artifact)  source)
           (equal tokens  #f)
           (equal modes  #f)
           (equal snapshots  '#()))))
    (scenario "plain prepared runtimes ignore outer layout columns and frames"
      (each (rules) ('(((root (alias Root (token word))))
         ((root (alias Root (choice (reference first) (reference second))))
          (first (precedence dynamic 2 (token word)))
          (second (precedence dynamic 1 (token word))))))
         (scope ((runtime (lr-prepare (compile-lr-spec rules 'root 'selective-glr)))
               (tokens (list (make-token 'word "x" 0 1)))
               (columns (make-layout-columns "x"))
               (frames '((100 . "outer"))))
           (each (events?) ('(#f #t))
              (parameters ((current-lr-event-program-enabled? events?))
                (scope ((selected (lr-runtime-for-current-semantic-backend runtime)))
                  (scope-values (((expected rest) (lr-parse/prepared selected tokens)))
(locals ((artifact (lambda ( root)
                      (make-success-parse-artifact
                       (string-append "sha256:" (make-string 64 #\0)) "x" tokens root #f))))
                    (equal rest  '())

                    (parameters ((current-layout-columns columns) (current-layout-frames frames))
                      (scope-values (((root rest) (lr-parse/prepared selected tokens)))
                        (equal rest  '())
                        (equal (artifact root)  (artifact expected)))
                      ;; The independent materialized GLR entry has its own
                      ;; fast and memoized action lookups, even for plain IR.
                      (scope-values (((root rest receipt) (lr-parse/prepared/receipt selected tokens)))
                        (equal rest  '())
                        (equal (artifact root)  (artifact expected)))
                      (equal (eq? columns (current-layout-columns))  #t)
                      (equal (eq? frames (current-layout-frames))  #t))))))))))
    (scenario "streaming LR callbacks can parse inside an active outer layout request"
      (sequential ((runtime (lr-prepare (compile-lr-spec '((root (alias Root (token word)))) 'root)))
             (input (make-token 'word "x" 0 1))
             (pending (list input)) (shifted '())
             (columns (make-layout-columns "x")) (frames '((100 . "outer"))))
        (parameters ((current-layout-columns columns) (current-layout-frames frames))
          (scope-values (((status payload)
                        (lr-checkpoint-drive
                         (lr-initial-checkpoint runtime '())
                         (lambda (_mode)
                           (and (pair? pending)
                                (let (token (car pending))
                                  (set! pending (cdr pending)) token)))
                         (lambda (token _states _values _actions _shifts)
                           (set! shifted (cons token shifted))))))
            (equal status  'accepted)
            (equal (cadr payload)  '())
            (equal shifted  (list input)))
          (equal (eq? columns (current-layout-columns))  #t)
          (equal (eq? frames (current-layout-frames))  #t))))
    (scenario "layout casefold lookup retains the declared row and guard decision"
(locals ((entries '(((terminal literal "END") layout-guard (shift 1 #f) (reduce 0))
          ((terminal literal "É") shift 2 #f)
          ((terminal token word) shift 3 #f)))
(row (index-action-row entries)))


      (parameters ((current-layout-columns (make-layout-columns "end é"))
                     (current-layout-frames '()))
        (equal (layout-current-action-row row (make-token 'word "end" 0 3) #t)
                '((terminal literal "END") shift 1 #f))
        (equal (eq? (layout-current-action-row row (make-token 'word "é" 4 6) #t)
                    (cadr entries))  #t)
        (equal (eq? (layout-current-action-row row (make-token 'word "end" 0 3) #f)
                    (caddr entries))  #t)
        (parameters ((current-layout-frames '((100 . "outer"))))
          (equal (layout-current-action-row row (make-token 'word "end" 0 3) #t)
                  '((terminal literal "END") reduce 0))))))
    (scenario "nested source success and rejection restore the outer request context"
      (scope ((columns (make-layout-columns "outer")) (frames '((100 . "outer"))))
        (parameters ((current-layout-columns columns) (current-layout-frames frames))
          (each (source) ('("( |> x END" "( |>" "( |> x END"))
             (scope ((expected (equal? source "( |> x END"))
                   (artifact (parse-source closing-boundary-probe-parser source)))
               (equal (parse-artifact-success? artifact)  expected)
               (equal (parse-artifact-valid? artifact)  #t)
               (equal (parse-artifact-roundtrip artifact)  source)
               (equal (eq? columns (current-layout-columns))  #t)
               (equal (eq? frames (current-layout-frames))  #t))))))
    (property "closing boundaries are validated nullable grammar values"
      (bindings)
      (equal (grammar-expression? '(layout-end ")" "END"))  #t)
      (equal (grammar-expression-nullable? (grammar-expression (layout-end "END")))  #t)
      (equal (grammar-expression? '(layout-end ""))  #f)
      (equal (grammar-expression? '(layout-end END))  #f))
    (scenario "an arbitrary declared word closes a list to the right of its marker"
      (each (source) ('("( |> x END" "( |> x\n  |> y END"))
         (scope ((artifact (parse-source closing-boundary-probe-parser source)))
           (equal (parse-artifact-success? artifact)  #t)
           (equal (parse-artifact-valid? artifact)  #t)
           (equal (parse-artifact-roundtrip artifact)  source))))
    (scenario "undeclared right-hand boundaries retain the column restriction"
      (parameters ((current-layout-columns (make-layout-columns "( |> x END"))
                     (current-layout-frames '((3 . "|>"))))
        (scope ((end (make-token 'word "END" 7 10)))
          (equal (layout-after-end end)  #f)
          (equal (layout-after-end end '("END"))  '()))))
    (scenario "closing a nested list pops exactly one reference"
      (parameters ((current-layout-columns (make-layout-columns "( |> x END"))
                     (current-layout-frames '((3 . "|>") (1 . "outer"))))
        (equal (layout-after-end (make-token 'word "END" 7 10) '("END"))
                '((1 . "outer")))))
    (scenario "an aligned continuation remains in its list"
      (parameters ((current-layout-columns (make-layout-columns "  |> x\n  |> y"))
                     (current-layout-frames '((3 . "|>"))))
        (equal (layout-after-end (make-token 'punctuation "|>" 9 11) '("|>"))
                #f))))
