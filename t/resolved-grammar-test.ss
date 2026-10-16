;;; Resolved grammar semantics must be checked before target state construction.
(import :std/test
        (only-in :std/misc/ports read-all-as-string)
        (only-in :std/list/list append-map take)
        (only-in :std/test/base TestCase test-case-add!)
        (only-in :gerbil-parser/src/compiler/lr-compiler compile-lr-spec)
        (only-in :gerbil-parser/src/compiler/lr +lr-eof+ lr-spec-ref current-grammar-source-map compute-nullable compute-first compute-productive compute-completion terminal-symbol?
                 lower-rules production-precedence production-id production-lhs production-rhs production-table production-terminal-catalog
                 sequence-first sequence-nullable? base-symbol nonterminal-symbol? nonterminal-name)
        (only-in :gerbil-parser/src/compiler/funcs compiler-index-set-union compiler-index-set-member?)
        (only-in :gerbil-parser/src/compiler/lr-automaton make-item-layout make-core-symbol-catalog
                 make-core-item core-item-count core-item-production-id core-item-dot)
        (only-in :gerbil-parser/src/compiler/lr-lookahead make-core-suffix-catalog
                 build-states-via-lr0 build-states-via-canonical-lr1)
        (only-in :gerbil-parser/src/compiler/lr-follow-automaton build-states-via-follow-partition-lr1)
        (only-in :gerbil-parser/src/compiler/lr-conflict-candidates initial-backward-follow-partitions)
        (only-in :gerbil-parser/src/compiler/normalize compile-grammar grammar-ir-ref compile-grammar/context normalized-grammar-ir normalized-grammar-source-map)
        (only-in :gerbil-parser/src/modules/parser/objects make-grammar make-grammar-role)
        (only-in :gerbil-parser/src/runtime/lr-parser lr-parse lr-prepare lr-parse/prepared lr-runtime-with-action-selector lr-rejection-condition?)
        (only-in :gerbil-parser/src/runtime/lr-parser/prepare make-lr-action-selector)
        (only-in :gerbil-parser/src/runtime/lr-action-index index-action-rows)
        (only-in :gerbil-parser/src/runtime/scan make-ranked-lexical-scanner-factory)
        (only-in :gerbil-parser/src/runtime/token make-token token-kind)
        (only-in :gerbil-parser/src/runtime/recognition
                 recognition-node-kind recognition-node-start recognition-node-end
                 recognition-node-children recognition-child-field recognition-child-value
                 recognition-value-start recognition-value-end))
(import (only-in :gerbil-parser/t/fixtures/lr1-construction
                 grammar-model-rules native-fact-rules write-native-grammar-facts
                 lr1-not-lalr-rules shared-lookahead-rules precedence-expression-rules
                 mixed-context-rules lr1-context-family-rules
                 state-local-candidate-family-rules mixed-context-family-rules
                 acyclic-mixed-context-family-rules inactive-core-conflict-rules skew-width-rules))
(export resolved-grammar-test)

(def grammar-model-clauses '((start (A #\b)) (A (#\a A)) (A ())))

;;; Independent finite CFG derivation relation. Facts are (lhs,start,end).
;;; This uses neither lowered productions nor nullable/FIRST/LR tables.
(def (derivation-chart-accepts? grammar start word)
  (let ((facts (make-table test: equal?)) (limit (string-length word)))
    (def (rhs? symbols at end)
      (cond ((null? symbols) (= at end))
            ((char? (car symbols))
             (and (< at end) (char=? (car symbols) (string-ref word at))
                  (rhs? (cdr symbols) (+ at 1) end)))
            (else
             (let split ((mid at))
               (and (<= mid end)
                    (or (and (table-ref facts (list (car symbols) at mid) #f)
                             (rhs? (cdr symbols) mid end))
                        (split (+ mid 1))))))))
    (let converge ()
      (let (changed? #f)
        (for-each
         (lambda (rule)
           (for-each (lambda (at)
             (for-each (lambda (end)
               (let (key (list (car rule) at end))
                 (when (and (not (table-ref facts key #f)) (rhs? (cadr rule) at end))
                   (table-set! facts key #t) (set! changed? #t))))
               (iota (+ 1 (- limit at)) at))) (iota (+ limit 1)))) grammar)
        (when changed? (converge))))
    (table-ref facts (list start 0 limit) #f)))

(def (source-dsl-accepts? rules root word)
  ;; Span saturation evaluates the authored tree directly. No lowering helper,
  ;; native FIRST/nullable fact or parser table is consulted.
  (let ((facts (make-table test: equal?)) (limit (string-length word)))
    (def (some-split start end predicate)
      (let loop ((at start))
        (and (<= at end) (or (predicate at) (loop (+ at 1))))))
    (def (sequence? expressions start end)
      (if (null? expressions) (= start end)
        (some-split start end
          (lambda (mid) (and (accepts? (car expressions) start mid)
                            (sequence? (cdr expressions) mid end))))))
    (def (star? expression start end)
      (or (= start end)
          (some-split (+ start 1) end
            (lambda (mid) (and (accepts? expression start mid) (star? expression mid end))))))
    (def (accepts? expression start end)
      (case (car expression)
        ((empty) (= start end))
        ((literal) (string=? (substring word start end) (cadr expression)))
        ((token) (string=? (substring word start end) (symbol->string (cadr expression))))
        ((reference) (table-ref facts (list (cadr expression) start end) #f))
        ((choice) (any (lambda (child) (accepts? child start end)) (cdr expression)))
        ((sequence) (sequence? (cdr expression) start end))
        ((optional) (or (= start end) (accepts? (cadr expression) start end)))
        ((repeat) (star? (cadr expression) start end))
        ((repeat1) (some-split start end (lambda (mid)
                    (and (accepts? (cadr expression) start mid) (star? (cadr expression) mid end)))))
        ((field alias) (accepts? (caddr expression) start end))
        ((precedence) (accepts? (cadddr expression) start end))
        (else (error "unsupported source oracle expression" expression))))
    (let converge ()
      (let (new '())
        (for-each (lambda (row)
          (for-each (lambda (start)
            (for-each (lambda (end)
              (let (key (list (car row) start end))
                (when (and (not (table-ref facts key #f)) (accepts? (cadr row) start end))
                  (set! new (cons key new))))) (iota (+ 1 (- limit start)) start))) (iota (+ limit 1)))) rules)
        (unless (null? new)
          (for-each (lambda (key) (table-set! facts key #t)) new)
          (converge))))
    (table-ref facts (list root 0 limit) #f)))

(def (lowered-chart-rules productions)
  (map (lambda (production)
    (list (production-lhs production)
          (map (lambda (value)
            (let (symbol (base-symbol value))
              (if (nonterminal-symbol? symbol) (nonterminal-name symbol)
                (let (text (if (eq? (cadr symbol) 'token) (symbol->string (caddr symbol)) (caddr symbol)))
                  (unless (= (string-length text) 1) (error "chart fixture needs one-character terminals" symbol))
                  (string-ref text 0))))) (production-rhs production)))) productions))

(def (source-lowering-correspondence-case)
  (for-each
   (lambda (rules)
     (let (clauses (lowered-chart-rules (lower-rules rules 'start)))
       (let words ((word "") (remaining 3))
         (check (derivation-chart-accepts? clauses 'start word) => (source-dsl-accepts? rules 'start word))
         (unless (zero? remaining)
           (words (string-append word "a") (- remaining 1))
           (words (string-append word "b") (- remaining 1))))))
   (append native-fact-rules
     (list '((start (choice (repeat (literal "a")) (literal "b"))))
           '((start (choice (repeat1 (literal "a")) (literal "b"))))
           '((start (choice (precedence left 1 (repeat (literal "a"))) (literal "b"))))
           '((start (repeat (literal "a"))) (start (literal "b")))
           '((start (sequence)))
           '((start (optional (literal "a"))))
           '((start (repeat1 (choice (literal "a") (literal "b")))))
           '((start (precedence left 3 (sequence (field value (alias A (token a)))
                                             (optional (literal "b")))))))))
  ;; A semantic mutation must be visible to the independent authored evaluator.
  (check (source-dsl-accepts? '((start (sequence (literal "a") (literal "b")))) 'start "a") => #f)
  (check (derivation-chart-accepts? '((start (#\a))) 'start "a") => #t))

(def (repetition-owner-case)
  (for-each
   (lambda (repetition)
     (let* ((rules (list (list 'start (list 'alias 'Start (list 'choice repetition '(literal "b"))))))
            (events '())
            (productions (lower-rules rules 'start #f
              (lambda (production owner path expression)
                (set! events (cons (list production owner path expression) events))))))
       ;; The generic emission contract: a self-recursive repetition owner
       ;; cannot have an alternative emitted from another expression occurrence.
       (for-each (lambda (event)
         (let* ((production (car event)) (lhs (production-lhs production)) (rhs (production-rhs production)))
           (when (and (memq (car (cadddr event)) '(repeat repeat1))
                      (pair? rhs) (equal? (base-symbol (car rhs)) (list 'nonterminal lhs)))
             (check (every (lambda (other)
                       (or (not (eq? (production-lhs (car other)) lhs))
                           (equal? (cdr other) (cdr event)))) events) => #t)))) events)
       (for-each
        (lambda (route)
          (let (runtime (lr-prepare (compile-lr-spec rules 'start 'reject #f route)))
            (def (accepts? text)
              (with-catch (lambda (condition) (if (lr-rejection-condition? condition) #f (raise condition)))
                (lambda ()
                  (let-values (((root rest) (lr-parse/prepared runtime
                    (map (lambda (at) (make-token 'word (substring text at (+ at 1)) at (+ at 1)))
                         (iota (string-length text))))))
                    (check (recognition-node-kind root) => 'Start)
                    (null? rest)))))
            (for-each (lambda (count)
              (let (text (make-string count #\a))
                (check (accepts? text) => (source-dsl-accepts? rules 'start text)))) (iota 7))
            (check (accepts? "b") => #t)
            (for-each (lambda (text) (check (accepts? text) => #f)) '("ba" "baa" "ab" "bb"))))
        '(lalr canonical-lr1 follow-partition-lr1))))
   '((repeat (literal "a")) (repeat1 (literal "a"))
     (precedence left 1 (repeat (literal "a")))
     (precedence left 1 (repeat1 (literal "a"))))))

(def (native-fact-certificate-case)
  (check (call-with-output-string write-native-grammar-facts) =>
         (call-with-input-file "formal/ParserEngine/NativeFacts.lean" read-all-as-string)))

(def (grammar-model-pipeline-case)
  ;; The same S -> A b; A -> a A | epsilon fixture as GrammarPipeline.qnt.
  (let* ((grammar grammar-model-clauses)
         (rules grammar-model-rules)
         (runtimes (map (lambda (route)
                          (lr-prepare (compile-lr-spec rules 'start 'reject #f route)))
                        '(canonical-lr1 follow-partition-lr1))))
    (def (native-accepts? runtime tokens)
      (with-catch
       (lambda (condition)
         (if (lr-rejection-condition? condition) #f (raise condition)))
       (lambda ()
         (let-values (((_root rest) (lr-parse/prepared runtime tokens))) (null? rest)))))
    (let words ((word "") (remaining 4))
      (let* ((tokens (map (lambda (at)
                            (make-token (if (char=? (string-ref word at) #\a) 'a 'b)
                                        (substring word at (+ at 1)) at (+ at 1)))
                          (iota (string-length word))))
             (expected (derivation-chart-accepts? grammar 'start word)))
        (for-each (lambda (runtime) (check (native-accepts? runtime tokens) => expected)) runtimes))
      (unless (zero? remaining)
        (words (string-append word "a") (- remaining 1))
        (words (string-append word "b") (- remaining 1))))
    ;; Real ranked scanner -> real prepared parser. Longest beats rank; the
    ;; equal-width/rank winner is the earlier declaration. Modes can remove it.
    (let (factory (make-ranked-lexical-scanner-factory
                   '(("a" a 99 0) ("aa" a 1 1) ("aa" a 1 2) ("b" b 99 3))))
      (for-each
       (lambda (mask)
         (let ((scan (factory '() mask)) (tokens '()) (source "aab"))
           (let loop ((at 0))
             (unless (= at (string-length source))
               (let (selected (scan source at))
                 (check (and selected (> (cadr selected) at)) => #t)
                 (when (= at 0)
                   (check (cadr selected) => (if (vector-ref mask 1) 2 1))
                   (check (cadddr selected) => (if (vector-ref mask 1) 1 0)))
                 (set! tokens (cons (make-token (car selected) (substring source at (cadr selected))
                                               at (cadr selected)) tokens))
                 (loop (cadr selected)))))
           (set! tokens (reverse tokens))
           (check (map token-kind tokens) => (if (vector-ref mask 1) '(a b) '(a a b)))
           (for-each (lambda (runtime) (check (native-accepts? runtime tokens) => #t)) runtimes)))
       (list (vector #t #t #t #t) (vector #t #f #f #t))))))

(def (table-support-counterexample-case)
  ;; Schema-valid indices and reachable accept do not certify CFG semantics.
  ;; Mutate one EOF action: accepting after a without the required trailing b.
  (let* ((spec (compile-lr-spec grammar-model-rules 'start))
         (actions (vector-copy (lr-spec-ref spec 'actions)))
         (shift (cdr (assoc '(terminal token a) (vector-ref actions 0))))
         (target (cadr shift)))
    (vector-set! actions target
      (cons (cons +lr-eof+ '(accept))
            (filter (lambda (entry) (not (equal? (car entry) +lr-eof+))) (vector-ref actions target))))
    (let (runtime (lr-prepare (cons (cons 'actions actions) spec)))
      (let-values (((_root rest) (lr-parse/prepared runtime (list (make-token 'a "a" 0 1)))))
        (check (null? rest) => #t)
        (check (derivation-chart-accepts? grammar-model-clauses 'start "a") => #f)))))

(def (state-sensitive-terminal-case)
  (let* ((rows (vector '(((terminal literal "if") shift 0 #f)
                         ((terminal token word) shift 1 #f))
                      '(((terminal token word) shift 1 #f))))
         (select (make-lr-action-selector rows (index-action-rows rows #f) #f #f))
         (raw (make-token 'word "if" 0 2)))
    (check (car (select 0 raw)) => '(terminal literal "if"))
    (check (car (select 1 raw)) => '(terminal token word))))

;;; List-pair closure oracle; never uses packed +1, cached templates or the
;;; producer's closure implementation. The augmented production cannot predict.
(def (reference-core-closure table seeds)
  (let visit ((pending seeds) (seen '()))
    (if (null? pending) seen
      (let ((item (car pending)) (tail (cdr pending)))
        (if (member item seen) (visit tail seen)
          (let* ((production (vector-ref table (car item))) (dot (cadr item))
                 (rhs (production-rhs production))
                 (next (and (< dot (length rhs)) (base-symbol (list-ref rhs dot))))
                 (predicted
                  (if (and next (nonterminal-symbol? next))
                    (filter-map (lambda (id)
                       (and (not (zero? id))
                            (eq? (production-lhs (vector-ref table id)) (nonterminal-name next))
                            (list id 0))) (iota (vector-length table))) '())))
            (visit (append tail predicted) (cons item seen))))))))

(def (graph-construction-holds? table states transitions layout)
  (def (coordinates state)
    (map (lambda (core) (list (core-item-production-id core layout) (core-item-dot core layout)))
         (vector-ref states state)))
  (let (initial (reference-core-closure table '((0 0))))
    (and (every (lambda (item) (member item initial)) (coordinates 0))
         (every
          (lambda (edge)
            (let* ((source (coordinates (car edge))) (symbol (cadr edge))
                   (target (coordinates (caddr edge)))
                   (moved (filter-map (lambda (item)
                     (let ((rhs (production-rhs (vector-ref table (car item)))) (dot (cadr item)))
                       (and (< dot (length rhs)) (equal? symbol (base-symbol (list-ref rhs dot)))
                            (list (car item) (+ dot 1))))) source))
                   (closure (reference-core-closure table moved)))
              (and (not (member '(0 0) target))
                   (every (lambda (item) (member item closure)) target)))) transitions))))

(def (table-support-holds? spec table states transitions layout lookaheads offsets terminals)
  (def (supported? state key action)
    (case (car action)
      ((fork) (every (lambda (child) (supported? state key child)) (cdr action)))
      ((reject-nonassoc) #t) ; Rejection makes no successful transition claim.
      ((shift)
       (and (member (list state key (cadr action)) transitions)
            (any (lambda (core)
                   (let* ((rhs (production-rhs (vector-ref table (core-item-production-id core layout))))
                          (dot (core-item-dot core layout)))
                     (and (< dot (length rhs)) (equal? key (base-symbol (list-ref rhs dot))))))
                 (vector-ref states state))))
      ((reduce accept)
       (let* ((id (if (eq? (car action) 'accept) 0 (cadr action)))
              (width (length (production-rhs (vector-ref table id))))
              (terminal-id (let find ((at 0))
                             (and (< at (vector-length terminals))
                                  (if (equal? key (vector-ref terminals at)) at (find (+ at 1)))))))
         (and (if (zero? id) (and (eq? (car action) 'accept) (equal? key +lr-eof+) (= width 1)) #t)
              terminal-id
              (let find ((cores (vector-ref states state)) (node (vector-ref offsets state)))
                (and (pair? cores)
                     (or (and (= (core-item-production-id (car cores) layout) id)
                              (= (core-item-dot (car cores) layout) width)
                              (compiler-index-set-member? (vector-ref lookaheads node) terminal-id))
                         (find (cdr cores) (+ node 1))))))))
      (else #f)))
  (and (every (lambda (state)
                (every (lambda (cell) (supported? state (car cell) (cdr cell)))
                       (vector-ref (lr-spec-ref spec 'actions) state)))
              (iota (- (vector-length offsets) 1)))
       (every (lambda (state)
                (every (lambda (cell) (member (list state (list 'nonterminal (car cell)) (cdr cell)) transitions))
                       (vector-ref (lr-spec-ref spec 'gotos) state)))
              (iota (- (vector-length offsets) 1)))))

(def (reference-lookahead-closure table seeds first nullable)
  ;; Tuple items and symbolic terminals, independent of native masks, suffix
  ;; catalogs, closure scratch arrays and dependency queues.
  (def (suffix-first symbols inherited)
    (if (null? symbols) (list inherited)
      (let* ((symbol (base-symbol (car symbols)))
             (head (if (terminal-symbol? symbol) (list symbol)
                       (cdr (assq (nonterminal-name symbol) first)))))
        (append head (if (and (nonterminal-symbol? symbol)
                             (table-ref nullable (nonterminal-name symbol) #f))
                      (suffix-first (cdr symbols) inherited) '())))))
  (let visit ((pending seeds) (seen '()))
    (if (null? pending) seen
      (let* ((item (car pending)) (tail (cdr pending))
             (rhs (production-rhs (vector-ref table (car item)))) (dot (cadr item)))
        (if (member item seen) (visit tail seen)
          (let (symbol (and (< dot (length rhs)) (base-symbol (list-ref rhs dot))))
            (visit
             (append tail
               (if (and symbol (nonterminal-symbol? symbol))
                 (append-map (lambda (id)
                   (if (eq? (production-lhs (vector-ref table id)) (nonterminal-name symbol))
                     (map (lambda (lookahead) (list id 0 lookahead))
                          (suffix-first (list-tail rhs (+ dot 1)) (caddr item))) '()))
                   (iota (vector-length table))) '()))
             (cons item seen))))))))

(def (reference-graph-lookaheads table states transitions layout count first nullable)
  (let (expected (make-vector count '()))
    (vector-set! expected 0 (reference-lookahead-closure table (list (list 0 0 +lr-eof+)) first nullable))
    (let converge ()
      (let (changed? #f)
        (for-each (lambda (edge)
          (let* ((source (car edge)) (symbol (cadr edge)) (target (caddr edge))
                 (moved (filter-map (lambda (item)
                   (let* ((rhs (production-rhs (vector-ref table (car item)))) (dot (cadr item)))
                     (and (< dot (length rhs)) (equal? symbol (base-symbol (list-ref rhs dot)))
                          (list (car item) (+ dot 1) (caddr item))))) (vector-ref expected source)))
                 (closed (reference-lookahead-closure table moved first nullable)))
            (for-each (lambda (item)
              (unless (member item (vector-ref expected target))
                (vector-set! expected target (cons item (vector-ref expected target)))
                (set! changed? #t))) closed))) transitions)
        (when changed? (converge))))
    expected))

(def (lookahead-provenance-case)
  (for-each
   (lambda (entry)
     (let* ((rules (car entry)) (root (cadr entry)) (productions (lower-rules rules root))
            (table (production-table productions)) (nullable (reference-nullable productions))
            (first (reference-first productions nullable)) (first-index (make-table test: eq?)))
       (for-each (lambda (row) (table-set! first-index (car row) (cdr row))) first)
       (for-each
        (lambda (build)
          (let-values (((states count lookaheads offsets transitions terminals layout _symbols _visits _items)
                        (build productions table first-index nullable)))
            (let (expected (reference-graph-lookaheads table states transitions layout count first nullable))
              (for-each (lambda (state)
                (let ((node (vector-ref offsets state)) (actual '()))
                  (for-each (lambda (core)
                    (for-each (lambda (terminal)
                      (when (compiler-index-set-member? (vector-ref lookaheads node) terminal)
                        (set! actual (cons (list (core-item-production-id core layout)
                                                (core-item-dot core layout) (vector-ref terminals terminal)) actual))))
                      (iota (vector-length terminals)))
                    (set! node (+ node 1))) (vector-ref states state))
                  (check (every (lambda (item) (not (not (member item (vector-ref expected state))))) actual) => #t)
                  (check (every (lambda (item) (not (not (member item actual)))) (vector-ref expected state)) => #t)
                  ;; Unsupported lookahead cannot gain provenance from a
                  ;; completed native item or its in-range terminal index.
                  (check (member (list 0 1 '(terminal token impossible)) (vector-ref expected state)) => #f)))
                (iota count)))))
        (list build-states-via-lr0 build-states-via-canonical-lr1 build-states-via-follow-partition-lr1))))
   (list (list grammar-model-rules 'start)
         (list lr1-not-lalr-rules 'source-file)
         (list shared-lookahead-rules 'source-file)
         (list '((start (sequence (reference a) (reference dead)))
                 (a (literal "a")) (dead (reference dead))) 'start))))

(def (graph-provenance-case)
  (for-each
   (lambda (entry)
     (let* ((rules (car entry)) (root (cadr entry)) (productions (lower-rules rules root))
            (table (production-table productions)))
       (let-values (((_nullable nullable _productive _productive-index) (compute-completion productions)))
         (let-values (((_first first) (compute-first productions nullable)))
           (for-each
            (lambda (build)
              (let-values (((states _count lookaheads offsets transitions terminals layout _symbols _visits _items)
                            (build productions table first nullable)))
                (check (not (not (graph-construction-holds? table states transitions layout))) => #t)
                (let* ((route (cond ((eq? build build-states-via-lr0) 'lalr)
                                    ((eq? build build-states-via-canonical-lr1) 'canonical-lr1)
                                    (else 'follow-partition-lr1)))
                       (spec (compile-lr-spec rules root 'probe #f route)))
                  (check (not (not (table-support-holds? spec table states transitions layout lookaheads offsets terminals))) => #t)
                  (let (actions (vector-copy (lr-spec-ref spec 'actions)))
                    (vector-set! actions 0 (cons (cons +lr-eof+ '(reduce 0)) (vector-ref actions 0)))
                    (check (not (table-support-holds? (cons (cons 'actions actions) spec)
                                  table states transitions layout lookaheads offsets terminals)) => #t)))
                ;; Inventing a positive-dot RHS symbol must fail the independent
                ;; closure relation even when the graph target remains in range.
                (when (pair? transitions)
                  (let* ((edge (car transitions))
                         (mutated (cons (list (car edge) '(terminal token impossible) (caddr edge))
                                        (cdr transitions))))
                    (check (not (graph-construction-holds? table states mutated layout)) => #t)))))
            (list build-states-via-lr0 build-states-via-canonical-lr1 build-states-via-follow-partition-lr1))))))
   (list (list grammar-model-rules 'start)
         (list lr1-not-lalr-rules 'source-file)
         (list shared-lookahead-rules 'source-file)
         (list '((start (sequence (literal "x") (reference $accept)))
                 ($accept (literal "a"))) 'start))))

;; Mirrors the model's physical boundary: the callback observes the same
;; production in ordinal order and identifies a real authored occurrence.
(def (emission-origin-alignment-case)
  (def (source-at expression path)
    (if (null? path) expression
      (let* ((kind (car expression))
             (children (case kind
                         ((choice sequence) (cdr expression))
                         ((optional repeat repeat1) (list (cadr expression)))
                         ((field alias) (list (caddr expression)))
                         ((precedence) (list (cadddr expression)))
                         (else '())))
             (index (car path)))
        (and (< index (length children)) (source-at (list-ref children index) (cdr path))))))
  (let* ((reserved (map (lambda (index) (string->symbol (string-append "$start." (number->string index))))
                        (iota 128)))
         ;; Half are declared; half are deliberately undefined references. Both
         ;; groups must be reserved before any helper or augmentation is claimed.
         (rules (cons
                 (list 'start
                   (list 'alias 'Start
                     (list 'choice
                       '(sequence (optional (literal "a"))
                                  (repeat1 (sequence (field word (token word)) (literal "b"))))
                       (cons 'sequence (map (lambda (name) (list 'reference name)) (cons '$accept reserved))))))
                 (map (lambda (name) (list name '(empty))) (take reserved 64))))
         (observed '())
         (productions
           (lower-rules rules 'start #f
             (lambda (production owner path expression)
               (check (any (lambda (row)
                             (and (eq? owner (car row)) (equal? expression (source-at (cadr row) path)))) rules) => #t)
               (set! observed (cons production observed))))))
    (check (lower-rules rules 'start) => productions)
    (check (map production-id productions) => (iota (length productions)))
    (check (every (lambda (pair) (eq? (car pair) (cadr pair)))
                    (map list (reverse observed) (cdr productions))) => #t)
    (for-each
     (lambda (production)
       (let (name (production-lhs production))
         (unless (assq name rules)
           (check (memq name (cons '$accept reserved)) => #f)))) productions)
    ;; The source occurrence comparison must reject a stale/wrong child path.
    (check (source-at '(sequence (literal "a") (literal "b")) '(1)) => '(literal "b"))
    (check (equal? '(literal "a") (source-at '(sequence (literal "a") (literal "b")) '(1))) => #f)))

(def (lowering-proof-publication-case)
  (let* ((rules '((start (choice (repeat (literal "a"))
                                (sequence (optional (token word)) (reference missing))))))
         (bindings '()) (published '()) (events '())
         (productions
          (lower-rules rules 'start #f #f
            (lambda (kind value proof)
              (set! events (cons kind events))
              (case kind
                ((binding)
                 (check (memq value bindings) => #f)
                 (set! bindings (cons value bindings)))
                ((production)
                 (check (pair? proof) => #t)
                 ;; A helper's physical identity must be published before any
                 ;; rule that mentions it. Authored and missing names are separate.
                 (for-each (lambda (operand)
                   (let (symbol (base-symbol operand))
                     (when (nonterminal-symbol? symbol)
                       (let (name (nonterminal-name symbol))
                         (unless (memq name '(start missing))
                           (check (and (memq name bindings) #t) => #t))))))
                   (production-rhs value))
                 (set! published (cons value published))))))))
    (check productions => (lower-rules rules 'start))
    (check (map production-id (reverse published)) => (cdr (iota (length productions))))
    (check (every eq? (reverse published) (cdr productions)) => #t)
    (check (car (reverse events)) => 'binding))
  ;; A failed proof sink propagates its original condition; it cannot publish
  ;; a successful truncated replay receipt.
  (let (failure (list 'proof-sink-failure))
    (check (with-catch (lambda (condition) (eq? condition failure))
             (lambda ()
               (lower-rules '((start (empty))) 'start #f #f
                 (lambda (_kind _value _proof) (raise failure)))
               #f)) => #t)))

(def (internal-name-isolation-case)
  ;; Both augmented and lowered helper names must avoid authored declarations.
  (for-each
   (lambda (entry)
     (let* ((rules (car entry)) (root (cadr entry)) (text (caddr entry))
            (productions (lower-rules rules root)))
       (check (lower-rules rules root) => productions)
       (check (member (production-lhs (car productions)) (map car rules)) => #f)
       (for-each
        (lambda (route)
          (let (runtime (lr-prepare (compile-lr-spec rules root 'reject #f route)))
            (let-values (((_root rest) (lr-parse/prepared runtime
              (map (lambda (at) (make-token 'punctuation (substring text at (+ at 1)) at (+ at 1)))
                   (iota (string-length text))))))
              (check (null? rest) => #t))
            (check (with-catch
                    (lambda (condition)
                      (if (lr-rejection-condition? condition) #t (raise condition)))
                    (lambda () (lr-parse/prepared runtime '()) #f)) => #t)))
        '(canonical-lr1 follow-partition-lr1))))
   (list
    (list '((start (alias Start (sequence (literal "x") (reference $accept))))
            ($accept (literal "a"))) 'start "xa")
    (list '(($accept (alias Start (sequence (literal "x") (reference $accept.0))))
            ($accept.0 (literal "a"))) '$accept "xa")
    (list '((start (alias Start (sequence (optional (literal "x")) (reference $start.0))))
            ($start.0 (literal "a"))) 'start "xa"))))

(def (raw-request-terminal-case)
  (let* ((rules '((start (choice (alias Keyword (sequence (literal "a") (literal "if")))
                                (alias Identifier (sequence (literal "b") (token word)))))))
         (base (lr-prepare (compile-lr-spec rules 'start)))
         (raw (make-token 'word "if" 1 3)) (selected '())
         (runtime (lr-runtime-with-action-selector base
          (lambda (rows index insensitive? layout?)
            (let (select (make-lr-action-selector rows index insensitive? layout?))
              (lambda (state input)
                (let (entry (select state input))
                  (when (and (eq? input raw) entry (eq? (cadr entry) 'shift))
                    (set! selected (cons (car entry) selected)))
                  entry)))))))
    (for-each
     (lambda (prefix kind)
       (let-values (((root rest) (lr-parse/prepared runtime
                         (list (make-token 'punctuation prefix 0 1) raw))))
         (check (null? rest) => #t)
         (check (recognition-node-kind root) => kind))) '("a" "b") '(Keyword Identifier))
    (check (reverse selected) => '((terminal literal "if") (terminal token word)))))

(def (first-is-not-productivity-case)
  (let (productions (lower-rules
                     '((start (sequence (literal "a") (reference dead)))
                       (dead (reference dead))) 'start))
    (let-values (((_nullable nullable _productive productive) (compute-completion productions)))
      (let-values (((_rows first) (compute-first productions nullable)))
        (check (table-ref nullable 'start #f) => #f)
        (check (table-ref productive 'start #f) => #f)
        (check (table-ref first 'start '()) => '((terminal literal "a")))))))

(def (duplicate-production-identity-case)
  ;; Equal base CFG rows retain distinct RHS aliases, precedence and IDs.
  (let* ((rules '((start (choice
                         (precedence left 1 (alias Lower (literal "a")))
                         (precedence left 2 (alias Higher (literal "a")))))))
         (productions (lower-rules rules 'start))
         (authored (cdr productions))
         (table (production-table productions)))
    (check (length authored) => 2)
    (check (map production-id authored) => '(1 2))
    (check (production-lhs (car authored)) => (production-lhs (cadr authored)))
    (check (map base-symbol (production-rhs (car authored)))
      => (map base-symbol (production-rhs (cadr authored))))
    (check (equal? (production-rhs (car authored)) (production-rhs (cadr authored))) => #f)
    (check (equal? (production-precedence (car authored)) (production-precedence (cadr authored))) => #f)
    (let-values (((terminals _index) (production-terminal-catalog productions)))
      (let (layout (make-item-layout table terminals))
        (for-each (lambda (dot)
          (let ((left (make-core-item 1 dot layout)) (right (make-core-item 2 dot layout)))
            (check (= left right) => #f)
            (check (core-item-production-id left layout) => 1)
            (check (core-item-production-id right layout) => 2))) '(0 1))))
    (for-each (lambda (route)
      (let* ((spec (compile-lr-spec rules 'start 'reject #f route))
             (runtime (lr-prepare spec)))
        ;; Repeated requests use the published table after all scratch cleanup.
        (for-each (lambda (iteration)
          (let-values (((root rest) (lr-parse/prepared runtime (list (make-token 'punctuation "a" 0 1)))))
            (check (recognition-node-kind root) => 'Higher)
            (check rest => '()))) '(0 1))))
      '(lalr canonical-lr1 follow-partition-lr1))))

;;; Independent coordinates include epsilon, unary and uneven-width rules.
;;; Expected coordinates are not decoded from the packed value.
(def (dense-item-coordinates-case)
  (let* ((productions (lower-rules '((start (sequence (literal "a") (literal "b") (literal "c")))
                                   (empty (sequence)) (short (literal "z"))) 'start))
         (table (production-table productions)))
    (let-values (((terminals _index) (production-terminal-catalog productions)))
      (let* ((layout (make-item-layout table terminals))
             (symbols (make-core-symbol-catalog table layout))
             (coordinates '((0 0) (0 1) (1 0) (1 1) (1 2) (1 3) (2 0) (3 0) (3 1))))
        (check (core-item-count layout) => 9)
        (check (vector->list symbols)
          => '((nonterminal start) #f (terminal literal "a") (terminal literal "b")
               (terminal literal "c") #f #f (terminal literal "z") #f))
        (for-each
         (lambda (coordinate item)
           (check (make-core-item (car coordinate) (cadr coordinate) layout) => item)
           (check (core-item-production-id item layout) => (car coordinate))
           (check (core-item-dot item layout) => (cadr coordinate)))
         coordinates (iota 9))))))

(def (dense-layout-refinement-case)
  (for-each
   (lambda (widths)
     (let* ((rules (map (lambda (width id)
                         (list (string->symbol (string-append "rule-" (number->string id)))
                               (cons 'sequence (make-list width '(token word)))))
                       widths (iota (length widths))))
            (productions (lower-rules rules 'rule-0))
            (table (production-table productions))
            ;; Ordered flattening is intentional: the SDK append-map utility
            ;; reverses each returned segment and is unsuitable for coordinates.
            (coordinates (apply append (map (lambda (production)
                           (map (lambda (dot) (list (production-id production) dot))
                                (iota (+ 1 (length (production-rhs production)))))) productions))))
       (let-values (((terminals _index) (production-terminal-catalog productions)))
         (let* ((layout (make-item-layout table terminals))
                (symbols (make-core-symbol-catalog table layout)))
           (check (core-item-count layout) => (length coordinates))
           (for-each
            (lambda (coordinate code)
              (let* ((id (car coordinate)) (dot (cadr coordinate))
                     (rhs (production-rhs (vector-ref table id))))
                (check (make-core-item id dot layout) => code)
                (check (list (core-item-production-id code layout) (core-item-dot code layout)) => coordinate)
                (check (vector-ref symbols code)
                  => (and (< dot (length rhs)) (base-symbol (list-ref rhs dot))))
                (when (< dot (length rhs))
                  ;; The logical goto theorem uses this exact after-dot guard.
                  (check (list (core-item-production-id (+ code 1) layout)
                               (core-item-dot (+ code 1) layout)) => (list id (+ dot 1))))))
            coordinates (iota (length coordinates)))))))
   '((0) (0 0 0) (1 0 7 2 0) (64 1 0 3) (1 128 1 0))))

(def (skew-item-space-case)
  (let* ((productions (lower-rules (skew-width-rules 128 64) 'source-file))
         (table (production-table productions)))
    (let-values (((terminals _index) (production-terminal-catalog productions)))
      (let* ((layout (make-item-layout table terminals))
             (symbols (make-core-symbol-catalog table layout)))
        ;; Two unary entry rules, 128 unary composition members and one wide
        ;; rule. The old padded domain had 8,515 positions for this grammar.
        (check (vector-length symbols) => 325)
        (check (make-core-item 130 0 layout) => 260)
        (check (make-core-item 130 64 layout) => 324)
        (check (vector-ref symbols 324) => #f)))
    (check-suffix-catalog productions)
    (let-values (((_names nullable) (compute-nullable productions)))
      (let-values (((_rows first) (compute-first productions nullable)))
        (let-values (((partitions _candidates _terminals)
                      (initial-backward-follow-partitions productions table first nullable)))
          (check (vector-length partitions) => 325))))))

(def (admission-result rules (construction 'lalr) (policy 'reject))
  (with-catch
   (lambda (condition) (list (error-message condition) (error-irritants condition)))
   (lambda () (compile-lr-spec rules 'start policy #f construction) 'accepted)))

(def (nullable-repeat-result owner operand (kind 'repeat))
  (list "resolved repetition operand accepts empty input" (list owner kind operand)))

(def (parse-literals rules text (return-root? #f))
  (let (spec (compile-lr-spec
              (map (lambda (row)
                     (if (eq? (car row) 'start)
                       (list 'start (list 'alias 'Start (cadr row))) row)) rules)
              'start))
    (with-catch
     (lambda (condition)
       (if (lr-rejection-condition? condition) 'rejected (raise condition)))
     (lambda ()
       (let-values (((root rest)
                     (lr-parse spec
                      (map (lambda (index)
                             (make-token 'punctuation (string (string-ref text index))
                                         index (+ index 1)))
                           (iota (string-length text))))))
         (unless (null? rest) (error "unconsumed test input" rest))
         (if return-root? root 'accepted))))))

(def (rule-role name rules)
  (make-grammar-role name '() '() '() rules '() '() '() '() '()))

;; Independent repeated-scan oracle retained only in this semantic control.
(def (reference-nullable productions)
  (let (index (make-table test: eq?))
    (let converge ()
      (let (changed? #f)
        (for-each
         (lambda (production)
           (when (and (not (table-ref index (production-lhs production) #f))
                      (every (lambda (value)
                               (let (symbol (base-symbol value))
                                 (and (nonterminal-symbol? symbol)
                                      (table-ref index (nonterminal-name symbol) #f))))
                             (production-rhs production)))
             (table-set! index (production-lhs production) #t)
             (set! changed? #t))) productions)
        (when changed? (converge))))
    index))

(def (nullable-production name rhs) (list 0 name rhs #f #f))

(def (check-nullable-reference productions)
  (let (reference (reference-nullable productions))
    (let-values (((names index) (compute-nullable productions)))
      (check names => (filter (lambda (name) (table-ref reference name #f)) '(a b c)))
      (for-each (lambda (name)
                  (check (table-ref index name #f) => (table-ref reference name #f)))
                '(a b c missing)))))

(def (reference-first productions nullable)
  (let ((index (make-table test: eq?)) (names '()) (terminals '((terminal eof))))
    (def (union left right)
      (foldl (lambda (value found) (if (member value found) found (append found (list value)))) left right))
    (for-each
     (lambda (production)
       (set! names (if (memq (production-lhs production) names) names
                      (append names (list (production-lhs production)))))
       (for-each (lambda (value)
                   (let (symbol (base-symbol value))
                     (when (terminal-symbol? symbol)
                       (set! terminals (union terminals (list symbol))))))
                 (production-rhs production))) productions)
    (def (prefix rhs)
      (if (null? rhs) '()
          (let (symbol (base-symbol (car rhs)))
            (if (terminal-symbol? symbol) (list symbol)
                (let* ((name (nonterminal-name symbol)) (found (table-ref index name '())))
                  (if (table-ref nullable name #f) (union found (prefix (cdr rhs))) found))))))
    (let converge ()
      (let (changed? #f)
        (for-each
         (lambda (production)
           (let* ((name (production-lhs production)) (before (table-ref index name '()))
                  (after (union before (prefix (production-rhs production)))))
             (unless (equal? before after)
               (table-set! index name after) (set! changed? #t)))) productions)
        (when changed? (converge))))
    (map (lambda (name)
           (cons name (filter (lambda (terminal) (member terminal (table-ref index name '()))) terminals))) names)))

(def (check-first-reference productions)
  (let* ((nullable (reference-nullable productions))
         (expected (reference-first productions nullable)))
    (let-values (((rows index) (compute-first productions nullable)))
      (check rows => expected)
      (for-each (lambda (row) (check (table-ref index (car row)) => (cdr row))) expected))))

(def (reference-productive productions)
  (let (index (make-table test: eq?))
    (let converge ()
      (let (changed? #f)
        (for-each
         (lambda (production)
           (when (and (not (table-ref index (production-lhs production) #f))
                      (every (lambda (value)
                               (let (symbol (base-symbol value))
                                 (or (terminal-symbol? symbol)
                                     (table-ref index (nonterminal-name symbol) #f))))
                             (production-rhs production)))
             (table-set! index (production-lhs production) #t)
             (set! changed? #t))) productions)
        (when changed? (converge))))
    index))

(def (check-productive-reference productions)
  (let ((expected (reference-productive productions))
        (nullable (reference-nullable productions))
        (names '()))
    (for-each (lambda (production)
                (unless (memq (production-lhs production) names)
                  (set! names (append names (list (production-lhs production)))))) productions)
    (let-values (((nullable-found nullable-index found index) (compute-completion productions)))
      (check nullable-found => (filter (lambda (name) (table-ref nullable name #f)) names))
      (for-each (lambda (name) (check (table-ref nullable-index name #f) => (table-ref nullable name #f))) (cons 'missing names))
      (check found => (filter (lambda (name) (table-ref expected name #f)) names))
      (for-each (lambda (name)
                  (check (table-ref index name #f) => (table-ref expected name #f))
                  (when (table-ref nullable name #f)
                    (check (table-ref index name #f) => #t))) (cons 'missing names)))))

;; Independent forward suffix scans check the entire catalog, including
;; unused/padded slots. The optimized builder folds backwards and uses masks.
(def (check-suffix-catalog productions)
  (let (table (production-table productions))
    (let-values (((terminals terminal-index) (production-terminal-catalog productions))
                 ((names nullable) (compute-nullable productions)))
      (let-values (((first-rows first) (compute-first productions nullable)))
       (let* ((layout (make-item-layout table terminals))
             (symbols (make-core-symbol-catalog table layout))
             (expected-masks (make-vector (vector-length symbols) 0))
             (expected-nullable (make-vector (vector-length symbols) #f)))
        (let production-loop ((id 0))
          (when (< id (vector-length table))
            (let suffix-loop ((rest (production-rhs (vector-ref table id))) (dot 0))
              (let (item (make-core-item id dot layout))
                (vector-set! expected-masks item
                  (foldl (lambda (terminal mask)
                           (compiler-index-set-union mask
                             (arithmetic-shift 1 (table-ref terminal-index terminal))))
                         0 (sequence-first rest first nullable)))
                (vector-set! expected-nullable item (sequence-nullable? rest nullable)))
              (unless (null? rest)
                (suffix-loop (cdr rest) (+ dot 1))))
            (production-loop (+ id 1))))
        (let-values (((masks nullable-tails)
                      (make-core-suffix-catalog table layout symbols first nullable terminal-index)))
          (check masks => expected-masks)
          (check nullable-tails => expected-nullable)))))))

;;; Named case procedures retain independent assertions and separate native C hosts.

(def (suffix-facts-case)
  (for-each (lambda (rules) (check-suffix-catalog (lower-rules rules 'source-file)))
    [shared-lookahead-rules lr1-not-lalr-rules precedence-expression-rules
     inactive-core-conflict-rules (mixed-context-family-rules 8)])
  ;; Empty FIRST is not nullable; a missing reference blocks a suffix.
  ;; Marked symbols use the same underlying terminal/nonterminal facts.
  (let* ((a '(nonterminal a)) (b '(nonterminal b))
         (dead '(nonterminal dead)) (missing '(nonterminal missing))
         (x '(terminal literal "x"))
         (marked-a (list 'marked a '((alias A))))
         (marked-x (list 'marked x '((alias X))))
         (operands [a b dead missing x marked-a marked-x]))
    (for-each
      (lambda (tail)
        (check-suffix-catalog
          (list (list 0 'root (append operands (list tail)) #f #f)
                (list 1 'a '() #f #f)
                (list 2 'a (list x) #f #f)
                (list 3 'b (list x) #f #f)
                (list 4 'dead (list dead) #f #f)))) operands)
    (check-suffix-catalog
      (list (list 0 'root (make-list 256 marked-a) #f #f)
            (list 1 'a '() #f #f) (list 2 'a (list x) #f #f)))))

(def (completion-publications-case)
  (let ((prefix (list (nullable-production 'c '((nonterminal a) (nonterminal b)))
                      (nullable-production 'a '((nonterminal d)))
                      (nullable-production 'b '((nonterminal a) (nonterminal a)))
                      (nullable-production 'd '((nonterminal b)))
                      (nullable-production 'blocked '((terminal literal "x") (nonterminal missing)))))
        (seeds (list (nullable-production 'a '((marked (terminal literal "x") (field value))))
                     (nullable-production 'b '()))))
    (check-productive-reference (append prefix seeds))
    (check-productive-reference (append prefix (reverse seeds))))
  (check-productive-reference '())
  (check-productive-reference
   (list (nullable-production 'a '((nonterminal missing) (nonterminal missing)))
         (nullable-production 'b '((terminal literal "x") (nonterminal a)))
         (nullable-production 'c '((marked (nonterminal b) (field value)))))))

(def (cyclic-productivity-case)
  (let (operands '(() ((terminal literal "x")) ((nonterminal a))
                     ((nonterminal b)) ((nonterminal c))
                     ((nonterminal a) (nonterminal a))
                     ((terminal literal "x") (nonterminal b))
                     ((marked (nonterminal b) (field value)))
                     ((nonterminal missing))))
    (for-each (lambda (a)
                (for-each (lambda (b)
                            (for-each (lambda (c)
                                        (check-productive-reference
                                         (list (nullable-production 'a a)
                                               (nullable-production 'b b)
                                               (nullable-production 'c c)))) operands)) operands)) operands)))

(def (family-productivity-case)
  (for-each (lambda (rules) (check-productive-reference (lower-rules rules 'source-file)))
            (list lr1-not-lalr-rules shared-lookahead-rules precedence-expression-rules
                  mixed-context-rules (lr1-context-family-rules 8)
                  (state-local-candidate-family-rules 4) (mixed-context-family-rules 4)
                  (acyclic-mixed-context-family-rules 4)))
  (check-productive-reference
   (list (nullable-production 'a '((nonterminal b) (nonterminal b)))
         (nullable-production 'b '((nonterminal c)))
         (nullable-production 'c '((nonterminal a)))
         (nullable-production 'c '((marked (terminal literal "x") (field value))))
         (nullable-production 'a '((nonterminal missing)))
         (nullable-production 'unused '((nonterminal unused))))))

(def (reverse-productivity-case)
  (let* ((names (map (lambda (n) (string->symbol (string-append "productive-chain-" (number->string n)))) (iota 4096)))
         (productions (map (lambda (name next)
                             (nullable-production name (if next (list (list 'nonterminal next))
                                                          '((terminal literal "x")))))
                           names (append (cdr names) '(#f)))))
    (let-values (((found index) (compute-productive productions)))
      (check found => names)
      (check (every (lambda (name) (table-ref index name #f)) names) => #t))))

(def (dead-start-case)
  (for-each
   (lambda (rules)
     (for-each
      (lambda (route)
        (for-each
         (lambda (policy)
           (let (result (admission-result rules route policy))
             (check (car result) => "LR start rule has no finite terminal derivation")
             (check (car (cadr result)) => 'start)))
         '(reject selective-glr probe)))
      '(lalr canonical-lr1 partitioned-lr1 follow-partition-lr1 lalr-then-follow)))
   '(((start (reference start)))
     ((start (sequence (literal "a") (reference tail))) (tail (reference tail))))))

(def (finite-exits-case)
  (check (parse-literals '((start (empty)) (dead (reference dead))) "") => 'accepted)
  (check (parse-literals '((start (choice (sequence (reference start) (literal "a"))
                                        (literal "b")))
                          (dead (reference dead))) "baa") => 'accepted)
  (check (parse-literals '((start (choice (literal "b") (reference dead)))
                          (dead (sequence (literal "a") (reference dead)))) "b") => 'accepted)
  (check (parse-literals '((start (choice (literal "b") (reference dead)))
                          (dead (sequence (literal "a") (reference dead)))) "a") => 'rejected))

(def (blocked-productivity-case)
  (let* ((rules '((start (alias Start (choice (reference a)
                                            (sequence (literal "x") (reference b)))))
                  (a (reference start)) (b (reference b))))
         (result (parameterize ((current-grammar-source-map '((rule))))
                   (admission-result rules)))
         (details (cadr (cadr result)))
         (blockers (cdr (assq 'unproductiveRuleBlockers details)))
         (start (car blockers))
         (references (cdr (assq 'blockedReferences start))))
    (check (map (lambda (row) (cdr (assq 'rule (cdr (assq 'ruleOrigin row))))) blockers)
           => '(start b a))
    (check (map (lambda (row) (cdr (assq 'rule row))) references) => '(a b))
    (check (map (lambda (row) (cdr (assq 'expressionPath (cdr (assq 'referenceOrigin row))))) references)
           => '((0 0) (0 1 1)))
    (check (length blockers) => 3)))

(def (authored-productivity-case)
  (let* ((rules '((start (alias Start (repeat1 (field value (reference dead)))))
                  (dead (reference dead))))
         (result (admission-result rules))
         (details (cadr (cadr result)))
         (blockers (cdr (assq 'unproductiveRuleBlockers details)))
         (reference (car (cdr (assq 'blockedReferences (car blockers))))))
    (check (map (lambda (row) (cdr (assq 'rule (cdr (assq 'ruleOrigin row))))) blockers)
           => '(start dead))
    (check (cdr (assq 'expressionPath (cdr (assq 'referenceOrigin reference)))) => '(0 0 0)))
  (for-each (lambda (kind)
              (check (parse-literals (list (list 'start (list kind '(reference dead)))
                                         '(dead (sequence (literal "a") (reference dead)))) "") => 'accepted))
            '(optional repeat))
  ;; Finite epsilon admission does not discharge independent LR conflicts.
  (check (car (admission-result '((start (optional (reference dead)))
                                 (dead (reference dead)))))
         => "unresolved reduce/reduce conflict"))

(def (poo-productivity-case)
  (let* ((base (make-grammar 'base '()
                 (list (rule-role 'base-rules '((start (reference item)) (item (literal "a")))))))
         (dead (make-grammar 'dead (list base) '()
                 (list (cons 'override (rule-role 'dead-item '((item (reference item))))))))
         (normalized (compile-grammar/context dead))
         (result (parameterize ((current-grammar-source-map (normalized-grammar-source-map normalized "unknown-source")))
                   (admission-result (grammar-ir-ref (normalized-grammar-ir normalized) 'rules))))
         (blockers (cdr (assq 'unproductiveRuleBlockers (cadr (cadr result)))))
         (item-source (cdr (assq 'source (cdr (assq 'ruleOrigin (cadr blockers)))))))
    (check (cdr (assq 'componentOwner item-source)) => 'dead-item)
    (check (cdr (assq 'compositionOperation item-source)) => 'override)
    (for-each
     (lambda (exit)
       (let (restored (make-grammar 'restored (list dead) '()
                       (list (cons 'override (rule-role 'restored-item (list (list 'item exit)))))))
         (check (admission-result (grammar-ir-ref (compile-grammar restored) 'rules)) => 'accepted)))
     '((literal "b") (empty)))
    (check (current-grammar-source-map) => '())))

(def (cyclic-first-case)
  (let (operands '(() ((terminal literal "x")) ((terminal literal "y"))
                     ((nonterminal a)) ((nonterminal b)) ((nonterminal c))
                     ((nonterminal a) (terminal literal "x"))
                     ((marked (nonterminal b) (field value)))))
    (for-each (lambda (a)
                (for-each (lambda (b)
                            (for-each (lambda (c)
                                        (check-first-reference
                                         (list (nullable-production 'a a)
                                               (nullable-production 'b b)
                                               (nullable-production 'c c)))) operands)) operands)) operands)))

(def (family-first-case)
  (for-each (lambda (rules) (check-first-reference (lower-rules rules 'source-file)))
            (list lr1-not-lalr-rules shared-lookahead-rules precedence-expression-rules
                  mixed-context-rules (lr1-context-family-rules 8)
                  (state-local-candidate-family-rules 4) (mixed-context-family-rules 4)
                  (acyclic-mixed-context-family-rules 4))))

(def (first-prefix-case)
  (check-first-reference
   (list (nullable-production 'c '((marked (nonterminal a) (field value))))
         (nullable-production 'a '((nonterminal b) (terminal literal "z")))
         (nullable-production 'b '())
         (nullable-production 'a '((terminal literal "x")))
         (nullable-production 'a '((nonterminal b) (nonterminal b) (terminal literal "z")))
         (nullable-production 'c '((nonterminal c) (terminal eof)))))
  (check-first-reference
   (list (nullable-production 'a '((nonterminal b) (terminal literal "blocked")))
         (nullable-production 'b '((marked (terminal literal "y") (field value))))
         (nullable-production 'c '((nonterminal missing) (terminal literal "blocked"))))))

(def (incomplete-prefix-case)
  (let (productions
        (list (nullable-production 'a '((terminal literal "x") (nonterminal b)))
              (nullable-production 'b '((nonterminal b)))
              (nullable-production 'c '((nonterminal a)))))
    (check-first-reference productions)
    (let-values (((rows index) (compute-first productions (reference-nullable productions))))
      (check (table-ref index 'a) => '((terminal literal "x")))
      (check (table-ref index 'b) => '())
      (check (table-ref index 'c) => '((terminal literal "x"))))))

(def (bignum-first-case)
  (let* ((terminals (map (lambda (n) (list 'terminal 'literal (number->string n))) (iota 128)))
         (productions
          (append (list (nullable-production 'a '((nonterminal b)))
                        (nullable-production 'b '((nonterminal c)))
                        (nullable-production 'c '((nonterminal a))))
                  (map (lambda (terminal) (nullable-production 'b (list terminal))) terminals))))
    (check-first-reference productions)
    (let-values (((rows index) (compute-first productions (reference-nullable productions))))
      (check (table-ref index 'a) => terminals))))

(def (reverse-first-case)
  (let* ((names (map (lambda (n) (string->symbol (string-append "first-chain-" (number->string n)))) (iota 4096)))
         (productions (map (lambda (name next)
                             (nullable-production name (if next (list (list 'nonterminal next))
                                                          '((terminal literal "x")))))
                           names (append (cdr names) '(#f)))))
    (let-values (((rows index) (compute-first productions (reference-nullable productions))))
      (check (map car rows) => names)
      (check (every (lambda (name) (equal? (table-ref index name) '((terminal literal "x")))) names) => #t))))

(def (cyclic-nullability-case)
  (let (operands '(() ((terminal literal "x")) ((nonterminal a))
                     ((nonterminal b)) ((nonterminal c))
                     ((nonterminal a) (nonterminal a))
                     ((nonterminal a) (nonterminal b))
                     ((marked (nonterminal b) (field value)))))
    (for-each (lambda (a)
                (for-each (lambda (b)
                            (for-each (lambda (c)
                                        (check-nullable-reference
                                         (list (nullable-production 'a a)
                                               (nullable-production 'b b)
                                               (nullable-production 'c c)))) operands)) operands)) operands)))

(def (nullable-occurrences-case)
  (check-nullable-reference
   (list (nullable-production 'a '((nonterminal b) (nonterminal b)))
         (nullable-production 'a '((nonterminal missing)))
         (nullable-production 'b '((nonterminal c) (terminal literal "x")))
         (nullable-production 'b '())
         (nullable-production 'c '((nonterminal c)))))
  (check-nullable-reference
   (list (nullable-production 'a '((nonterminal missing)))
         (nullable-production 'b '((nonterminal a)))
         (nullable-production 'c '())))
  (check-nullable-reference
   (list (nullable-production 'a '((marked (terminal literal "x") (field value))))
         (nullable-production 'b '()) (nullable-production 'c '())))
  (let-values (((names index)
                (compute-nullable
                 (list (nullable-production 'c '()) (nullable-production 'b '())
                       (nullable-production 'c '()) (nullable-production 'a '())))))
    (check names => '(c b a))))

(def (reverse-nullability-case)
  (let* ((names (map (lambda (n) (string->symbol (string-append "chain-" (number->string n)))) (iota 4096)))
         (productions (map (lambda (name next)
                             (nullable-production name (if next (list (list 'nonterminal next)) '())))
                           names (append (cdr names) '(#f)))))
    (let-values (((found index) (compute-nullable productions)))
      (check found => names)
      (check (every (lambda (name) (table-ref index name #f)) names) => #t))))

(def (empty-reference-case)
  (check (admission-result '((start (repeat (reference item))) (item (empty))))
         => (nullable-repeat-result 'start '(reference item))))

(def (optional-reference-case)
  (for-each
   (lambda (kind)
     (check (admission-result
             `((start (,kind (reference item)))
               (item (reference tail)) (tail (optional (literal "a")))))
            => (nullable-repeat-result 'start '(reference item) kind)))
   '(repeat repeat1)))

(def (wrapped-nullability-case)
  (let (operand '(precedence left 1
                   (alias Item (field value
                     (sequence (reference left) (reference right))))))
    (check (admission-result
            `((start (repeat ,operand)) (left (empty))
              (right (optional (literal "b")))))
           => (nullable-repeat-result 'start operand))))

(def (mutual-nullability-case)
  (check (admission-result
          '((start (repeat (reference item)))
            (item (reference tail))
            (tail (choice (reference item) (empty)))))
         => (nullable-repeat-result 'start '(reference item))))

(def (nested-repetition-case)
  (check (admission-result
          '((start (reference container))
            (container (alias Container
                        (sequence (literal "[")
                         (field items (repeat1 (reference item))) (literal "]"))))
            (item (optional (literal "a")))))
         => (nullable-repeat-result 'container '(reference item) 'repeat1)))

(def (obligation-traversal-case)
  (for-each
   (lambda (expression)
     (check (admission-result `((start ,expression) (item (empty))))
            => (nullable-repeat-result 'start '(reference item))))
   '((sequence (literal "a") (repeat (reference item)))
     (choice (empty) (repeat (reference item)))
     (optional (repeat (reference item))))))

(def (construction-admission-case)
  (for-each
   (lambda (construction)
     (check (admission-result
             '((start (repeat (reference item))) (item (empty))) construction)
            => (nullable-repeat-result 'start '(reference item))))
   '(lalr canonical-lr1 partitioned-lr1 follow-partition-lr1 lalr-then-follow)))

(def (glr-repetition-case)
  (for-each
   (lambda (policy)
     (check (admission-result
             '((start (repeat (reference item))) (item (empty))) 'lalr policy)
            => (nullable-repeat-result 'start '(reference item))))
   '(reject selective-glr probe)))

(def (raw-nullability-case)
  (for-each
   (lambda (operand)
     (check (admission-result `((start (repeat ,operand))))
            => (nullable-repeat-result 'start operand)))
   '((empty) (layout-end))))

(def (consuming-suffix-case)
  (let (rules '((start (repeat (reference item)))
                (item (sequence (reference prefix) (literal "b")))
                (prefix (optional (literal "a")))))
    (check (parse-literals rules "") => 'accepted)
    (check (parse-literals rules "babb") => 'accepted)
    (check (parse-literals rules "a") => 'rejected)))

(def (productive-recursion-case)
  (let (rules '((start (repeat (reference item)))
                (item (choice (sequence (reference item) (literal "a"))
                              (literal "b")))))
    (check (parse-literals rules "baabaa") => 'accepted)
    (check (parse-literals rules "a") => 'rejected)))

(def (effective-override-case)
  (let* ((base (make-grammar 'base '()
                 (list (rule-role 'base-rules
                         '((start (repeat (reference item))) (item (literal "a")))))))
         (nullable-role (rule-role 'nullable-item '((item (empty)))))
         (derived (make-grammar 'derived (list base) '()
                    (list (cons 'override nullable-role))))
         (restored (make-grammar 'restored (list derived) '()
                     (list (cons 'override (rule-role 'consuming-item
                                           '((item (literal "b")))))))))
    (check (admission-result (grammar-ir-ref (compile-grammar derived) 'rules))
           => (nullable-repeat-result 'start '(reference item)))
    (check (parse-literals (grammar-ir-ref (compile-grammar restored) 'rules) "bb")
           => 'accepted)))

(def (nullable-origin-case)
  (let* ((base (make-grammar 'base '()
                 (list (rule-role 'base-rules
                         '((start (alias Start (sequence (repeat (reference item))
                                                       (literal "z"))))
                           (item (literal "a")))))))
         (derived (make-grammar 'derived (list base) '()
                    (list (cons 'override (rule-role 'nullable-item '((item (empty))))))))
         (normalized (compile-grammar/context derived))
         (source-map (normalized-grammar-source-map normalized "unknown-source"))
         (result (parameterize ((current-grammar-source-map source-map))
                   (admission-result (grammar-ir-ref (normalized-grammar-ir normalized) 'rules))))
         (details (list-ref (cadr result) 3))
         (origin (cdr (assq 'expressionOrigin details)))
         (references (cdr (assq 'nullableReferencePath details))))
    (check (cdr (assq 'rule origin)) => 'start)
    (check (cdr (assq 'expressionPath origin)) => '(0 0))
    (check (cdr (assq 'componentOwner (cdr (assq 'source origin)))) => 'base-rules)
    (check (map (lambda (row) (cdr (assq 'rule row))) references) => '(item))
    (let (source (cdr (assq 'source (car references))))
      (check (cdr (assq 'componentOwner source)) => 'nullable-item)
      (check (cdr (assq 'compositionOperation source)) => 'override))
    ;; The dynamic context ends with this compile, never contaminating another.
    (check (current-grammar-source-map) => '())))

(def (finite-witness-case)
  (let* ((rules '((start (repeat (reference a)))
                  (a (choice (reference b) (reference epsilon)))
                  (b (reference a)) (epsilon (empty))))
         (result (parameterize ((current-grammar-source-map '((rule))))
                   (admission-result rules)))
         (details (list-ref (cadr result) 3)))
    (check (map (lambda (row) (cdr (assq 'rule row)))
                (cdr (assq 'nullableReferencePath details))) => '(a epsilon))))

(def (shared-witness-case)
  (let* ((names (map (lambda (index) (string->symbol (string-append "nullable-" (number->string index)))) (iota 32)))
         (rules (cons (list 'start (list 'repeat (list 'reference (car names))))
                      (map (lambda (name index)
                             (list name (if (= index 31) '(empty)
                                          (list 'sequence (list 'reference (list-ref names (+ index 1)))
                                                          (list 'reference (list-ref names (+ index 1)))))))
                           names (iota 32))))
         (result (parameterize ((current-grammar-source-map '((rule)))) (admission-result rules)))
         (details (list-ref (cadr result) 3)))
    (check (map (lambda (row) (cdr (assq 'rule row)))
                (cdr (assq 'nullableReferencePath details))) => names)))

(def (choice-acceptance-case)
  (for-each
   (lambda (alternatives)
     (let (rules `((start (alias Start
                           (sequence (field prefix (choice ,@alternatives))
                                     (literal "c"))))))
       (check (parse-literals rules "abc") => 'accepted)
       (check (parse-literals rules "ac") => 'accepted)
       (check (parse-literals rules "ab") => 'rejected)))
   '(((literal "a") (sequence (literal "a") (literal "b")))
     ((sequence (literal "a") (literal "b")) (literal "a")))))

(def (choice-span-case)
  (let* ((short '(literal "a"))
         (long '(sequence (literal "a") (literal "b")))
         (forward `((start (sequence (field prefix (choice ,short ,long))
                                    (literal "c")))))
         (reverse `((start (sequence (field prefix (choice ,long ,short))
                                    (literal "c")))))
         (root (parse-literals forward "abc" #t)))
    (check root => (parse-literals reverse "abc" #t))
    (check (recognition-node-kind root) => 'Start)
    (check (list (recognition-node-start root) (recognition-node-end root)) => '(0 3))
    (check (map recognition-child-field (recognition-node-children root)) => '(prefix #f))
    (let (prefix (recognition-child-value (car (recognition-node-children root))))
      (check (list (recognition-value-start prefix) (recognition-value-end prefix)) => '(0 2)))))

(def (helper-continuation-case)
  (let (rules '((start (sequence (reference prefix) (literal "c")))
                (prefix (choice (literal "a") (reference longer)))
                (longer (sequence (literal "a") (literal "b")))))
    (check (parse-literals rules "abc") => 'accepted)
    (check (parse-literals rules "ac") => 'accepted)
    (check (parse-literals rules "abbc") => 'rejected)))

(def resolved-grammar-test
  (test-suite "resolved grammar admission and CFG alternatives"
    (test-case-add! (TestCase "duplicate CFG rows retain physical IDs and precedence through publication"
                      duplicate-production-identity-case))
    (test-case-add! (TestCase "native dense item layout refines ordered grammar coordinates and goto"
                      dense-layout-refinement-case))
    (test-case-add! (TestCase "lowering proof replay publishes bindings and preserves native identity"
                      lowering-proof-publication-case))
    (test-case-add! (TestCase "physical names, emission ordinals and source occurrences agree"
                      emission-origin-alignment-case))
    (test-case-add! (TestCase "grammar derivation chart, scanner and LR tables compose on the shared model fixture"
                      grammar-model-pipeline-case))
    (test-case-add! (TestCase "schema-valid LR tables need a separate grammar-support obligation"
                      table-support-counterexample-case))
    (test-case-add! (TestCase "one raw token selects different grammar terminals across LR states"
                      state-sensitive-terminal-case))
    (test-case-add! (TestCase "repetition recursion is isolated from sibling alternatives"
                      repetition-owner-case))
    (test-case-add! (TestCase "authored DSL agrees with lowered CFG derivations"
                      source-lowering-correspondence-case))
    (test-case-add! (TestCase "native completion facts match kernel-checked Lean certificates"
                      native-fact-certificate-case))
    (test-case-add! (TestCase "native LR lookaheads equal independent global closure"
                      lookahead-provenance-case))
    (test-case-add! (TestCase "constructed LR graphs preserve item origins and start separation"
                      graph-provenance-case))
    (test-case-add! (TestCase "internal production names are isolated and deterministic"
                      internal-name-isolation-case))
    (test-case-add! (TestCase "complete raw requests choose terminals from the current LR state"
                      raw-request-terminal-case))
    (test-case-add! (TestCase "FIRST retains a terminal prefix before an unproductive suffix"
                      first-is-not-productivity-case))
    (test-case-add! (TestCase "dense item coordinates preserve epsilon and production boundaries"
                      dense-item-coordinates-case))
    (test-case-add! (TestCase "uneven rule widths allocate only actual dotted positions"
                      skew-item-space-case))
    (test-case-add! (TestCase "suffix facts preserve forward scans and marked operands"
                      suffix-facts-case))
    (test-case-add! (TestCase "joint completion preserves delayed and coalesced domain publications"
                      completion-publications-case))
    (test-case-add! (TestCase "productive facts match exhaustive cyclic fixed-point controls"
                      cyclic-productivity-case))
    (test-case-add! (TestCase "productive facts match actual LR families and finite alternative exits"
                      family-productivity-case))
    (test-case-add! (TestCase "reverse productivity chain completes in canonical name order"
                      reverse-productivity-case))
    (test-case-add! (TestCase "LR targets reject a dead start before state construction for every route"
                      dead-start-case))
    (test-case-add! (TestCase "finite epsilon and LR recursive exits survive unreachable dead rules"
                      finite-exits-case))
    (test-case-add! (TestCase "productivity diagnostics retain every blocked choice and finite cyclic graph"
                      blocked-productivity-case))
    (test-case-add! (TestCase "productivity explains authored wrappers rather than generated helpers"
                      authored-productivity-case))
    (test-case-add! (TestCase "productivity sees selected POO exits and their declaration owners"
                      poo-productivity-case))
    (test-case-add! (TestCase "FIRST delta facts match exhaustive cyclic grammar controls"
                      cyclic-first-case))
    (test-case-add! (TestCase "FIRST facts match an independent oracle across actual LR grammar families"
                      family-first-case))
    (test-case-add! (TestCase "FIRST prefix blockers and duplicate edges preserve canonical rows"
                      first-prefix-case))
    (test-case-add! (TestCase "FIRST keeps a terminal prefix without claiming finite completion"
                      incomplete-prefix-case))
    (test-case-add! (TestCase "FIRST bignum deltas preserve terminal catalog order across a cycle"
                      bignum-first-case))
    (test-case-add! (TestCase "long reverse FIRST chain propagates its terminal without full rescans"
                      reverse-first-case))
    (test-case-add! (TestCase "dependency nullable facts match exhaustive cyclic grammar controls"
                      cyclic-nullability-case))
    (test-case-add! (TestCase "nullable alternatives retain duplicate occurrences and terminal blockers"
                      nullable-occurrences-case))
    (test-case-add! (TestCase "long reverse nullable chain publishes every name in production order"
                      reverse-nullability-case))
    (test-case-add! (TestCase "direct reference to an empty rule rejects before LR conflicts"
                      empty-reference-case))
    (test-case-add! (TestCase "indirect optional reference rejects both repetition forms"
                      optional-reference-case))
    (test-case-add! (TestCase "nullable sequence through field alias and precedence rejects"
                      wrapped-nullability-case))
    (test-case-add! (TestCase "nullable alternative propagates through a mutual reference cycle"
                      mutual-nullability-case))
    (test-case-add! (TestCase "nested repetitions retain the original rule owner"
                      nested-repetition-case))
    (test-case-add! (TestCase "boolean analysis never skips nested repetition obligations"
                      obligation-traversal-case))
    (test-case-add! (TestCase "all LR constructions share resolved repetition admission"
                      construction-admission-case))
    (test-case-add! (TestCase "selective GLR cannot admit a zero-width repetition cycle"
                      glr-repetition-case))
    (test-case-add! (TestCase "raw IR empty and layout operands reject before state construction"
                      raw-nullability-case))
    (test-case-add! (TestCase "a nullable prefix followed by a consuming suffix remains valid"
                      consuming-suffix-case))
    (test-case-add! (TestCase "productive left recursion remains valid inside repeat"
                      productive-recursion-case))
    (test-case-add! (TestCase "effective POO override is analyzed rather than its parent declaration"
                      effective-override-case))
    (test-case-add! (TestCase "nullable diagnostics retain selected POO override and expression occurrence"
                      nullable-origin-case))
    (test-case-add! (TestCase "finite nullable witness skips cycles and retains the effective empty exit"
                      finite-witness-case))
    (test-case-add! (TestCase "nullable explanation follows a shared DAG without expanding derivation trees"
                      shared-witness-case))
    (test-case-add! (TestCase "CFG choice reordering preserves acceptance with shared token prefixes"
                      choice-acceptance-case))
    (test-case-add! (TestCase "choice ordering preserves node fields and exact ranges"
                      choice-span-case))
    (test-case-add! (TestCase "CFG helper-reference alternatives preserve enclosing continuation"
                      helper-continuation-case))))
