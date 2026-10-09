#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; LR(1)-but-not-LALR(1) grammar checks the state-splitting reference.

(import :std/test
        (only-in :core/observability/testing-case poo-flow-test-case)
        (only-in :gerbil-parser/src/compiler/funcs
                 compiler-index-set-difference compiler-index-set-union
                 compiler-index-set-for-each compiler-index-set-reachable
                 compiler-index-set-reachable/memo
                 compiler-index-partition-refine)
        (only-in :gerbil-parser/src/compiler/lr
                 compute-first compute-nullable lower-rules lr-spec-ref
                 production-rhs production-table production-terminal-catalog)
        (only-in :gerbil-parser/src/compiler/lr-automaton
                 build-lr0-automaton make-item-layout make-core-symbol-catalog)
        (only-in :gerbil-parser/src/compiler/lr-compiler compile-lr-spec)
        (only-in :gerbil-parser/src/compiler/lr-conflict-candidates
                 forward-reachable-follow-masks
                 initial-backward-follow-partitions
                 lr0-conflict-candidates raw-conflict-cells)
        (only-in :gerbil-parser/src/compiler/lr-lookahead
                 build-states-via-canonical-lr1 build-states-via-lr0)
        (only-in :gerbil-parser/src/runtime/lr-parser
                 lr-parse lr-rejection-condition?)
        (only-in :gerbil-parser/src/runtime/token make-token)
        (only-in :gerbil-parser/t/fixtures/lr1-construction
                 lr1-not-lalr-rules shared-lookahead-rules
                 genuine-reduce-conflict-rules precedence-expression-rules
                 mixed-context-rules lr1-context-family-rules
                 state-local-candidate-family-rules
                 mixed-context-family-rules acyclic-mixed-context-family-rules
                 split-shared-context-family-rules
                 unreachable-repeated-context-family-rules
                 inactive-core-conflict-rules))

(def (character-tokens text (base 0))
  (let loop ((index 0) (found '()))
    (if (= index (string-length text))
      (reverse found)
      (loop (+ index 1)
            (cons (make-token 'punctuation
                              (string (string-ref text index))
                              (+ base index) (+ base index 1))
                  found)))))

(def (parse-token-result spec tokens)
  (with-catch
   (lambda (condition)
     (if (lr-rejection-condition? condition)
       'rejected
       (raise condition)))
   (lambda ()
     (let-values (((root rest)
                   (lr-parse spec tokens)))
       (unless (null? rest)
         (error "LR parser left a token suffix" rest))
       root))))

(def (parse-result spec text)
  (parse-token-result spec (character-tokens text)))

(def (check-parse-equivalence canonical other alphabet length)
  (let walk ((prefix "") (remaining length))
    (if (zero? remaining)
      (check (equal? (parse-result canonical prefix)
                     (parse-result other prefix))
             => #t)
      (for-each
       (lambda (part)
         (walk (string-append prefix part) (- remaining 1)))
       alphabet))))

(def (check-lr0-candidates-cover-canonical rules)
  (let* ((productions (lower-rules rules 'source-file))
         (table (production-table productions)))
    (let-values (((nullable nullable-index)
                  (compute-nullable productions)))
      (let-values (((first first-index)
                    (compute-first productions nullable-index)))
        (let-values (((lr0-states lr0-count terminals candidates)
                      (lr0-conflict-candidates
                       productions table first-index nullable-index)))
          (let-values (((canonical-states canonical-count lookaheads offsets
                            transitions canonical-terminals layout core-symbols
                            state-visits item-visits)
                        (build-states-via-canonical-lr1
                         productions table first-index nullable-index)))
            (let ((index (make-table test: equal?))
                  (canonical-conflicts
                   (raw-conflict-cells
                    canonical-states canonical-count lookaheads offsets
                    canonical-terminals layout core-symbols)))
              (check (equal? terminals canonical-terminals) => #t)
              (let index-loop ((state 0))
                (when (< state lr0-count)
                  (table-set! index (vector-ref lr0-states state) state)
                  (index-loop (+ state 1))))
              (let check-loop ((state 0))
                (when (< state canonical-count)
                  (let (lr0-state
                        (table-ref index (vector-ref canonical-states state)
                                   #f))
                    (check (number? lr0-state) => #t)
                    (check
                     (compiler-index-set-difference
                      (vector-ref canonical-conflicts state)
                      (vector-ref candidates lr0-state))
                     => 0))
                  (check-loop (+ state 1))))
              (values candidates canonical-conflicts))))))))

(def (check-forward-follows-match-lr0 rules)
  (let* ((productions (lower-rules rules 'source-file))
         (table (production-table productions)))
    (let-values (((nullable nullable-index)
                  (compute-nullable productions)))
      (let-values (((first first-index)
                    (compute-first productions nullable-index)))
        (let-values (((follows follow-terminals)
                      (forward-reachable-follow-masks
                       productions table first-index nullable-index)))
          (let-values (((states count lookaheads offsets transitions
                            lr0-terminals layout core-symbols
                            state-visits item-visits)
                        (build-states-via-lr0
                         productions table first-index nullable-index)))
            (let (completed (make-vector (vector-length table) 0))
              (check (equal? follow-terminals lr0-terminals) => #t)
              (let state-loop ((state 0))
                (when (< state count)
                  (let ((node (vector-ref offsets state)))
                    (for-each
                     (lambda (core)
                       (unless (vector-ref core-symbols core)
                         (let (production-id (quotient core (cdr layout)))
                           (vector-set!
                            completed production-id
                            (compiler-index-set-union
                             (vector-ref completed production-id)
                             (vector-ref lookaheads node)))))
                       (set! node (+ node 1)))
                     (vector-ref states state)))
                  (state-loop (+ state 1))))
              (let production-loop ((id 0))
                (when (< id (vector-length table))
                  (check (vector-ref follows id)
                         => (vector-ref completed id))
                  (production-loop (+ id 1)))))))))))

(def (check-initial-backward-partitions rules)
  (let* ((productions (lower-rules rules 'source-file))
         (table (production-table productions)))
    (let-values (((nullable nullable-index)
                  (compute-nullable productions)))
      (let-values (((first first-index)
                    (compute-first productions nullable-index)))
        (let-values (((follows terminals)
                      (forward-reachable-follow-masks
                       productions table first-index nullable-index)))
          (let-values (((partitions candidates partition-terminals)
                        (initial-backward-follow-partitions
                         productions table first-index nullable-index)))
            (check (equal? terminals partition-terminals) => #t)
            (let ((width (quotient (vector-length partitions)
                                   (vector-length table))))
              (let production-loop ((id 0))
                (when (< id (vector-length table))
                  (let dot-loop ((dot 0))
                    (when (<= dot (length
                                   (production-rhs (vector-ref table id))))
                      (let (blocks (vector-ref partitions (+ dot (* id width))))
                        (check
                         (foldl (lambda (block mask)
                                  (compiler-index-set-union mask (cdr block)))
                                0 blocks)
                         => (vector-ref follows id)))
                      (dot-loop (+ dot 1))))
                  (production-loop (+ id 1)))))
            partitions))))))

;; Kernel identity is independent of closure construction and key encoding.
;; Check its graph invariants on recursive, nullable, and shared contexts.
(def (check-lr0-kernel-identity rules)
  (let* ((productions (lower-rules rules 'source-file))
         (table (production-table productions)))
    (let-values (((terminals terminal-index)
                  (production-terminal-catalog productions)))
      (let* ((layout (make-item-layout table terminals))
             (symbols (make-core-symbol-catalog table layout)))
        (let-values (((states transitions count by-lhs visits)
                      (build-lr0-automaton productions table layout symbols)))
          (let ((kernels (make-vector count '()))
                (seen (make-table test: equal?)))
            (check visits => count)
            (let index ((state 0))
              (when (< state count)
                (let (kernel
                      (filter (lambda (item)
                                (not (zero? (modulo item (cdr layout)))))
                              (vector-ref states state)))
                  (vector-set! kernels state kernel)
                  (if (zero? state)
                    (check kernel => '())
                    (begin
                      (check (pair? kernel) => #t)
                      (check (table-ref seen kernel #f) => #f)
                      (table-set! seen kernel state))))
                (index (+ state 1))))
            (let edges ((state 0))
              (when (< state count)
                ;; Every after-dot symbol must be published exactly once, in
                ;; sorted-item discovery order, including shared predictions.
                (let ((expected '()) (symbols-seen (make-table test: equal?)))
                  (for-each
                   (lambda (item)
                     (let (symbol (vector-ref symbols item))
                       (when (and symbol (not (table-ref symbols-seen symbol #f)))
                         (table-set! symbols-seen symbol #t)
                         (set! expected (cons symbol expected)))))
                   (vector-ref states state))
                  (check (map car (vector-ref transitions state)) => (reverse expected)))
                (for-each
                 (lambda (edge)
                   (let* ((symbol (car edge)) (target (cdr edge))
                          (expected
                           (map (lambda (item) (+ item 1))
                                (filter (lambda (item)
                                          (equal? (vector-ref symbols item) symbol))
                                        (vector-ref states state)))))
                     (check (> target 0) => #t)
                     (check (equal? expected (vector-ref kernels target)) => #t)))
                 (vector-ref transitions state))
                (edges (+ state 1))))))))))

(def canonical-lr1-reference-test
  (test-suite "canonical LR(1) reference"
    (poo-flow-test-case "affected partition refinement matches exhaustive synchronous oracle"
      (def (signature predecessors node ids)
        (filter-map
         (lambda (label)
           (let (groups
                 (filter (lambda (group)
                           (ormap (lambda (edge)
                                    (and (= label (car edge))
                                         (= group (vector-ref ids (cdr edge)))))
                                  (vector-ref predecessors node)))
                         (iota (vector-length ids))))
             (and (pair? groups) (cons label groups))))
         '(0 2)))
      (def (oracle initial initial-count predecessors record)
        (let refine ((ids initial) (groups initial-count))
          (let ((next (make-vector (vector-length ids) 0))
                (keys '()) (next-count 0))
            (for-each
             (lambda (node)
               ;; Match the existing engine's singleton elision when counting
               ;; work, while the oracle still recomputes every node's key.
               (when (> (length (filter (lambda (group)
                                         (= group (vector-ref ids node)))
                                       (vector->list ids))) 1)
                 (record))
               (let* ((key (cons (vector-ref ids node)
                                 (signature predecessors node ids)))
                      (known (assoc key keys)))
                 (unless known
                   (set! known (cons key next-count))
                   (set! keys (cons known keys))
                   (set! next-count (+ next-count 1)))
                 (vector-set! next node (cdr known))))
             (iota (vector-length ids)))
            (if (= groups next-count) (values next next-count)
                (refine next next-count)))))
      (def (compare predecessors initial initial-count)
        (let ((saved (vector->list initial)) (expected-calls 0) (actual-calls 0))
          (let-values (((expected expected-count)
                        (oracle initial initial-count predecessors
                                (lambda () (set! expected-calls (+ expected-calls 1)))))
                       ((actual actual-count)
                        (compiler-index-partition-refine
                         initial initial-count
                         (lambda (node ids) (signature predecessors node ids))
                         (lambda (source visit)
                           (for-each
                            (lambda (target)
                              (for-each (lambda (edge)
                                          (when (= source (cdr edge)) (visit target)))
                                        (vector-ref predecessors target)))
                            (iota (vector-length predecessors))))
                         (lambda (groups nodes examined)
                           (set! actual-calls (+ actual-calls examined))))))
            (check actual => expected)
            (check actual-count => expected-count)
            (check (vector->list initial) => saved)
            (check (<= actual-calls expected-calls) => #t)
            (values actual-calls expected-calls))))
      (let graphs ((graph 0))
        (when (< graph 512)
          (let (predecessors (make-vector 3 '()))
            (for-each (lambda (bit)
                        (when (not (zero? (bitwise-and graph (arithmetic-shift 1 bit))))
                          (let ((target (modulo bit 3)) (source (quotient bit 3)))
                            (vector-set! predecessors target
                                         (cons (cons 0 source) (vector-ref predecessors target))))))
                      (iota 9))
            (for-each (lambda (entry) (compare predecessors (car entry) (cdr entry)))
                      (list (cons '#(0 0 0) 1) (cons '#(0 0 1) 2)
                            (cons '#(0 1 0) 2) (cons '#(0 1 1) 2) (cons '#(0 1 2) 3))))
          (graphs (+ graph 1))))
      (let graphs ((graph 0))
        (when (< graph 64)
          (let (predecessors (make-vector 3 '()))
            (for-each (lambda (bit)
                        (when (not (zero? (bitwise-and graph (arithmetic-shift 1 bit))))
                          (let* ((source (modulo bit 3)) (target (modulo (+ source 1) 3))
                                 (edge (cons (if (< bit 3) 0 2) source)))
                            (vector-set! predecessors target
                                         (cons edge (cons edge (vector-ref predecessors target)))))))
                      (iota 6))
            (compare predecessors '#(0 0 0) 1)
            (compare predecessors '#(0 0 1) 2))
          (graphs (+ graph 1))))
      ;; A split propagates along the chain while the independent self-loop
      ;; group remains stable, despite changes to its numeric group identity.
      (let (predecessors '#(() ((0 . 0)) ((0 . 1)) ((0 . 2)) ((0 . 3)) ((0 . 4)) ((2 . 6)) ((2 . 7))))
        (let-values (((actual expected) (compare predecessors '#(0 1 1 1 1 1 2 2) 3)))
          (check actual => 16)
          (check expected => 24)
          (check (< actual expected) => #t)))
      ;; Sizes belong to group identities, not node order. Exercise dense IDs
      ;; that are initially permuted before first-seen canonical publication.
      (compare '#(() ((0 . 0)) ((2 . 1))) '#(1 0 0) 2)
      (compare '#(() ((0 . 0)) ((2 . 1))) '#(2 0 1) 3)
      (compare '#() '#() 0)
      ;; Larger interacting groups exercise retained/new identities and both
      ;; member/worklist orders. Reverse the entire graph, not just its IDs;
      ;; every result is compared with the independent synchronous oracle.
      (let graphs ((graph 0))
        (when (< graph 64)
          (let ((predecessors (make-vector 6 '()))
                (reversed (make-vector 6 '())))
            (for-each
             (lambda (source)
               (let* ((target (modulo (+ source 1) 6))
                      (label (if (zero? (bitwise-and graph (arithmetic-shift 1 source))) 0 2))
                      (edge (cons label source)))
                 (vector-set! predecessors target
                              (cons edge (cons edge (vector-ref predecessors target))))
                 (when (zero? (modulo source 2))
                   (let (target (modulo (+ source 3) 6))
                     (vector-set! predecessors target
                                  (cons (cons 2 source) (vector-ref predecessors target)))))
                 (when (not (zero? (bitwise-and graph (arithmetic-shift 1 source))))
                   (vector-set! predecessors source
                                (cons (cons 0 source) (vector-ref predecessors source))))))
             (iota 6))
            (for-each
             (lambda (node)
               (vector-set! reversed (- 5 node)
                            (map (lambda (edge) (cons (car edge) (- 5 (cdr edge))))
                                 (vector-ref predecessors node))))
             (iota 6))
            (for-each
             (lambda (entry)
               (compare predecessors (car entry) (cdr entry))
               (compare reversed (list->vector (reverse (vector->list (car entry)))) (cdr entry)))
             (list (cons '#(0 0 0 0 0 0) 1) (cons '#(1 0 0 1 0 0) 2)
                   (cons '#(2 0 1 2 0 1) 3) (cons '#(0 1 1 1 1 1) 2))))
          (graphs (+ graph 1))))
      ;; Workspaces must remain local to an invocation. A result produced
      ;; after multiple rounds survives a separate refinement call.
      (let ((initial '#(0 0 0)) (rounds 0))
        (let-values (((first groups)
                      (compiler-index-partition-refine
                       initial 1 (lambda (node ids) (if (= node 0) 0 1))
                       (lambda (source visit) (void))
                       (lambda (groups nodes examined) (set! rounds (+ rounds 1))))))
          (check first => '#(0 1 1))
          (check groups => 2)
          (check rounds => 2)
          (let-values (((second groups)
                        (compiler-index-partition-refine
                         initial 1 (lambda (node ids) node)
                         (lambda (source visit) (void))
                         (lambda (groups nodes examined) (void)))))
            (check second => '#(0 1 2))
            (check groups => 3)
            (check (eq? first second) => #f))
          (check first => '#(0 1 1))
          (check initial => '#(0 0 0)))))
    (poo-flow-test-case "indexed reachability agrees with exhaustive list graph oracle"
      (def (oracle initial rows)
        (let visit ((pending (filter (lambda (node) (not (zero? (bitwise-and initial (arithmetic-shift 1 node))))) (iota (vector-length rows))))
                    (seen '()))
          (if (null? pending)
            (foldl (lambda (node mask) (bitwise-ior mask (arithmetic-shift 1 node))) 0 seen)
            (let (node (car pending))
              (if (memv node seen)
                (visit (cdr pending) seen)
                (visit (append (vector-ref rows node) (cdr pending)) (cons node seen)))))))
      (let graphs ((graph 0))
        (when (< graph 512)
          (let ((edges (make-vector 3 0)) (rows (make-vector 3 '())))
            (for-each (lambda (node)
              (let (mask (bitwise-and 7 (arithmetic-shift graph (* -3 node))))
                (vector-set! edges node mask)
                (vector-set! rows node (filter (lambda (target) (not (zero? (bitwise-and mask (arithmetic-shift 1 target))))) '(0 1 2))))) '(0 1 2))
            (let (cache (make-vector 3 #f))
              (for-each
               (lambda (initial)
                 (let (expected (oracle initial rows))
                   (check (compiler-index-set-reachable initial edges) => expected)
                   (check (compiler-index-set-reachable/memo initial edges (make-vector 3 #f)) => expected)
                   (check (compiler-index-set-reachable/memo initial edges cache) => expected)))
               (iota 8))
              (for-each
               (lambda (node)
                 (let (closure (vector-ref cache node))
                   (when closure (check closure => (oracle (arithmetic-shift 1 node) rows)))))
               (iota 3))))
          (graphs (+ graph 1))))
      (check (compiler-index-set-reachable 0 '#()) => 0)
      (check (compiler-index-set-reachable/memo 0 '#() '#()) => 0)
      (let ((chain (make-vector 1024 0)) (cycle (make-vector 1024 0)))
        (let nodes ((node 0))
          (when (< node 1024)
            (when (< node 1023) (vector-set! chain node (arithmetic-shift 1 (+ node 1))))
            (vector-set! cycle node (arithmetic-shift 1 (modulo (+ node 1) 1024)))
            (nodes (+ node 1))))
        (check (compiler-index-set-reachable 1 chain) => (- (arithmetic-shift 1 1024) 1))
        (check (compiler-index-set-reachable (arithmetic-shift 1 512) cycle) => (- (arithmetic-shift 1 1024) 1))
        ;; High-bit, overlapping roots preserve exact cold/warm results and
        ;; graph-specific cache ownership across independent calls.
        (let ((cache (make-vector 1024 #f))
              (roots (bitwise-ior 1 (arithmetic-shift 1 512) (arithmetic-shift 1 1023)))
              (all (- (arithmetic-shift 1 1024) 1)))
          (check (compiler-index-set-reachable/memo roots chain cache) => all)
          (check (vector-ref cache 0) => all)
          (check (compiler-index-set-reachable/memo roots chain cache) => all)
          (check (compiler-index-set-reachable/memo roots cycle (make-vector 1024 #f)) => all))))
    (poo-flow-test-case "indexed traversal agrees with bit membership across fields"
      (def (check-mask mask)
        (let ((actual '()) (expected '()))
          (compiler-index-set-for-each mask (lambda (index) (set! actual (cons index actual))))
          (let bits ((index 0))
            (when (< index (integer-length mask))
              (when (not (zero? (bitwise-and mask (arithmetic-shift 1 index)))) (set! expected (cons index expected)))
              (bits (+ index 1))))
          (check actual => expected)))
      (let masks ((mask 0))
        (when (< mask 4096) (check-mask mask) (masks (+ mask 1))))
      (let (width (integer-length (##greatest-fixnum)))
        (for-each
         (lambda (index)
           (let (bit (arithmetic-shift 1 index))
             (check-mask bit)
             (check-mask (- bit 1))
             (check-mask (bitwise-ior bit 1 (arithmetic-shift 1 (quotient index 2))))))
         [(- width 1) width (+ width 1) (* width 2) 1024 4096]))
      ;; Reentrant traversal must not disturb the outer field or order.
      (let ((outer '()) (inner '()))
        (compiler-index-set-for-each (bitwise-ior 1 (arithmetic-shift 1 1024))
          (lambda (index)
            (set! outer (cons index outer))
            (compiler-index-set-for-each 5 (lambda (bit) (set! inner (cons bit inner))))))
        (check outer => '(1024 0))
        (check inner => '(2 0 2 0)))
      (let ((outer '()) (nested-count 0))
        (compiler-index-set-for-each (- (arithmetic-shift 1 128) 1)
          (lambda (index)
            (set! outer (cons index outer))
            (compiler-index-set-for-each 5 (lambda (_) (set! nested-count (+ nested-count 1))))))
        (check outer => (reverse (iota 128)))
        (check nested-count => 256))
      (let ((calls 0) (sentinel (list 'callback-failure)))
        (check (with-catch identity
                 (lambda ()
                   (compiler-index-set-for-each (- (arithmetic-shift 1 128) 1)
                     (lambda (_) (set! calls (+ calls 1)) (raise sentinel)))))
               => sentinel)
        (check calls => 1)))
    (poo-flow-test-case "LR(0) kernels uniquely identify states and goto targets"
      (check-lr0-kernel-identity precedence-expression-rules)
      (check-lr0-kernel-identity inactive-core-conflict-rules)
      (check-lr0-kernel-identity (mixed-context-family-rules 16))
      (check-lr0-kernel-identity (state-local-candidate-family-rules 16))
      (check-lr0-kernel-identity (lr1-context-family-rules 16)))
    (poo-flow-test-case "forward reachable follows agree with LR(0) propagation"
      (check-forward-follows-match-lr0 lr1-not-lalr-rules)
      (check-forward-follows-match-lr0 shared-lookahead-rules)
      (check-forward-follows-match-lr0 genuine-reduce-conflict-rules)
      (check-forward-follows-match-lr0 mixed-context-rules)
      (check-forward-follows-match-lr0 (lr1-context-family-rules 16)))
    (poo-flow-test-case "initial backward partition preserves all follow strings"
      (let (partitions
            (check-initial-backward-partitions lr1-not-lalr-rules))
        (check (vector-any (lambda (blocks)
                             (and blocks (> (length blocks) 1)))
                           partitions)
               => #t))
      (check-initial-backward-partitions shared-lookahead-rules)
      (check-initial-backward-partitions genuine-reduce-conflict-rules))
    (poo-flow-test-case "initial partition skips cores without a conflict action"
      (let (blocks (vector->list
                   (check-initial-backward-partitions
                    inactive-core-conflict-rules)))
        (check (foldl (lambda (entry total)
                        (+ total (if entry (length entry) 0)))
                      0 blocks)
               => 41)
        (check (foldl (lambda (entry total)
                        (+ total (if (and entry (> (length entry) 1)) 1 0)))
                      0 blocks)
               => 3)))
    (poo-flow-test-case "LR(0) raw candidates conservatively cover canonical conflicts"
      (let-values (((candidates canonical-conflicts)
                    (check-lr0-candidates-cover-canonical
                     lr1-not-lalr-rules)))
        (check (vector-any (lambda (mask) (not (zero? mask))) candidates)
               => #t)
        (check (vector-any (lambda (mask) (not (zero? mask)))
                           canonical-conflicts)
               => #f))
      (check-lr0-candidates-cover-canonical shared-lookahead-rules)
      (check-lr0-candidates-cover-canonical inactive-core-conflict-rules)
      (let-values (((candidates canonical-conflicts)
                    (check-lr0-candidates-cover-canonical
                     genuine-reduce-conflict-rules)))
        (check (vector-any (lambda (mask) (not (zero? mask))) candidates)
               => #t)
        (check (vector-any (lambda (mask) (not (zero? mask)))
                           canonical-conflicts)
               => #t))
      (check-exception
       (compile-lr-spec genuine-reduce-conflict-rules 'source-file
                        'reject #f 'follow-partition-lr1)
       true))
    (poo-flow-test-case "split contexts remove an LALR reduce/reduce conflict"
      (check-exception
       (compile-lr-spec lr1-not-lalr-rules 'source-file)
       true)
      (let (spec
            (compile-lr-spec lr1-not-lalr-rules 'source-file
                             'reject #f 'canonical-lr1))
        (check (lr-spec-ref spec 'algorithm)
               => 'canonical-lr1-reference-v1)
        (check (> (lr-spec-ref spec 'state-count) 0) => #t))
      (let ((canonical
             (compile-lr-spec lr1-not-lalr-rules 'source-file
                              'reject #f 'canonical-lr1))
            (partitioned
             (compile-lr-spec lr1-not-lalr-rules 'source-file
                              'reject #f 'partitioned-lr1))
            (direct
             (compile-lr-spec lr1-not-lalr-rules 'source-file
                              'reject #f 'follow-partition-lr1)))
        (check (lr-spec-ref partitioned 'algorithm)
               => 'conflict-partitioned-lr1-v1)
        (check (lr-spec-ref direct 'algorithm)
               => 'follow-partition-lr1-v1)
        (check (<= (lr-spec-ref partitioned 'state-count)
                   (lr-spec-ref canonical 'state-count))
               => #t)
        (check (> (lr-spec-ref direct 'follow-block-count) 0) => #t)))
    (poo-flow-test-case "independent LR(1) contexts preserve parse products"
      (for-each
       (lambda (context-count)
         (let* ((rules (lr1-context-family-rules context-count))
                (canonical
                 (compile-lr-spec rules 'source-file 'reject #f
                                  'canonical-lr1))
                (direct
                 (compile-lr-spec rules 'source-file 'reject #f
                                  'follow-partition-lr1)))
           (check-exception (compile-lr-spec rules 'source-file) true)
           (check (lr-spec-ref canonical 'state-count)
                  => (lr-spec-ref direct 'state-count))
           (when (memv context-count '(32 48 64))
             (check (lr-spec-ref direct 'follow-block-count) => 0))
           (for-each
            (lambda (index)
              (let* ((prefix
                      (string-append "region-" (number->string index) ":"))
                     (prefix-length (string-length prefix))
                     (prefix-token
                      (make-token 'punctuation prefix 0 prefix-length)))
                (for-each
                 (lambda (body)
                   (let* ((tokens
                           (cons prefix-token
                                 (character-tokens body prefix-length)))
                          (expected (parse-token-result canonical tokens)))
                     (check (eq? expected 'rejected) => #f)
                     (check (equal? expected
                                    (parse-token-result direct tokens))
                            => #t)))
                 '("acd" "ace" "bcd" "bce"))
                (let (tokens
                      (cons prefix-token
                            (character-tokens "acc" prefix-length)))
                  (check (parse-token-result canonical tokens) => 'rejected)
                  (check (parse-token-result direct tokens) => 'rejected))))
            (iota context-count))))
       '(4 16 32 48 64)))
    (poo-flow-test-case "state-local conflict candidates preserve parse products"
      (let* ((rules (state-local-candidate-family-rules 32))
             (canonical
              (compile-lr-spec rules 'source-file 'selective-glr #f
                               'canonical-lr1))
             (direct
              (compile-lr-spec rules 'source-file 'selective-glr #f
                               'follow-partition-lr1)))
        (check (< (lr-spec-ref direct 'state-count)
                  (lr-spec-ref canonical 'state-count)) => #t)
        (check (> (lr-spec-ref direct 'follow-block-count) 0) => #t)
        (for-each
         (lambda (index)
           (let* ((prefix
                   (string-append "region-" (number->string index) ":"))
                  (prefix-length (string-length prefix))
                  (prefix-token
                   (make-token 'punctuation prefix 0 prefix-length)))
             (def (tokens body)
               (cons prefix-token (character-tokens body prefix-length)))
             (for-each
              (lambda (body)
                (let (input (tokens body))
                  (check (equal? (parse-token-result canonical input)
                                 (parse-token-result direct input)) => #t)))
              '("axd" "axe" "bxxe" "bxe" "axf" "bxxd" ""))))
         '(0 15 31))))
    (poo-flow-test-case "mixed context family compresses forward follow blocks"
      (for-each
       (lambda (context-count)
         (let* ((rules (mixed-context-family-rules context-count))
                (canonical
                 (compile-lr-spec rules 'source-file 'reject #f
                                  'canonical-lr1))
                (direct
                 (compile-lr-spec rules 'source-file 'reject #f
                                  'follow-partition-lr1)))
           (check (< (lr-spec-ref direct 'state-count)
                     (lr-spec-ref canonical 'state-count)) => #t)
           (check (< (lr-spec-ref direct 'follow-block-count)
                     (lr-spec-ref direct 'output-item-count)) => #t)
           (for-each
            (lambda (index)
              (let* ((prefix
                      (string-append "region-" (number->string index) ":"))
                     (prefix-length (string-length prefix))
                     (prefix-token
                      (make-token 'punctuation prefix 0 prefix-length)))
                (for-each
                 (lambda (body)
                   (let* ((tokens
                          (cons prefix-token
                                 (character-tokens body prefix-length)))
                          (expected (parse-token-result canonical tokens)))
                     (check (eq? expected 'rejected)
                            => (if (member body '("acc" "cd")) #t #f))
                     (check (equal? expected
                                    (parse-token-result direct tokens))
                            => #t)))
                 '("acd" "ace" "bcd" "bce" "dd" "cdd" "acc" "cd"))))
            (iota context-count))))
       '(4 16 32 64 128)))
    (poo-flow-test-case "FOLLOW stable edges preserve products after production reordering"
      ;; Reordering productions changes core/block numbering. Compare with
      ;; canonical construction to catch reuse of a previous round's edge IDs,
      ;; including singleton and inactive blocks beside the final split.
      (def (nullable-tail-family count)
        (let (rules (mixed-context-family-rules count))
          (cons (list 'source-file
                      (list 'sequence (cadar rules) '(optional (literal "z"))))
                (cdr rules))))
      (for-each
       (lambda (family)
         (for-each
          (lambda (count)
            (let (rules (family count))
              (for-each
               (lambda (ordered)
                 (let ((canonical (compile-lr-spec ordered 'source-file 'reject #f 'canonical-lr1))
                       (direct (compile-lr-spec ordered 'source-file 'reject #f 'follow-partition-lr1)))
                   (check (> (lr-spec-ref direct 'follow-block-count) 0) => #t)
                   (for-each
                    (lambda (region)
                      (let* ((prefix (string-append "region-" (number->string region) ":"))
                             (size (string-length prefix)))
                        (for-each
                         (lambda (body)
                           (let (tokens (cons (make-token 'punctuation prefix 0 size)
                                             (character-tokens body size)))
                             (check (parse-token-result direct tokens)
                                    => (parse-token-result canonical tokens))))
                         '("acd" "ace" "bcd" "bce" "cc" "cd" "dc" "dd" "acc" "acdz" "cddz" "accz"))))
                    (iota count))))
               (list rules (reverse rules)))))
          '(4 8)))
       (list mixed-context-family-rules acyclic-mixed-context-family-rules nullable-tail-family)))
    (poo-flow-test-case "acyclic shared contexts still need follow compression"
      (for-each
       (lambda (context-count)
         (let* ((rules (acyclic-mixed-context-family-rules context-count))
                (canonical
                 (compile-lr-spec rules 'source-file 'reject #f
                                  'canonical-lr1))
                (direct
                 (compile-lr-spec rules 'source-file 'reject #f
                                  'follow-partition-lr1)))
           (check (< (lr-spec-ref direct 'state-count)
                     (lr-spec-ref canonical 'state-count)) => #t)
           (check (> (lr-spec-ref direct 'follow-block-count) 0) => #t)
           (for-each
            (lambda (index)
              (let* ((prefix
                      (string-append "region-" (number->string index) ":"))
                     (prefix-length (string-length prefix))
                     (prefix-token
                      (make-token 'punctuation prefix 0 prefix-length)))
                (for-each
                 (lambda (body)
                   (let* ((tokens
                           (cons prefix-token
                                 (character-tokens body prefix-length)))
                          (expected (parse-token-result canonical tokens)))
                     (check (equal? expected
                                    (parse-token-result direct tokens)) => #t)))
                 '("acd" "ace" "bcd" "bce" "cc" "cd" "dc" "dd" "acc"))))
            (list 0 15 (- context-count 1)))))
       '(32 64)))
    (poo-flow-test-case "split shared productions do not force compression"
      (for-each
       (lambda (context-count)
         (let* ((rules (split-shared-context-family-rules context-count))
                (canonical
                 (compile-lr-spec rules 'source-file 'reject #f
                                  'canonical-lr1))
                (direct
                 (compile-lr-spec rules 'source-file 'reject #f
                                  'follow-partition-lr1)))
           (check (lr-spec-ref direct 'state-count)
                  => (lr-spec-ref canonical 'state-count))
           (check (zero? (lr-spec-ref direct 'follow-block-count))
                  => (>= context-count 64))
           (for-each
            (lambda (index)
              (let* ((prefix
                      (string-append "region-" (number->string index) ":"))
                     (prefix-length (string-length prefix))
                     (prefix-token
                      (make-token 'punctuation prefix 0 prefix-length)))
                (for-each
                 (lambda (body)
                   (let* ((tokens
                           (cons prefix-token
                                 (character-tokens body prefix-length)))
                          (expected (parse-token-result canonical tokens)))
                     (check (eq? expected 'rejected)
                            => (equal? body "acc"))
                     (check (equal? expected
                                    (parse-token-result direct tokens)) => #t)))
                 '("dx" "dy" "cdx" "cdy" "acc"))))
            (list 0 15 (- context-count 1)))))
       '(16 64)))
    (poo-flow-test-case "unreachable repeated helper keeps canonical route"
      (let* ((rules (unreachable-repeated-context-family-rules 64))
             (canonical
              (compile-lr-spec rules 'source-file 'reject #f 'canonical-lr1))
             (direct
              (compile-lr-spec rules 'source-file 'reject #f
                               'follow-partition-lr1)))
        (check (lr-spec-ref direct 'state-count)
               => (lr-spec-ref canonical 'state-count))
        (check (lr-spec-ref direct 'follow-block-count) => 0)
        (for-each
         (lambda (index)
           (let* ((prefix
                   (string-append "region-" (number->string index) ":"))
                  (prefix-length (string-length prefix))
                  (prefix-token
                   (make-token 'punctuation prefix 0 prefix-length)))
             (for-each
              (lambda (body)
                (let (tokens
                      (cons prefix-token
                            (character-tokens body prefix-length)))
                  (check (equal? (parse-token-result canonical tokens)
                                 (parse-token-result direct tokens)) => #t)))
              '("acd" "bce" "acc"))))
         '(0 31 63))))
    (poo-flow-test-case "bounded canonical trial stops above its state budget"
      (let* ((productions (lower-rules lr1-not-lalr-rules 'source-file))
             (table (production-table productions)))
        (let-values (((nullable nullable-index)
                      (compute-nullable productions)))
          (let-values (((first first-index)
                        (compute-first productions nullable-index)))
            (check
             (call-with-values
              (lambda ()
                (build-states-via-canonical-lr1
                 productions table first-index nullable-index 1))
              list)
             => '(#f))))))
    (poo-flow-test-case "an LALR grammar remains admitted on both paths"
      (let ((lalr (compile-lr-spec shared-lookahead-rules 'source-file))
            (canonical
             (compile-lr-spec shared-lookahead-rules 'source-file
                              'reject #f 'canonical-lr1))
            (direct
             (compile-lr-spec shared-lookahead-rules 'source-file
                              'reject #f 'follow-partition-lr1)))
        (check (>= (lr-spec-ref canonical 'state-count)
                   (lr-spec-ref lalr 'state-count))
               => #t)
        (check (> (lr-spec-ref canonical 'output-item-count) 0)
               => #t)
        (check (> (lr-spec-ref direct 'state-count) 0) => #t)
        (check (lr-spec-ref direct 'follow-block-count) => 0)
        (for-each
         (lambda (first-token)
           (for-each
            (lambda (second-token)
              (for-each
               (lambda (third-token)
                 (let* ((text
                         (string-append first-token second-token third-token))
                        (expected (parse-result canonical text)))
                   (check (equal? (parse-result lalr text) expected) => #t)
                   (check (equal? (parse-result direct text) expected) => #t)))
               '("c" "d")))
            '("c" "d")))
         '("c" "d"))
        (let (partitioned
              (compile-lr-spec shared-lookahead-rules 'source-file
                               'reject #f 'partitioned-lr1))
          (check (<= (lr-spec-ref partitioned 'state-count)
                     (lr-spec-ref canonical 'state-count))
                 => #t)
          (check (lr-spec-ref partitioned 'state-count)
                 => (lr-spec-ref lalr 'state-count)))))
    (poo-flow-test-case "canonical and partitioned parsers accept the same fixtures"
      (let ((canonical
             (compile-lr-spec lr1-not-lalr-rules 'source-file
                              'reject #f 'canonical-lr1))
            (partitioned
             (compile-lr-spec lr1-not-lalr-rules 'source-file
                              'reject #f 'partitioned-lr1))
            (direct
             (compile-lr-spec lr1-not-lalr-rules 'source-file
                              'reject #f 'follow-partition-lr1)))
        (for-each
         (lambda (text)
           (let-values (((canonical-root canonical-rest)
                         (lr-parse canonical (character-tokens text)))
                        ((partitioned-root partitioned-rest)
                         (lr-parse partitioned (character-tokens text)))
                        ((direct-root direct-rest)
                         (lr-parse direct (character-tokens text))))
             (check canonical-rest => '())
             (check partitioned-rest => '())
             (check direct-rest => '())
             (check (equal? canonical-root partitioned-root) => #t)
             (check (equal? canonical-root direct-root) => #t)))
         '("acd" "ace" "bcd" "bce"))
        (check-exception
         (lr-parse canonical (character-tokens "acc")) true)
        (check-exception
         (lr-parse partitioned (character-tokens "acc")) true)
        (check-exception
         (lr-parse direct (character-tokens "acc")) true)
        (let ((alphabet '("a" "b" "c" "d" "e"))
              (accepted 0))
          (for-each
           (lambda (first)
             (for-each
              (lambda (second)
                (for-each
                 (lambda (third)
                   (let* ((text (string-append first second third))
                          (left (parse-result canonical text))
                          (right (parse-result partitioned text))
                          (via-follow (parse-result direct text)))
                     (unless (eq? left 'rejected)
                       (set! accepted (+ accepted 1)))
                     (check (equal? left right) => #t)
                     (check (equal? left via-follow) => #t)))
                 alphabet))
              alphabet))
           alphabet)
          (check accepted => 4))))
    (poo-flow-test-case "precedence grammar keeps canonical parse products"
      (let ((canonical
             (compile-lr-spec precedence-expression-rules 'source-file
                              'reject #f 'canonical-lr1))
            (direct
             (compile-lr-spec precedence-expression-rules 'source-file
                              'reject #f 'follow-partition-lr1)))
        (check (> (lr-spec-ref direct 'follow-block-count) 0) => #t)
        (for-each
         (lambda (text)
           (check (equal? (parse-result canonical text)
                          (parse-result direct text))
                  => #t))
         '("a" "a+a" "a*a" "a+a*a" "a*a+a" "a+a+a" "a*a*a"))))
    (poo-flow-test-case "mixed contexts retain LR(1) recognition"
      (let ((canonical
             (compile-lr-spec mixed-context-rules 'source-file
                              'reject #f 'canonical-lr1))
            (direct
             (compile-lr-spec mixed-context-rules 'source-file
                              'reject #f 'follow-partition-lr1)))
        (check (> (lr-spec-ref direct 'follow-block-count) 0) => #t)
        (check (<= (lr-spec-ref direct 'state-count)
                   (lr-spec-ref canonical 'state-count)) => #t)
        (for-each
         (lambda (text)
           (check (equal? (parse-result canonical text)
                          (parse-result direct text))
                  => #t))
         '("acd" "ace" "bcd" "bce" "dd" "cdd" "cdcd" "acc" "ccc"))
        (for-each
         (lambda (length)
           (check-parse-equivalence
            canonical direct '("a" "b" "c" "d" "e") length))
         '(2 3 4))))))

(export canonical-lr1-reference-test)
