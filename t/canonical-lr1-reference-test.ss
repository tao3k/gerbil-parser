#!/usr/bin/env gxi
(import :gerbil-parser/language-test-support)
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
                 build-lr0-automaton make-item-layout make-core-symbol-catalog
                 core-item-production-id core-item-dot core-item-count)
        (only-in :gerbil-parser/src/compiler/lr-compiler compile-lr-spec)
        (only-in :gerbil-parser/src/compiler/lr-conflict-candidates
                 forward-reachable-follow-masks
                 initial-backward-follow-partitions
                 lr0-conflict-candidates raw-conflict-cells)
        (only-in :gerbil-parser/src/compiler/lr-lookahead
                 build-states-via-canonical-lr1 build-states-via-lr0)
        (only-in :gerbil-parser/src/runtime/lr-parser
                 lr-parse lr-prepare lr-parse/prepared lr-rejection-condition?)
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

(import (only-in :gerbil-parser/src/compiler/lr-partition refine-canonical-partition build-states-via-partitioned-lr1)
        (only-in :std/vector/extensible ExtensibleVector-fill-pointer ExtensibleVector-ref))

(defparser-check (rooted-compiler-representation-case)
(locals ((make-reader (lambda ()
    (let ((table (make-table test: equal? hash: (lambda (key) 0)))
          (width (integer-length (##greatest-fixnum))))
      (table-set! table '(sentinel) 0)
      (for-each
       (lambda (row)
         (let* ((mask (bitwise-ior (- (arithmetic-shift 1 (+ width 5)) 1)
                                  (arithmetic-shift 1 (+ 1024 row))))
                (payload (vector row mask #f)))
           (vector-set! payload 2 payload)
           (table-set! table (list 'state row) payload)))
       (iota 17))
      (lambda (key (replacement #f))
        (when replacement (table-set! table key replacement))
        (table-ref table key #f))))))
  ;; The closure is the only retained owner of this strong, deliberately
  ;; colliding table; every payload includes a cycle and a wide immutable mask.

  (scope ((read (make-reader)) (width (integer-length (##greatest-fixnum))))
    (each (row) ('(0 8 16))
       (act (##gc))
       (equal (read (list 'sentinel))  0)
       (equal (read (list 'missing row))  #f)
       (scope ((payload (read (list 'state row))) (visited '()))
         (equal (vector-ref payload 0)  row)
         (equal (eq? (vector-ref payload 2) payload)  #t)
         (act (compiler-index-set-for-each (vector-ref payload 1)
           (lambda (bit)
             (when (zero? bit) (##gc))
             (set! visited (cons bit visited)))))
         (equal (reverse visited)  (append (iota (+ width 5)) (list (+ 1024 row))))
         (equal (eq? (read (list 'state row)) payload)  #t)
         ;; Replace through a fresh equal key, force GC, then verify both the
         ;; replacement and a different colliding entry, including zero.
         (scope ((replacement (vector (+ row 17) (vector-ref payload 1) #f))
               (neighbor 1))
           (act (vector-set! replacement 2 replacement))
           (act (read (list 'state row) replacement))
           (act (read (list 'sentinel) 0))
           (act (##gc))
           (equal (eq? (read (list 'state row)) replacement)  #t)
           (equal (vector-ref (read (list 'state neighbor)) 0)  neighbor)
           (equal (read (list 'sentinel))  0)))))))

(defparser-check (physical-table-probe-case)
  (each (generic?) ('(#t #f))
     (sequential ((table (if generic?
                    (make-table size: 3 test: equal? hash: (lambda (key) -17))
                    (make-table size: 3 test: eq?)))
            (keys (list->vector (map (lambda (index) (vector index)) (iota 192))))
            (zero (vector 'zero)))
(locals ((key (lambda ( index)
         (if generic? (vector index) (vector-ref keys index)))))

       (act (table-set! table zero 0))
       (each (index) ((iota 96)) (act (table-set! table (key index) index)))
       ;; DELETED must continue probing; growth/GC rehash must retain the other
       ;; equality classes, including a legitimate zero value.
       (each (index) ((iota 48)) (act (table-set! table (key (* 2 index)))))
       (act (##gc))
       (each (index) ((iota 96))
         (equal (table-ref table (key index) 'absent)  (if (even? index) 'absent index)))
       (each (index) ((iota 96)) (act (table-set! table (key (+ 96 index)) (+ 96 index))))
       (act (##gc))
       (equal (table-ref table zero)  0)
       (each (index) ((iota 96))
         (equal (table-ref table (key (+ 96 index)))  (+ 96 index)))))))

(defparser-check (canonical-interning-domain-case)
  (each (rules) ((list shared-lookahead-rules lr1-not-lalr-rules (lr1-context-family-rules 4)))
     (sequential ((productions (lower-rules rules 'source-file)) (table (production-table productions)))
       (scope-values (((_nullable nullable) (compute-nullable productions)))
         (scope-values (((_first first) (compute-first productions nullable)))
           (scope-values (((states count masks offsets edges terminals layout _symbols visits _items)
                         (build-states-via-canonical-lr1 productions table first nullable)))
             (scope ((keys (make-table test: equal?)) (edge-keys (make-table test: equal?)))
               (equal visits  count)
               (each (state) ((iota count))
                 (scope ((dense (make-vector (core-item-count layout) 0)) (actual '())
                       (position (vector-ref offsets state)))
                   (each (core) ((vector-ref states state))
                     (scope ((mask (vector-ref masks position)))
                       (equal (> mask 0)  #t)
                       (equal (< mask (arithmetic-shift 1 (vector-length terminals)))  #t)
                       (equal (vector-ref dense core)  0)
                       (act (vector-set! dense core mask))
                       (act (set! actual (cons (cons core mask) actual)))
                       (act (set! position (+ position 1)))))
                   (scope ((normalized (filter-map (lambda (core)
                                     (let (mask (vector-ref dense core))
                                       (and (> mask 0) (cons core mask))))
                                   (iota (vector-length dense)))))
                     (equal (reverse actual)  normalized)
                     (equal (table-ref keys normalized #f)  #f)
                     (act (table-set! keys normalized state))
                     (equal (table-ref keys normalized #f)  state))))
               (each (edge) (edges)
                 (scope ((key (list (car edge) (cadr edge))))
                   (equal (table-ref edge-keys key #f)  #f)
                   (act (table-set! edge-keys key #t))
                   (equal (< (car edge) count)  #t)
                   (equal (< (caddr edge) count)  #t)))
               (act (##gc))
               (equal (table-length keys)  count))))))))

(defparser-check (partition-termination-bound-case)
  ;; This is an operation bound, not a timing assertion. The real dirty loop
  ;; must recurse only while the dense group count grows below the node count.
  (each (nodes) ('(0 1 2 3 8 16))
     (scope ((rounds 0) (observed '()) (initial (if (= nodes 0) 0 1)))
       (scope-values (((ids groups)
                     (compiler-index-partition-refine
                      (make-vector nodes 0) initial
                      (lambda (node ids) (if (= node 0) '() (list (vector-ref ids (- node 1)))))
                      (lambda (source visit) (when (< (+ source 1) nodes) (visit (+ source 1))))
                      (parser-check-procedure (parser-check (groups count examined)
                        (act (set! rounds (+ rounds 1)))
                        (act (set! observed (cons groups observed)))
                        (equal count  nodes)
                        (equal (<= examined nodes)  #t))))))
         (equal groups  nodes)
         (equal (<= rounds (+ 1 (- nodes initial)))  #t)
         (each (node) ((iota nodes)) (equal (vector-ref ids node)  node))))))

(defparser-check (lookahead-delivery-bound-case)
  ;; Every native dequeue has a nonempty fresh pending mask. At most one such
  ;; delivery can be charged to each (physical item node, terminal) bit.
  (each (rules) ((list shared-lookahead-rules lr1-not-lalr-rules (lr1-context-family-rules 8)))
     (sequential ((productions (lower-rules rules 'source-file)) (table (production-table productions)))
       (scope-values (((_nullable nullable) (compute-nullable productions)))
         (scope-values (((_first first) (compute-first productions nullable)))
           (scope-values (((_states count masks offsets _edges terminals _layout _symbols _lr0-visits visits)
                         (build-states-via-lr0 productions table first nullable)))
             (equal (vector-ref offsets count)  (vector-length masks))
             (equal (> visits 0)  #t)
             (equal (<= visits (* (vector-length masks) (vector-length terminals)))  #t)))))))

(defparser-check (quotient-lookahead-storage-case)
  (each (rules) ((list shared-lookahead-rules lr1-not-lalr-rules (lr1-context-family-rules 4)))
    (sequential ((productions (lower-rules rules 'source-file)) (table (production-table productions)))
      (scope-values (((_nullable nullable) (compute-nullable productions)))
        (scope-values (((_first first) (compute-first productions nullable)))
          (scope-values (((states count masks offsets edges terminals _layout _symbols _visits _items)
                        (build-states-via-canonical-lr1 productions table first nullable))
                       ((merged group-count merged-masks merged-offsets merged-edges merged-terminals
                                _merged-layout _merged-symbols _merged-visits _merged-items)
                        (build-states-via-partitioned-lr1 productions table first nullable)))
            (scope ((owners (make-vector count #f))
                  (expected (make-vector (vector-length merged-masks) 0)))
              ;; Recover the quotient map from paired reachable transitions,
              ;; independently of the implementation's interning/group IDs.
              (iterate visit ((pending '((0 . 0))))
                (unless (null? pending)
                  (sequential ((pair (car pending)) (state (car pair)) (group (cdr pair))
                         (known (vector-ref owners state)))
                    (branch known
                      (group (equal known  group) (continue visit (cdr pending)))
                      (group
                        (act (vector-set! owners state group))
                        (equal (vector-ref states state)  (vector-ref merged group))
                        (scope ((next (cdr pending)))
                          (each (edge) (edges)
                            (when (= (car edge) state)
                              (scope ((target (find (lambda (candidate)
                                             (and (= (car candidate) group)
                                                  (equal? (cadr candidate) (cadr edge)))) merged-edges)))
                                (equal (not (not target))  #t)
                                (act (set! next (cons (cons (caddr edge) (caddr target)) next))))))
                          (continue visit next)))))))
              (equal terminals  merged-terminals)
              (each (state) ((iota count))
                (scope ((group (vector-ref owners state)))
                  (equal (number? group)  #t)
                  (each (position) ((iota (length (vector-ref states state))))
                    (scope ((target (+ (vector-ref merged-offsets group) position)))
                      (act (vector-set! expected target
                        (compiler-index-set-union (vector-ref expected target)
                          (vector-ref masks (+ (vector-ref offsets state) position)))))))))
              (equal expected  merged-masks)
              (equal (vector-ref merged-offsets group-count)  (vector-length merged-masks)))))))))

(defparser-check (dirty-successor-composition-case)
(locals ((refine (lambda ( deliver?)
    (compiler-index-partition-refine
     '#(0 0 0 0) 1
     (lambda (node ids) (if (= node 0) '() (list (vector-ref ids (- node 1)))))
     (lambda (source visit) (when (and deliver? (< source 3)) (visit (+ source 1))))
     (lambda (groups nodes examined) (void))))))
  ;; A predecessor chain needs successive dirty rounds. Omitting successor
  ;; delivery leaves a plausible but incorrect stable partition.

  (scope-values (((correct count) (refine #t))
               ((missed missed-count) (refine #f)))
    (equal correct  '#(0 1 2 3))
    (equal count  4)
    (equal missed  '#(0 1 1 1))
    (equal missed-count  2))))

(defparser-check (refined-conflict-origin-case)
  ;; All three members were initially packable because state 0 witnessed reduction mask
  ;; 56. Transition refinement separates it; states 1 and 2 cannot then retain
  ;; their unwitnessed conflicting union 24 in the remaining bucket.
  (scope ((ids '#(0 0 0 1))
        (rows '#((((terminal literal "x") . 3))
                 (((terminal literal "x") . 0))
                 (((terminal literal "x") . 0)) ()))
        (actions '#(#(56 1) #(8 1) #(16 1) #(0 0))))
    (scope-values (((groups refined)
                   (refine-canonical-partition ids rows actions 4 2)))
      (equal (ExtensibleVector-fill-pointer groups)  4)
      (equal (= (vector-ref refined 1) (vector-ref refined 2))  #f)
      (each (left) ((iota 4))
        (each (right) ((iota 4))
          (when (= (vector-ref refined left) (vector-ref refined right))
            (equal (vector-ref ids left)  (vector-ref ids right))))))))

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

(def (prepared-result-parser spec)
  ;; One fixed grammar/capability basis per sweep; requests still own fresh
  ;; token vectors and LR state, including recovery after a rejected word.
  (let (runtime (lr-prepare spec))
    (lambda (tokens)
      (with-catch
       (lambda (condition)
         (if (lr-rejection-condition? condition) 'rejected (raise condition)))
       (lambda ()
         (let-values (((root rest) (lr-parse/prepared runtime tokens)))
           (unless (null? rest) (error "LR parser left a token suffix" rest))
           root))))))

(defparser-check (check-parse-equivalence canonical other alphabet length)
  (scope ((left (prepared-result-parser canonical)) (right (prepared-result-parser other)))
    (iterate walk ((prefix "") (remaining length))
      (branch (zero? remaining)
        (equal (equal? (left (character-tokens prefix))
                       (right (character-tokens prefix)))
                #t)
        (each (part) (alphabet)
           (continue walk (string-append prefix part) (- remaining 1)))))))

(defparser-check (check-lr0-candidates-cover-canonical rules)
  (sequential ((productions (lower-rules rules 'source-file))
         (table (production-table productions)))
    (scope-values (((nullable nullable-index)
                  (compute-nullable productions)))
      (scope-values (((first first-index)
                    (compute-first productions nullable-index)))
        (scope-values (((lr0-states lr0-count terminals candidates)
                      (lr0-conflict-candidates
                       productions table first-index nullable-index)))
          (scope-values (((canonical-states canonical-count lookaheads offsets
                            transitions canonical-terminals layout core-symbols
                            state-visits item-visits)
                        (build-states-via-canonical-lr1
                         productions table first-index nullable-index)))
            (scope ((index (make-table test: equal?))
                  (canonical-conflicts
                   (raw-conflict-cells
                    canonical-states canonical-count lookaheads offsets
                    canonical-terminals layout core-symbols)))
              (equal (equal? terminals canonical-terminals)  #t)
              (iterate index-loop ((state 0))
                (when (< state lr0-count)
                  (act (table-set! index (vector-ref lr0-states state) state))
                  (continue index-loop (+ state 1))))
              (iterate check-loop ((state 0))
                (when (< state canonical-count)
                  (scope ((lr0-state
                        (table-ref index (vector-ref canonical-states state)
                                   #f)))
                    (equal (number? lr0-state)  #t)
                    (equal
                     (compiler-index-set-difference
                      (vector-ref canonical-conflicts state)
                      (vector-ref candidates lr0-state))
                      0))
                  (continue check-loop (+ state 1))))
              (act (values candidates canonical-conflicts)))))))))

(defparser-check (check-forward-follows-match-lr0 rules)
  (sequential ((productions (lower-rules rules 'source-file))
         (table (production-table productions)))
    (scope-values (((nullable nullable-index)
                  (compute-nullable productions)))
      (scope-values (((first first-index)
                    (compute-first productions nullable-index)))
        (scope-values (((follows follow-terminals)
                      (forward-reachable-follow-masks
                       productions table first-index nullable-index)))
          (scope-values (((states count lookaheads offsets transitions
                            lr0-terminals layout core-symbols
                            state-visits item-visits)
                        (build-states-via-lr0
                         productions table first-index nullable-index)))
            (scope ((completed (make-vector (vector-length table) 0)))
              (equal (equal? follow-terminals lr0-terminals)  #t)
              (iterate state-loop ((state 0))
                (when (< state count)
                  (scope ((node (vector-ref offsets state)))
                    (each (core) ((vector-ref states state))
                       (unless (vector-ref core-symbols core)
                         (scope ((production-id (core-item-production-id core layout)))
                           (act (vector-set!
                            completed production-id
                            (compiler-index-set-union
                             (vector-ref completed production-id)
                             (vector-ref lookaheads node))))))
                       (act (set! node (+ node 1)))))
                  (continue state-loop (+ state 1))))
              (iterate production-loop ((id 0))
                (when (< id (vector-length table))
                  (equal (vector-ref follows id)
                          (vector-ref completed id))
                  (continue production-loop (+ id 1)))))))))))

(defparser-check (check-initial-backward-partitions rules)
  (sequential ((productions (lower-rules rules 'source-file))
         (table (production-table productions)))
    (scope-values (((nullable nullable-index)
                  (compute-nullable productions)))
      (scope-values (((first first-index)
                    (compute-first productions nullable-index)))
        (scope-values (((follows terminals)
                      (forward-reachable-follow-masks
                       productions table first-index nullable-index)))
          (scope-values (((partitions candidates partition-terminals)
                        (initial-backward-follow-partitions
                         productions table first-index nullable-index)))
            (equal (equal? terminals partition-terminals)  #t)
            ;; Walk actual RHS intervals independently of the layout decoder.
            ;; A constant stride inferred from vector size hides padding.
            (iterate production-loop ((id 0) (start 0))
              (when (< id (vector-length table))
                (iterate dot-loop ((dot 0))
                  (when (<= dot (length
                                 (production-rhs (vector-ref table id))))
                    (scope ((blocks (vector-ref partitions (+ start dot))) (seen 0))
                      (each (block) (blocks)
                        (equal (> (cdr block) 0)  #t)
                        (equal (bitwise-and seen (cdr block))  0)
                        (act (set! seen (compiler-index-set-union seen (cdr block)))))
                      (equal
                       (foldl (lambda (block mask)
                                (compiler-index-set-union mask (cdr block)))
                              0 blocks)
                        (vector-ref follows id)))
                    (continue dot-loop (+ dot 1))))
                (continue production-loop (+ id 1) (+ start 1 (length (production-rhs (vector-ref table id)))))))
            (result partitions)))))))

;; Kernel identity is independent of closure construction and key encoding.
;; Check its graph invariants on recursive, nullable, and shared contexts.
(defparser-check (check-lr0-kernel-identity rules)
  (sequential ((productions (lower-rules rules 'source-file))
         (table (production-table productions)))
    (scope-values (((terminals terminal-index)
                  (production-terminal-catalog productions)))
      (sequential ((layout (make-item-layout table terminals))
             (symbols (make-core-symbol-catalog table layout)))
        (scope-values (((states transitions count by-lhs visits)
                      (build-lr0-automaton productions table layout symbols)))
          (scope ((kernels (make-vector count '()))
                (seen (make-table test: equal?)))
            (equal visits  count)
            (iterate index ((state 0))
              (when (< state count)
                (scope ((kernel
                      (filter (lambda (item)
                                (not (zero? (core-item-dot item layout))))
                              (vector-ref states state))))
                  (act (vector-set! kernels state kernel))
                  (branch (zero? state)
                    (equal kernel  '())
                    (group
                      (equal (pair? kernel)  #t)
                      (equal (table-ref seen kernel #f)  #f)
                      (act (table-set! seen kernel state)))))
                (continue index (+ state 1))))
            (iterate edges ((state 0))
              (when (< state count)
                ;; Every after-dot symbol must be published exactly once, in
                ;; sorted-item discovery order, including shared predictions.
                (scope ((expected '()) (symbols-seen (make-table test: equal?)))
                  (each (item) ((vector-ref states state))
                     (scope ((symbol (vector-ref symbols item)))
                       (when (and symbol (not (table-ref symbols-seen symbol #f)))
                         (act (table-set! symbols-seen symbol #t))
                         (act (set! expected (cons symbol expected))))))
                  (equal (map car (vector-ref transitions state))  (reverse expected)))
                (each (edge) ((vector-ref transitions state))
                   (sequential ((symbol (car edge)) (target (cdr edge))
                          (expected
                           (map (lambda (item) (+ item 1))
                                (filter (lambda (item)
                                          (equal? (vector-ref symbols item) symbol))
                                        (vector-ref states state)))))
                     (equal (> target 0)  #t)
                     (equal (equal? expected (vector-ref kernels target))  #t)))
                (continue edges (+ state 1))))))))))

(deflanguage-parser-tests canonical-lr1-reference-test "canonical LR(1) reference"
  (engine)
    (profiled "physical generic and eq probes retain tombstones, zero and rehashed entries"
      (verify physical-table-probe-case ))
    (profiled "closure roots retain colliding tables, cycles and wide mask callbacks across GC"
      (verify rooted-compiler-representation-case ))
    (profiled "native canonical keys inhabit their finite matrix catalogue and intern once"
      (verify canonical-interning-domain-case ))
    (profiled "native partition rounds satisfy the finite dense-group bound"
      (verify partition-termination-bound-case ))
    (profiled "native lookahead delivery stays within its finite item-terminal domain"
      (verify lookahead-delivery-bound-case ))
    (profiled "quotient lookaheads preserve physical core positions and exact member unions"
      (verify quotient-lookahead-storage-case ))
    (profiled "dirty successor delivery composes across predecessor rounds"
      (verify dirty-successor-composition-case ))
    (profiled "transition splits retain a surviving canonical conflict witness"
      (verify refined-conflict-origin-case ))
    (profiled "prepared corpus readers preserve fresh rejection and recovery"
      (sequential ((spec (compile-lr-spec lr1-not-lalr-rules 'source-file 'reject #f 'canonical-lr1))
             (parse (prepared-result-parser spec)))
        (equal (eq? (parse-result spec "acd") 'rejected)  #f)
        (each (text) ('("" "acd" "bce" "bad" "ace" "bcd" "acd" ""))
            (equal (parse (character-tokens text))  (parse-result spec text)))))
    (profiled "affected partition refinement matches exhaustive synchronous oracle"
(locals ((signature (lambda ( predecessors node ids)
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
         '(0 2))))
(oracle (lambda ( initial initial-count predecessors record)
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
                (refine next next-count))))))
(compare (parser-check ( predecessors initial initial-count)
        (scope ((saved (vector->list initial)) (expected-calls 0) (actual-calls 0))
          (scope-values (((expected expected-count)
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
            ;; One graph comparison carries all four facts. Keep full vector,
            ;; count, input immutability and examined-work checks, without four
            ;; separately synchronized progress transactions per tiny graph.
            (equal (list actual actual-count (vector->list initial)
                         (<= actual-calls expected-calls))
                    (list expected expected-count saved #t))
            (act (values actual-calls expected-calls)))))))


      (iterate graphs ((graph 0))
        (when (< graph 512)
          (scope ((predecessors (make-vector 3 '())))
            (each (bit) ((iota 9))
                        (when (not (zero? (bitwise-and graph (arithmetic-shift 1 bit))))
                          (scope ((target (modulo bit 3)) (source (quotient bit 3)))
                            (act (vector-set! predecessors target
                                         (cons (cons 0 source) (vector-ref predecessors target)))))))
            (each (entry) ((list (cons '#(0 0 0) 1) (cons '#(0 0 1) 2)
                            (cons '#(0 1 0) 2) (cons '#(0 1 1) 2) (cons '#(0 1 2) 3))) (verify compare predecessors (car entry) (cdr entry))))
          (continue graphs (+ graph 1))))
      (iterate graphs ((graph 0))
        (when (< graph 64)
          (scope ((predecessors (make-vector 3 '())))
            (each (bit) ((iota 6))
                        (when (not (zero? (bitwise-and graph (arithmetic-shift 1 bit))))
                          (sequential ((source (modulo bit 3)) (target (modulo (+ source 1) 3))
                                 (edge (cons (if (< bit 3) 0 2) source)))
                            (act (vector-set! predecessors target
                                         (cons edge (cons edge (vector-ref predecessors target))))))))
            (verify compare predecessors '#(0 0 0) 1)
            (verify compare predecessors '#(0 0 1) 2))
          (continue graphs (+ graph 1))))
      ;; A split propagates along the chain while the independent self-loop
      ;; group remains stable, despite changes to its numeric group identity.
      (scope ((predecessors '#(() ((0 . 0)) ((0 . 1)) ((0 . 2)) ((0 . 3)) ((0 . 4)) ((2 . 6)) ((2 . 7)))))
        (scope-values (((actual expected) (parser-check-result compare predecessors '#(0 1 1 1 1 1 2 2) 3)))
          (equal actual  16)
          (equal expected  24)
          (equal (< actual expected)  #t)))
      ;; Sizes belong to group identities, not node order. Exercise dense IDs
      ;; that are initially permuted before first-seen canonical publication.
      (verify compare '#(() ((0 . 0)) ((2 . 1))) '#(1 0 0) 2)
      (verify compare '#(() ((0 . 0)) ((2 . 1))) '#(2 0 1) 3)
      (verify compare '#() '#() 0)
      ;; Larger interacting groups exercise retained/new identities and both
      ;; member/worklist orders. Reverse the entire graph, not just its IDs;
      ;; every result is compared with the independent synchronous oracle.
      (iterate graphs ((graph 0))
        (when (< graph 64)
          (scope ((predecessors (make-vector 6 '()))
                (reversed (make-vector 6 '())))
            (each (source) ((iota 6))
               (sequential ((target (modulo (+ source 1) 6))
                      (label (if (zero? (bitwise-and graph (arithmetic-shift 1 source))) 0 2))
                      (edge (cons label source)))
                 (act (vector-set! predecessors target
                              (cons edge (cons edge (vector-ref predecessors target)))))
                 (when (zero? (modulo source 2))
                   (scope ((target (modulo (+ source 3) 6)))
                     (act (vector-set! predecessors target
                                  (cons (cons 2 source) (vector-ref predecessors target))))))
                 (when (not (zero? (bitwise-and graph (arithmetic-shift 1 source))))
                   (act (vector-set! predecessors source
                                (cons (cons 0 source) (vector-ref predecessors source)))))))
            (each (node) ((iota 6))
               (act (vector-set! reversed (- 5 node)
                            (map (lambda (edge) (cons (car edge) (- 5 (cdr edge))))
                                 (vector-ref predecessors node)))))
            (each (entry) ((list (cons '#(0 0 0 0 0 0) 1) (cons '#(1 0 0 1 0 0) 2)
                   (cons '#(2 0 1 2 0 1) 3) (cons '#(0 1 1 1 1 1) 2)))
               (verify compare predecessors (car entry) (cdr entry))
               (verify compare reversed (list->vector (reverse (vector->list (car entry)))) (cdr entry))))
          (continue graphs (+ graph 1))))
      ;; Workspaces must remain local to an invocation. A result produced
      ;; after multiple rounds survives a separate refinement call.
      (scope ((initial '#(0 0 0)) (rounds 0))
        (scope-values (((first groups)
                      (compiler-index-partition-refine
                       initial 1 (lambda (node ids) (if (= node 0) 0 1))
                       (lambda (source visit) (void))
                       (lambda (groups nodes examined) (set! rounds (+ rounds 1))))))
          (equal first  '#(0 1 1))
          (equal groups  2)
          (equal rounds  2)
          (scope-values (((second groups)
                        (compiler-index-partition-refine
                         initial 1 (lambda (node ids) node)
                         (lambda (source visit) (void))
                         (lambda (groups nodes examined) (void)))))
            (equal second  '#(0 1 2))
            (equal groups  3)
            (equal (eq? first second)  #f))
          (equal first  '#(0 1 1))
          (equal initial  '#(0 0 0))))))
    (profiled "indexed reachability agrees with exhaustive list graph oracle"
(locals ((oracle (lambda ( initial rows)
        (let visit ((pending (filter (lambda (node) (not (zero? (bitwise-and initial (arithmetic-shift 1 node))))) (iota (vector-length rows))))
                    (seen '()))
          (if (null? pending)
            (foldl (lambda (node mask) (bitwise-ior mask (arithmetic-shift 1 node))) 0 seen)
            (let (node (car pending))
              (if (memv node seen)
                (visit (cdr pending) seen)
                (visit (append (vector-ref rows node) (cdr pending)) (cons node seen)))))))))

      (iterate graphs ((graph 0))
        (when (< graph 512)
          (scope ((edges (make-vector 3 0)) (rows (make-vector 3 '())))
            (each (node) ('(0 1 2))
              (scope ((mask (bitwise-and 7 (arithmetic-shift graph (* -3 node)))))
                (act (vector-set! edges node mask))
                (act (vector-set! rows node (filter (lambda (target) (not (zero? (bitwise-and mask (arithmetic-shift 1 target))))) '(0 1 2))))))
            (scope ((cache (make-vector 3 #f)))
              (each (initial) ((iota 8))
                 (scope ((expected (oracle initial rows)))
                   (equal (compiler-index-set-reachable initial edges)  expected)
                   (equal (compiler-index-set-reachable/memo initial edges (make-vector 3 #f))  expected)
                   (equal (compiler-index-set-reachable/memo initial edges cache)  expected)))
              (each (node) ((iota 3))
                 (scope ((closure (vector-ref cache node)))
                   (when closure (equal closure  (oracle (arithmetic-shift 1 node) rows)))))))
          (continue graphs (+ graph 1))))
      (equal (compiler-index-set-reachable 0 '#())  0)
      (equal (compiler-index-set-reachable/memo 0 '#() '#())  0)
      (scope ((chain (make-vector 1024 0)) (cycle (make-vector 1024 0)))
        (iterate nodes ((node 0))
          (when (< node 1024)
            (when (< node 1023) (act (vector-set! chain node (arithmetic-shift 1 (+ node 1)))))
            (act (vector-set! cycle node (arithmetic-shift 1 (modulo (+ node 1) 1024))))
            (continue nodes (+ node 1))))
        (equal (compiler-index-set-reachable 1 chain)  (- (arithmetic-shift 1 1024) 1))
        (equal (compiler-index-set-reachable (arithmetic-shift 1 512) cycle)  (- (arithmetic-shift 1 1024) 1))
        ;; High-bit, overlapping roots preserve exact cold/warm results and
        ;; graph-specific cache ownership across independent calls.
        (scope ((cache (make-vector 1024 #f))
              (roots (bitwise-ior 1 (arithmetic-shift 1 512) (arithmetic-shift 1 1023)))
              (all (- (arithmetic-shift 1 1024) 1)))
          (equal (compiler-index-set-reachable/memo roots chain cache)  all)
          (equal (vector-ref cache 0)  all)
          (equal (compiler-index-set-reachable/memo roots chain cache)  all)
          (equal (compiler-index-set-reachable/memo roots cycle (make-vector 1024 #f))  all)))))
    (profiled "indexed traversal agrees with bit membership across fields"
(locals ((check-mask (parser-check ( mask)
        (scope ((actual '()) (expected '()))
          (act (compiler-index-set-for-each mask (lambda (index) (set! actual (cons index actual)))))
          (iterate bits ((index 0))
            (when (< index (integer-length mask))
              (when (not (zero? (bitwise-and mask (arithmetic-shift 1 index)))) (act (set! expected (cons index expected))))
              (continue bits (+ index 1))))
          (equal actual  expected)))))

      (iterate masks ((mask 0))
        (when (< mask 4096) (verify check-mask mask) (continue masks (+ mask 1))))
      (scope ((width (integer-length (##greatest-fixnum))))
        (each (index) ([(- width 1) width (+ width 1) (* width 2) 1024 4096])
           (scope ((bit (arithmetic-shift 1 index)))
             (verify check-mask bit)
             (verify check-mask (- bit 1))
             (verify check-mask (bitwise-ior bit 1 (arithmetic-shift 1 (quotient index 2)))))))
      ;; Reentrant traversal must not disturb the outer field or order.
      (scope ((outer '()) (inner '()))
        (act (compiler-index-set-for-each (bitwise-ior 1 (arithmetic-shift 1 1024))
          (lambda (index)
            (set! outer (cons index outer))
            (compiler-index-set-for-each 5 (lambda (bit) (set! inner (cons bit inner)))))))
        (equal outer  '(1024 0))
        (equal inner  '(2 0 2 0)))
      (scope ((outer '()) (nested-count 0))
        (act (compiler-index-set-for-each (- (arithmetic-shift 1 128) 1)
          (lambda (index)
            (set! outer (cons index outer))
            (compiler-index-set-for-each 5 (lambda (_) (set! nested-count (+ nested-count 1)))))))
        (equal outer  (reverse (iota 128)))
        (equal nested-count  256))
      (scope ((calls 0) (sentinel (list 'callback-failure)))
        (equal (with-catch identity
                 (lambda ()
                   (compiler-index-set-for-each (- (arithmetic-shift 1 128) 1)
                     (lambda (_) (set! calls (+ calls 1)) (raise sentinel)))))
                sentinel)
        (equal calls  1))))
    (profiled "LR(0) kernels uniquely identify states and goto targets"
      (verify check-lr0-kernel-identity precedence-expression-rules)
      (verify check-lr0-kernel-identity inactive-core-conflict-rules)
      (verify check-lr0-kernel-identity (mixed-context-family-rules 16))
      (verify check-lr0-kernel-identity (state-local-candidate-family-rules 16))
      (verify check-lr0-kernel-identity (lr1-context-family-rules 16)))
    (profiled "forward reachable follows agree with LR(0) propagation"
      (verify check-forward-follows-match-lr0 lr1-not-lalr-rules)
      (verify check-forward-follows-match-lr0 shared-lookahead-rules)
      (verify check-forward-follows-match-lr0 genuine-reduce-conflict-rules)
      (verify check-forward-follows-match-lr0 mixed-context-rules)
      (verify check-forward-follows-match-lr0 (lr1-context-family-rules 16)))
    (profiled "initial backward partition preserves all follow strings"
      (scope ((partitions
            (parser-check-result check-initial-backward-partitions lr1-not-lalr-rules)))
        (equal (vector-any (lambda (blocks)
                             (and blocks (> (length blocks) 1)))
                           partitions)
                #t))
      (verify check-initial-backward-partitions shared-lookahead-rules)
      (verify check-initial-backward-partitions genuine-reduce-conflict-rules))
    (profiled "initial partition skips cores without a conflict action"
      (scope ((blocks (vector->list
                   (parser-check-result check-initial-backward-partitions inactive-core-conflict-rules))))
        (equal (foldl (lambda (entry total)
                        (+ total (if entry (length entry) 0)))
                      0 blocks)
                41)
        (equal (foldl (lambda (entry total)
                        (+ total (if (and entry (> (length entry) 1)) 1 0)))
                      0 blocks)
                3)))
    (profiled "LR(0) raw candidates conservatively cover canonical conflicts"
      (scope-values (((candidates canonical-conflicts)
                    (parser-check-result check-lr0-candidates-cover-canonical lr1-not-lalr-rules)))
        (equal (vector-any (lambda (mask) (not (zero? mask))) candidates)
                #t)
        (equal (vector-any (lambda (mask) (not (zero? mask)))
                           canonical-conflicts)
                #f))
      (verify check-lr0-candidates-cover-canonical shared-lookahead-rules)
      (verify check-lr0-candidates-cover-canonical inactive-core-conflict-rules)
      (scope-values (((candidates canonical-conflicts)
                    (parser-check-result check-lr0-candidates-cover-canonical genuine-reduce-conflict-rules)))
        (equal (vector-any (lambda (mask) (not (zero? mask))) candidates)
                #t)
        (equal (vector-any (lambda (mask) (not (zero? mask)))
                           canonical-conflicts)
                #t))
      (raises
       (compile-lr-spec genuine-reduce-conflict-rules 'source-file
                        'reject #f 'follow-partition-lr1)
       true))
    (profiled "split contexts remove an LALR reduce/reduce conflict"
      (raises
       (compile-lr-spec lr1-not-lalr-rules 'source-file)
       true)
      (scope ((spec
            (compile-lr-spec lr1-not-lalr-rules 'source-file
                             'reject #f 'canonical-lr1)))
        (equal (lr-spec-ref spec 'algorithm)
                'canonical-lr1-reference-v1)
        (equal (> (lr-spec-ref spec 'state-count) 0)  #t))
      (scope ((canonical
             (compile-lr-spec lr1-not-lalr-rules 'source-file
                              'reject #f 'canonical-lr1))
            (partitioned
             (compile-lr-spec lr1-not-lalr-rules 'source-file
                              'reject #f 'partitioned-lr1))
            (direct
             (compile-lr-spec lr1-not-lalr-rules 'source-file
                              'reject #f 'follow-partition-lr1)))
        (equal (lr-spec-ref partitioned 'algorithm)
                'conflict-partitioned-lr1-v1)
        (equal (lr-spec-ref direct 'algorithm)
                'follow-partition-lr1-v1)
        (equal (<= (lr-spec-ref partitioned 'state-count)
                   (lr-spec-ref canonical 'state-count))
                #t)
        (equal (> (lr-spec-ref direct 'follow-block-count) 0)  #t)))
    (profiled "prepared sweeps isolate successful and rejected requests"
      (each (route) ('(canonical-lr1 follow-partition-lr1))
         (sequential ((spec (compile-lr-spec lr1-not-lalr-rules 'source-file 'reject #f route))
                (parse (prepared-result-parser spec))
                (first-input (character-tokens "acd"))
                (first-result (parse first-input)))
           (equal (eq? first-result 'rejected)  #f)
           (equal (parse (character-tokens "acc"))  'rejected)
           (each (text) ('("ace" "" "bcd" "acc" "bce" "acd"))
              (scope ((tokens (character-tokens text)))
                (equal (parse tokens)  (parse-token-result spec tokens))))
           ;; A later rejection must not overwrite an earlier returned product.
           (equal first-result  (parse-token-result spec first-input)))))
    (profiled "independent LR(1) contexts preserve parse products"
      (each (context-count) ('(4 16 32 48 64))
         (sequential ((rules (lr1-context-family-rules context-count))
                (canonical
                 (compile-lr-spec rules 'source-file 'reject #f
                                  'canonical-lr1))
                (direct
                 (compile-lr-spec rules 'source-file 'reject #f
                                  'follow-partition-lr1))
                (parse-canonical (prepared-result-parser canonical))
                (parse-direct (prepared-result-parser direct)))
           (raises (compile-lr-spec rules 'source-file) true)
           (equal (lr-spec-ref canonical 'state-count)
                   (lr-spec-ref direct 'state-count))
           (when (memv context-count '(32 48 64))
             (equal (lr-spec-ref direct 'follow-block-count)  0))
           (each (index) ((iota context-count))
              (sequential ((prefix
                      (string-append "region-" (number->string index) ":"))
                     (prefix-length (string-length prefix))
                     (prefix-token
                      (make-token 'punctuation prefix 0 prefix-length)))
                (each (body) ('("acd" "ace" "bcd" "bce"))
                   (sequential ((tokens
                           (cons prefix-token
                                 (character-tokens body prefix-length)))
                          (expected (parse-canonical tokens)))
                     (equal (eq? expected 'rejected)  #f)
                     (equal (equal? expected
                                    (parse-direct tokens))
                             #t)))
                (scope ((tokens
                      (cons prefix-token
                            (character-tokens "acc" prefix-length))))
                  (equal (parse-canonical tokens)  'rejected)
                  (equal (parse-direct tokens)  'rejected)))))))
    (profiled "state-local conflict candidates preserve parse products"
      (sequential ((rules (state-local-candidate-family-rules 32))
             (canonical
              (compile-lr-spec rules 'source-file 'selective-glr #f
                               'canonical-lr1))
             (direct
              (compile-lr-spec rules 'source-file 'selective-glr #f
                               'follow-partition-lr1))
             (parse-canonical (prepared-result-parser canonical))
             (parse-direct (prepared-result-parser direct)))
        (equal (< (lr-spec-ref direct 'state-count)
                  (lr-spec-ref canonical 'state-count))  #t)
        (equal (> (lr-spec-ref direct 'follow-block-count) 0)  #t)
        (each (index) ('(0 15 31))
           (sequential ((prefix
                   (string-append "region-" (number->string index) ":"))
                  (prefix-length (string-length prefix))
                  (prefix-token
                   (make-token 'punctuation prefix 0 prefix-length)))
(locals ((tokens (lambda ( body)
               (cons prefix-token (character-tokens body prefix-length)))))

             (each (body) ('("axd" "axe" "bxxe" "bxe" "axf" "bxxd" ""))
                (scope ((input (tokens body)))
                  (equal (equal? (parse-canonical input)
                                 (parse-direct input))  #t))))))))
    (profiled "mixed context family compresses forward follow blocks"
      (each (context-count) ('(4 16 32 64 128))
         (sequential ((rules (mixed-context-family-rules context-count))
                (canonical
                 (compile-lr-spec rules 'source-file 'reject #f
                                  'canonical-lr1))
                (direct
                 (compile-lr-spec rules 'source-file 'reject #f
                                  'follow-partition-lr1))
                (parse-canonical (prepared-result-parser canonical))
                (parse-direct (prepared-result-parser direct)))
           (equal (< (lr-spec-ref direct 'state-count)
                     (lr-spec-ref canonical 'state-count))  #t)
           (equal (< (lr-spec-ref direct 'follow-block-count)
                     (lr-spec-ref direct 'output-item-count))  #t)
           (each (index) ((iota context-count))
              (sequential ((prefix
                      (string-append "region-" (number->string index) ":"))
                     (prefix-length (string-length prefix))
                     (prefix-token
                      (make-token 'punctuation prefix 0 prefix-length)))
                (each (body) ('("acd" "ace" "bcd" "bce" "dd" "cdd" "acc" "cd"))
                   (sequential ((tokens
                          (cons prefix-token
                                 (character-tokens body prefix-length)))
                          (expected (parse-canonical tokens)))
                     (equal (eq? expected 'rejected)
                             (if (member body '("acc" "cd")) #t #f))
                     (equal (equal? expected
                                    (parse-direct tokens))
                             #t))))))))
    (profiled "FOLLOW stable edges preserve products after production reordering"
(locals ((nullable-tail-family (lambda ( count)
        (let (rules (mixed-context-family-rules count))
          (cons (list 'source-file
                      (list 'sequence (cadar rules) '(optional (literal "z"))))
                (cdr rules))))))
      ;; Reordering productions changes core/block numbering. Compare with
      ;; canonical construction to catch reuse of a previous round's edge IDs,
      ;; including singleton and inactive blocks beside the final split.

      (each (family) ((list mixed-context-family-rules acyclic-mixed-context-family-rules nullable-tail-family))
         (each (count) ('(4 8))
            (scope ((rules (family count)))
              (each (ordered) ((list rules (reverse rules)))
                 (scope ((canonical (compile-lr-spec ordered 'source-file 'reject #f 'canonical-lr1))
                       (direct (compile-lr-spec ordered 'source-file 'reject #f 'follow-partition-lr1)))
                   (equal (> (lr-spec-ref direct 'follow-block-count) 0)  #t)
                   (each (region) ((iota count))
                      (sequential ((prefix (string-append "region-" (number->string region) ":"))
                             (size (string-length prefix)))
                        (each (body) ('("acd" "ace" "bcd" "bce" "cc" "cd" "dc" "dd" "acc" "acdz" "cddz" "accz"))
                           (scope ((tokens (cons (make-token 'punctuation prefix 0 size)
                                             (character-tokens body size))))
                             (equal (parse-token-result direct tokens)
                                     (parse-token-result canonical tokens)))))))))))))
    (profiled "acyclic shared contexts still need follow compression"
      (each (context-count) ('(32 64))
         (sequential ((rules (acyclic-mixed-context-family-rules context-count))
                (canonical
                 (compile-lr-spec rules 'source-file 'reject #f
                                  'canonical-lr1))
                (direct
                 (compile-lr-spec rules 'source-file 'reject #f
                                  'follow-partition-lr1)))
           (equal (< (lr-spec-ref direct 'state-count)
                     (lr-spec-ref canonical 'state-count))  #t)
           (equal (> (lr-spec-ref direct 'follow-block-count) 0)  #t)
           (each (index) ((list 0 15 (- context-count 1)))
              (sequential ((prefix
                      (string-append "region-" (number->string index) ":"))
                     (prefix-length (string-length prefix))
                     (prefix-token
                      (make-token 'punctuation prefix 0 prefix-length)))
                (each (body) ('("acd" "ace" "bcd" "bce" "cc" "cd" "dc" "dd" "acc"))
                   (sequential ((tokens
                           (cons prefix-token
                                 (character-tokens body prefix-length)))
                          (expected (parse-token-result canonical tokens)))
                     (equal (equal? expected
                                    (parse-token-result direct tokens))  #t))))))))
    (profiled "split shared productions do not force compression"
      (each (context-count) ('(16 64))
         (sequential ((rules (split-shared-context-family-rules context-count))
                (canonical
                 (compile-lr-spec rules 'source-file 'reject #f
                                  'canonical-lr1))
                (direct
                 (compile-lr-spec rules 'source-file 'reject #f
                                  'follow-partition-lr1)))
           (equal (lr-spec-ref direct 'state-count)
                   (lr-spec-ref canonical 'state-count))
           (equal (zero? (lr-spec-ref direct 'follow-block-count))
                   (>= context-count 64))
           (each (index) ((list 0 15 (- context-count 1)))
              (sequential ((prefix
                      (string-append "region-" (number->string index) ":"))
                     (prefix-length (string-length prefix))
                     (prefix-token
                      (make-token 'punctuation prefix 0 prefix-length)))
                (each (body) ('("dx" "dy" "cdx" "cdy" "acc"))
                   (sequential ((tokens
                           (cons prefix-token
                                 (character-tokens body prefix-length)))
                          (expected (parse-token-result canonical tokens)))
                     (equal (eq? expected 'rejected)
                             (equal? body "acc"))
                     (equal (equal? expected
                                    (parse-token-result direct tokens))  #t))))))))
    (profiled "unreachable repeated helper keeps canonical route"
      (sequential ((rules (unreachable-repeated-context-family-rules 64))
             (canonical
              (compile-lr-spec rules 'source-file 'reject #f 'canonical-lr1))
             (direct
              (compile-lr-spec rules 'source-file 'reject #f
                               'follow-partition-lr1)))
        (equal (lr-spec-ref direct 'state-count)
                (lr-spec-ref canonical 'state-count))
        (equal (lr-spec-ref direct 'follow-block-count)  0)
        (each (index) ('(0 31 63))
           (sequential ((prefix
                   (string-append "region-" (number->string index) ":"))
                  (prefix-length (string-length prefix))
                  (prefix-token
                   (make-token 'punctuation prefix 0 prefix-length)))
             (each (body) ('("acd" "bce" "acc"))
                (scope ((tokens
                      (cons prefix-token
                            (character-tokens body prefix-length))))
                  (equal (equal? (parse-token-result canonical tokens)
                                 (parse-token-result direct tokens))  #t)))))))
    (profiled "bounded canonical trial stops above its state budget"
      (sequential ((productions (lower-rules lr1-not-lalr-rules 'source-file))
             (table (production-table productions)))
        (scope-values (((nullable nullable-index)
                      (compute-nullable productions)))
          (scope-values (((first first-index)
                        (compute-first productions nullable-index)))
            (equal
             (call-with-values
              (lambda ()
                (build-states-via-canonical-lr1
                 productions table first-index nullable-index 1))
              list)
              '(#f))))))
    (profiled "an LALR grammar remains admitted on both paths"
      (scope ((lalr (compile-lr-spec shared-lookahead-rules 'source-file))
            (canonical
             (compile-lr-spec shared-lookahead-rules 'source-file
                              'reject #f 'canonical-lr1))
            (direct
             (compile-lr-spec shared-lookahead-rules 'source-file
                              'reject #f 'follow-partition-lr1)))
        (equal (>= (lr-spec-ref canonical 'state-count)
                   (lr-spec-ref lalr 'state-count))
                #t)
        (equal (> (lr-spec-ref canonical 'output-item-count) 0)
                #t)
        (equal (> (lr-spec-ref direct 'state-count) 0)  #t)
        (equal (lr-spec-ref direct 'follow-block-count)  0)
        (each (first-token) ('("c" "d"))
           (each (second-token) ('("c" "d"))
              (each (third-token) ('("c" "d"))
                 (sequential ((text
                         (string-append first-token second-token third-token))
                        (expected (parse-result canonical text)))
                   (equal (equal? (parse-result lalr text) expected)  #t)
                   (equal (equal? (parse-result direct text) expected)  #t)))))
        (scope ((partitioned
              (compile-lr-spec shared-lookahead-rules 'source-file
                               'reject #f 'partitioned-lr1)))
          (equal (<= (lr-spec-ref partitioned 'state-count)
                     (lr-spec-ref canonical 'state-count))
                  #t)
          (equal (lr-spec-ref partitioned 'state-count)
                  (lr-spec-ref lalr 'state-count)))))
    (profiled "canonical and partitioned parsers accept the same fixtures"
      (scope ((canonical
             (compile-lr-spec lr1-not-lalr-rules 'source-file
                              'reject #f 'canonical-lr1))
            (partitioned
             (compile-lr-spec lr1-not-lalr-rules 'source-file
                              'reject #f 'partitioned-lr1))
            (direct
             (compile-lr-spec lr1-not-lalr-rules 'source-file
                              'reject #f 'follow-partition-lr1)))
        (each (text) ('("acd" "ace" "bcd" "bce"))
           (scope-values (((canonical-root canonical-rest)
                         (lr-parse canonical (character-tokens text)))
                        ((partitioned-root partitioned-rest)
                         (lr-parse partitioned (character-tokens text)))
                        ((direct-root direct-rest)
                         (lr-parse direct (character-tokens text))))
             (equal canonical-rest  '())
             (equal partitioned-rest  '())
             (equal direct-rest  '())
             (equal (equal? canonical-root partitioned-root)  #t)
             (equal (equal? canonical-root direct-root)  #t)))
        (raises
         (lr-parse canonical (character-tokens "acc")) true)
        (raises
         (lr-parse partitioned (character-tokens "acc")) true)
        (raises
         (lr-parse direct (character-tokens "acc")) true)
        (scope ((alphabet '("a" "b" "c" "d" "e"))
              (accepted 0))
          (each (first) (alphabet)
             (each (second) (alphabet)
                (each (third) (alphabet)
                   (sequential ((text (string-append first second third))
                          (left (parse-result canonical text))
                          (right (parse-result partitioned text))
                          (via-follow (parse-result direct text)))
                     (unless (eq? left 'rejected)
                       (act (set! accepted (+ accepted 1))))
                     (equal (equal? left right)  #t)
                     (equal (equal? left via-follow)  #t)))))
          (equal accepted  4))))
    (profiled "precedence grammar keeps canonical parse products"
      (scope ((canonical
             (compile-lr-spec precedence-expression-rules 'source-file
                              'reject #f 'canonical-lr1))
            (direct
             (compile-lr-spec precedence-expression-rules 'source-file
                              'reject #f 'follow-partition-lr1)))
        (equal (> (lr-spec-ref direct 'follow-block-count) 0)  #t)
        (each (text) ('("a" "a+a" "a*a" "a+a*a" "a*a+a" "a+a+a" "a*a*a"))
           (equal (equal? (parse-result canonical text)
                          (parse-result direct text))
                   #t))))
    (profiled "mixed contexts retain LR(1) recognition"
      (scope ((canonical
             (compile-lr-spec mixed-context-rules 'source-file
                              'reject #f 'canonical-lr1))
            (direct
             (compile-lr-spec mixed-context-rules 'source-file
                              'reject #f 'follow-partition-lr1)))
        (equal (> (lr-spec-ref direct 'follow-block-count) 0)  #t)
        (equal (<= (lr-spec-ref direct 'state-count)
                   (lr-spec-ref canonical 'state-count))  #t)
        (each (text) ('("acd" "ace" "bcd" "bce" "dd" "cdd" "cdcd" "acc" "ccc"))
           (equal (equal? (parse-result canonical text)
                          (parse-result direct text))
                   #t))
        (each (length) ('(2 3 4))
           (verify check-parse-equivalence canonical direct '("a" "b" "c" "d" "e") length)))))
