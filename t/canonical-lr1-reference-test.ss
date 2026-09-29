#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; LR(1)-but-not-LALR(1) grammar checks the state-splitting reference.

(import :std/test
        (only-in :core/observability/testing-case poo-flow-test-case)
        (only-in :gerbil-parser/src/compiler/funcs
                 compiler-index-set-difference compiler-index-set-union)
        (only-in :gerbil-parser/src/compiler/lr
                 compute-first compute-nullable lower-rules lr-spec-ref
                 production-rhs production-table)
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

(def canonical-lr1-reference-test
  (test-suite "canonical LR(1) reference"
    (poo-flow-test-case "forward reachable follows agree with LR(0) propagation"
      (check-forward-follows-match-lr0 lr1-not-lalr-rules)
      (check-forward-follows-match-lr0 shared-lookahead-rules)
      (check-forward-follows-match-lr0 genuine-reduce-conflict-rules))
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
       '(4 16)))
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
