#!/usr/bin/env gxi
(import :std/test
        :gerbil-parser/src/runtime/funcs
        :gerbil-parser/src/runtime/recognition
        :gerbil-parser/src/runtime/reduce
        (only-in :gerbil-parser/src/runtime/token make-token)
        "./scenarios/performance/selective-glr/scenario")
(def recognition-sequence-tests
  (test-suite "ordered semantic sequence publication"
    (test-case "mixed branch views isolate offsets and preserve untouched children"
      (let* ((a (make-recognition-child #f (make-token 'number "1" 0 1)))
             (b (make-recognition-child 'item (make-token 'number "2" 2 3)))
             (c (make-recognition-child #f (make-token 'number "3" 4 5)))
             (sequence (recognition-sequence-append
                        (recognition-sequence-relocate (list a) 10)
                        (recognition-sequence-append (list b)
                          (recognition-sequence-relocate (list c) -2))))
             (result (recognition-sequence->list sequence)))
        (check (map (lambda (child) (recognition-value-start (recognition-child-value child))) result)
               => '(10 2 2))
        (check (eq? (cadr result) b) => #t)
        (check (recognition-child-field (cadr result)) => 'item)
        (check (recognition-value-start (recognition-child-value a)) => 0)
        (check (recognition-value-start (recognition-child-value c)) => 4)
        (let (leaf (list b))
          (check (eq? (recognition-sequence->list leaf) leaf) => #t))))
    (test-case "deep concatenation preserves order and moved bounds without recursion"
      (let* ((children (map (lambda (n) (make-recognition-child #f (make-token 'number "1" n (+ n 1)))) (iota 10000)))
             (sequence (recognition-sequence-concatenate (map list children)))
             (moved (recognition-sequence-relocate (recognition-sequence-relocate sequence 13) -5)))
        (check (recognition-sequence->list sequence) => children)
        (check (recognition-sequence-start moved 0) => 8)
        (check (recognition-sequence-end moved 0) => 10008)
        (let (values (recognition-sequence->list moved))
          (check (recognition-value-start (recognition-child-value (car values))) => 8)
          (check (recognition-value-end (recognition-child-value (last values))) => 10008))))
    (test-case "fusion can append a prefix retained by a materialized session"
      (let (bare
            (parameterize ((current-recognition-sequence-fusion-enabled? #f))
              (recognition-sequence-append
               (list (make-recognition-child #f (make-token 'number "1" 1 2)))
               (list (make-recognition-child #f (make-token 'number "2" 3 4))))))
        (parameterize ((current-recognition-sequence-fusion-enabled? #t))
          (let (combined (recognition-sequence-append bare
                          (list (make-recognition-child #f (make-token 'number "3" 5 6)))))
            (check (recognition-sequence-arity combined) => 2)
            (check (recognition-sequence-start combined 0) => 1)
            (check (recognition-sequence-end combined 0) => 6)
            (check (length (recognition-sequence->list combined)) => 3)))))
    (test-case "field nesting and alias ranges survive deferred materialization"
      (let* ((one (list (make-recognition-child 'inner (make-token 'number "1" 3 4))))
             (two (recognition-sequence-append one (list (make-recognition-child #f (make-token 'number "2" 5 6)))))
             (moved (recognition-sequence-relocate two 7))
             (field (car (recognition-children-field 'outer moved 0)))
             (alias (recognition-child-value (car (recognition-children-alias 'pair moved 0)))))
        (check (recognition-children-field 'empty '() 99) => '())
        (check (recognition-sequence-start (recognition-sequence-relocate '() 7) 99) => 99)
        (check (recognition-sequence-arity
                (recognition-sequence-append (recognition-sequence-relocate '() 7) one)) => 1)
        (check (recognition-fragment? (recognition-child-value (car (recognition-children-field 'outer one 0)))) => #t)
        (check (recognition-child-field field) => 'outer)
        (check (recognition-value-start (recognition-child-value field)) => 10)
        (check (recognition-value-end alias) => 13)
        (check (recognition-child-field (car (recognition-sequence->list (recognition-node-children alias)))) => 'inner)))
    (test-case "GLR completion evidence remains canonical with fusion requested"
      (parameterize ((current-recognition-sequence-fusion-enabled? #t))
        (check (selective-glr-scenario-pass? (selective-glr-scenario)) => #t)))))
(def recognition-sequence-test recognition-sequence-tests)
(export recognition-sequence-tests recognition-sequence-test)
