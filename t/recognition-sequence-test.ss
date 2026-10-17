#!/usr/bin/env gxi
(import :gerbil-parser/language-test-support)
(import :std/test
        :gerbil-parser/src/runtime/funcs
        :gerbil-parser/src/runtime/recognition
        :gerbil-parser/src/runtime/reduce
        (only-in :gerbil-parser/src/runtime/token make-token)
        "./scenarios/performance/selective-glr/scenario")
(deflanguage-parser-tests recognition-sequence-tests "ordered semantic sequence publication"
  (engine)
    (scenario "mixed branch views isolate offsets and preserve untouched children"
      (sequential ((a (make-recognition-child #f (make-token 'number "1" 0 1)))
             (b (make-recognition-child 'item (make-token 'number "2" 2 3)))
             (c (make-recognition-child #f (make-token 'number "3" 4 5)))
             (sequence (recognition-sequence-append
                        (recognition-sequence-relocate (list a) 10)
                        (recognition-sequence-append (list b)
                          (recognition-sequence-relocate (list c) -2))))
             (result (recognition-sequence->list sequence)))
        (equal (map (lambda (child) (recognition-value-start (recognition-child-value child))) result)
                '(10 2 2))
        (equal (eq? (cadr result) b)  #t)
        (equal (recognition-child-field (cadr result))  'item)
        (equal (recognition-value-start (recognition-child-value a))  0)
        (equal (recognition-value-start (recognition-child-value c))  4)
        (scope ((leaf (list b)))
          (equal (eq? (recognition-sequence->list leaf) leaf)  #t))))
    (scenario "deep concatenation preserves order and moved bounds without recursion"
      (sequential ((children (map (lambda (n) (make-recognition-child #f (make-token 'number "1" n (+ n 1)))) (iota 10000)))
             (sequence (recognition-sequence-concatenate (map list children)))
             (moved (recognition-sequence-relocate (recognition-sequence-relocate sequence 13) -5)))
        (equal (recognition-sequence->list sequence)  children)
        (equal (recognition-sequence-start moved 0)  8)
        (equal (recognition-sequence-end moved 0)  10008)
        (scope ((values (recognition-sequence->list moved)))
          (equal (recognition-value-start (recognition-child-value (car values)))  8)
          (equal (recognition-value-end (recognition-child-value (last values)))  10008))))
    (scenario "pending branches retain independent relocation contexts"
      (sequential ((one (make-recognition-child 'a (make-token 'number "1" 0 1)))
             (two (make-recognition-child 'b (make-token 'number "2" 1 2)))
             (three (make-recognition-child 'c (make-token 'number "3" 2 3)))
             (four (make-recognition-child 'd (make-token 'number "4" 3 4)))
             (left (recognition-sequence-relocate
                    (recognition-sequence-append (list one) (list two)) 7))
             (right (recognition-sequence-append (list three)
                      (recognition-sequence-relocate (list four) -2)))
             (sequence (recognition-sequence-append left right))
             (observed '()))
        (act (recognition-sequence-for-each
         (lambda (child delta moved?) (set! observed (cons (list child delta moved?) observed))) sequence))
        (equal (reverse observed)  (list (list one 7 #t) (list two 7 #t)
                                          (list three 0 #f) (list four -2 #t)))
        (sequential ((nested (recognition-sequence-relocate
                        (recognition-sequence-append
                         (recognition-sequence-relocate (list one) -7) (list two)) 7))
               (observed '()))
          (act (recognition-sequence-for-each
           (lambda (child delta moved?) (set! observed (cons (list child delta moved?) observed))) nested))
          (equal (reverse observed)  (list (list one 0 #t) (list two 7 #t))))))
    (scenario "shared subtrees restore every nested view boundary"
      (sequential ((child (make-recognition-child 'value (make-token 'number "1" 1000 1001)))
             (product
              (let build ((depth 8))
                (if (zero? depth) (cons (list child) (list (list child 0 #f)))
                  (let* ((nested (build (- depth 1))) (sequence (car nested)) (expected (cdr nested)))
                    (cons (recognition-sequence-append
                           (recognition-sequence-relocate sequence depth)
                           (recognition-sequence-relocate sequence (- depth)))
                      (append (map (lambda (row) (list (car row) (+ (cadr row) depth) #t)) expected)
                              (map (lambda (row) (list (car row) (- (cadr row) depth) #t)) expected)))))))
             (observed '()))
        (act (recognition-sequence-for-each
         (lambda (value delta moved?) (set! observed (cons (list value delta moved?) observed))) (car product)))
        (equal (reverse observed)  (cdr product))
        (equal (map (lambda (value) (recognition-value-start (recognition-child-value value)))
                    (recognition-sequence->list (car product)))
                (map (lambda (row) (+ 1000 (cadr row))) (cdr product)))))
    (property "visitor failures propagate before later branches"
      (bindings (one (make-recognition-child #f (make-token 'number "1" 0 1))) (sequence (recognition-sequence-append (list one) (list one))) (condition (cons 'visitor 'stopped)) (visited 0))
      (equal (with-catch (lambda (caught) (eq? caught condition))
                 (lambda ()
                   (recognition-sequence-for-each
                    (lambda (_child _delta _moved?) (set! visited (+ visited 1)) (raise condition)) sequence)
                   #f))  #t)
      (equal visited  1))
    (scenario "fusion can append a prefix retained by a materialized session"
      (scope ((bare
            (parameterize ((current-recognition-sequence-fusion-enabled? #f))
              (recognition-sequence-append
               (list (make-recognition-child #f (make-token 'number "1" 1 2)))
               (list (make-recognition-child #f (make-token 'number "2" 3 4)))))))
        (parameters ((current-recognition-sequence-fusion-enabled? #t))
          (scope ((combined (recognition-sequence-append bare
                          (list (make-recognition-child #f (make-token 'number "3" 5 6))))))
            (equal (recognition-sequence-arity combined)  2)
            (equal (recognition-sequence-start combined 0)  1)
            (equal (recognition-sequence-end combined 0)  6)
            (equal (length (recognition-sequence->list combined))  3)))))
    (property "field nesting and alias ranges survive deferred materialization"
      (bindings (one (list (make-recognition-child 'inner (make-token 'number "1" 3 4)))) (two (recognition-sequence-append one (list (make-recognition-child #f (make-token 'number "2" 5 6))))) (moved (recognition-sequence-relocate two 7)) (field (car (recognition-children-field 'outer moved 0))) (alias (recognition-child-value (car (recognition-children-alias 'pair moved 0)))))
      (equal (recognition-children-field 'empty '() 99)  '())
      (equal (recognition-sequence-start (recognition-sequence-relocate '() 7) 99)  99)
      (equal (recognition-sequence-arity
                (recognition-sequence-append (recognition-sequence-relocate '() 7) one))  1)
      (equal (recognition-fragment? (recognition-child-value (car (recognition-children-field 'outer one 0))))  #t)
      (equal (recognition-child-field field)  'outer)
      (equal (recognition-value-start (recognition-child-value field))  10)
      (equal (recognition-value-end alias)  13)
      (equal (recognition-child-field (car (recognition-sequence->list (recognition-node-children alias))))  'inner))
    (scenario "GLR completion evidence remains canonical with fusion requested"
      (parameters ((current-recognition-sequence-fusion-enabled? #t))
        (equal (selective-glr-scenario-pass? (selective-glr-scenario))  #t))))
(def recognition-sequence-test recognition-sequence-tests)
(export  recognition-sequence-test)
