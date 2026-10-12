#!/usr/bin/env gxi
(import :std/test
        :gerbil-parser/src/runtime/funcs
        :gerbil-parser/src/runtime/recognition
        :gerbil-parser/src/runtime/reduce
        (only-in :gerbil-parser/src/runtime/token make-token))
(def recognition-prepend-test
  (test-suite "borrowed singleton recognition composition"
    (test-case "ordinary suffix sharing preserves both borrowed inputs"
      (let* ((a (make-recognition-child #f (make-token 'number "1" 0 1)))
             (b (make-recognition-child 'value (make-token 'number "2" 2 3)))
             (prefix (list a)) (suffix (list b))
             (result (recognition-sequence-append prefix suffix)))
        (check result => (append prefix suffix))
        (check (eq? (cdr result) suffix) => #t)
        (check prefix => (list a))
        (check suffix => (list b))
        (check (recognition-sequence-start result 99) => 0)
        (check (recognition-sequence-end result 99) => 3)))
    (test-case "shared suffix accepts independent prefixes without mutation"
      (let* ((child (make-recognition-child #f (make-token 'number "3" 4 5)))
             (suffix (list child))
             (prefixes (map (lambda (n)
                             (list (make-recognition-child #f (make-token 'number "1" n (+ n 1)))))
                           '(0 1 2)))
             (products (map (lambda (prefix) (recognition-sequence-append prefix suffix)) prefixes)))
        (for-each (lambda (prefix product)
                    (check product => (append prefix suffix))
                    (check (eq? (cdr product) suffix) => #t)) prefixes products)
        (check suffix => (list child))))
    (test-case "relocation retains its context across an ordinary prefix"
      (let* ((a (make-recognition-child #f (make-token 'number "1" 0 1)))
             (b (make-recognition-child 'value (make-token 'number "2" 2 3)))
             (suffix (recognition-sequence-relocate (list b) 7))
             (result (recognition-sequence-append (list a) suffix))
             (observed '()))
        (recognition-sequence-for-each
         (lambda (child delta moved?) (set! observed (cons (list child delta moved?) observed))) result)
        (check (reverse observed) => (list (list a 0 #f) (list b 7 #t)))
        (check (recognition-value-start (recognition-child-value b)) => 2)
        (check (recognition-sequence-end result 99) => 10)))
    (test-case "field and alias actions retain ordered canonical children"
      (let* ((a (make-recognition-child #f (make-token 'number "1" 0 1)))
             (b (make-recognition-child #f (make-token 'number "2" 2 3)))
             (children (recognition-sequence-append (list a) (list b)))
             (field (recognition-action-field 'value children 99))
             (alias (recognition-action-alias 'pair children 99)))
        (check field => (recognition-action-field 'value (list a b) 99))
        (check alias => (recognition-action-alias 'pair (list a b) 99))))
    (test-case "deep singleton prepending remains ordered without branch flattening"
      (let* ((children (map (lambda (n) (make-recognition-child #f (make-token 'number "1" n (+ n 1))))
                           (iota 10000)))
             (result (foldr (lambda (child suffix) (recognition-sequence-append (list child) suffix))
                            '() children)))
        (check (recognition-sequence->list result) => children)
        (check (recognition-sequence-arity result) => 2)
        (check (recognition-sequence-end result 99) => 10000)))))
(export recognition-prepend-test)
