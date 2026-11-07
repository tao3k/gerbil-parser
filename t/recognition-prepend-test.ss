#!/usr/bin/env gxi
(import :gerbil-parser/language-test-support)
(import :std/test
        :gerbil-parser/src/runtime/funcs
        :gerbil-parser/src/runtime/recognition
        :gerbil-parser/src/runtime/reduce
        (only-in :gerbil-parser/src/runtime/token make-token))
(deflanguage-parser-tests recognition-prepend-test "borrowed singleton recognition composition"
  (engine)
    (property "ordinary suffix sharing preserves both borrowed inputs"
      (bindings (a (make-recognition-child #f (make-token 'number "1" 0 1))) (b (make-recognition-child 'value (make-token 'number "2" 2 3))) (prefix (list a)) (suffix (list b)) (result (recognition-sequence-append prefix suffix)))
      (equal result  (append prefix suffix))
      (equal (eq? (cdr result) suffix)  #t)
      (equal prefix  (list a))
      (equal suffix  (list b))
      (equal (recognition-sequence-start result 99)  0)
      (equal (recognition-sequence-end result 99)  3))
    (scenario "shared suffix accepts independent prefixes without mutation"
      (sequential ((child (make-recognition-child #f (make-token 'number "3" 4 5)))
             (suffix (list child))
             (prefixes (map (lambda (n)
                             (list (make-recognition-child #f (make-token 'number "1" n (+ n 1)))))
                           '(0 1 2)))
             (products (map (lambda (prefix) (recognition-sequence-append prefix suffix)) prefixes)))
        (each (prefix product) (prefixes products)
                    (equal product  (append prefix suffix))
                    (equal (eq? (cdr product) suffix)  #t))
        (equal suffix  (list child))))
    (scenario "relocation retains its context across an ordinary prefix"
      (sequential ((a (make-recognition-child #f (make-token 'number "1" 0 1)))
             (b (make-recognition-child 'value (make-token 'number "2" 2 3)))
             (suffix (recognition-sequence-relocate (list b) 7))
             (result (recognition-sequence-append (list a) suffix))
             (observed '()))
        (act (recognition-sequence-for-each
         (lambda (child delta moved?) (set! observed (cons (list child delta moved?) observed))) result))
        (equal (reverse observed)  (list (list a 0 #f) (list b 7 #t)))
        (equal (recognition-value-start (recognition-child-value b))  2)
        (equal (recognition-sequence-end result 99)  10)))
    (property "field and alias actions retain ordered canonical children"
      (bindings (a (make-recognition-child #f (make-token 'number "1" 0 1))) (b (make-recognition-child #f (make-token 'number "2" 2 3))) (children (recognition-sequence-append (list a) (list b))) (field (recognition-action-field 'value children 99)) (alias (recognition-action-alias 'pair children 99)))
      (equal field  (recognition-action-field 'value (list a b) 99))
      (equal alias  (recognition-action-alias 'pair (list a b) 99)))
    (property "deep singleton prepending remains ordered without branch flattening"
      (bindings (children (map (lambda (n) (make-recognition-child #f (make-token 'number "1" n (+ n 1))))
                           (iota 10000))) (result (foldr (lambda (child suffix) (recognition-sequence-append (list child) suffix))
                            '() children)))
      (equal (recognition-sequence->list result)  children)
      (equal (recognition-sequence-arity result)  2)
      (equal (recognition-sequence-end result 99)  10000)))
