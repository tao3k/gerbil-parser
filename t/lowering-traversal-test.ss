(import :gerbil-parser/language-test-support
        (only-in :std/misc/ports read-all-as-string)
        (only-in :gerbil-parser/t/fixtures/lr1-construction write-lowering-traversal-data)
        (only-in :gerbil-parser/src/compiler/lr lower-rules production-id production-lhs production-rhs))

(def (nested-expression kind depth)
  (if (= depth 0) '(literal "x")
    (list kind (nested-expression kind (- depth 1)))))

(defparser-check (nested-traversal-case kind depth)
  (scope ((origins '()) (bindings '()) (recipes '())
          (rules (list (list 'start (nested-expression kind depth)))))
    (scope ((rows (lower-rules rules 'start #t
      (lambda (row owner path expression)
        (set! origins (cons (list row owner path expression) origins)))
      (lambda (tag value recipe)
        (case tag
          ((binding) (set! bindings (cons (cons value recipe) bindings)))
          ((production) (set! recipes (cons recipe recipes))))))))
      (equal rows (lower-rules rules 'start #t))
      (equal (length (cdr rows)) (* 2 depth))
      (equal (map production-id rows) (iota (+ 1 (* 2 depth))))
      (equal (length bindings) (- depth 1))
      (equal (length recipes) (* 2 depth))
      (equal (map caddr (reverse origins))
        (append (map (lambda (index) (make-list index 0)) (iota depth))
                (reverse (map (lambda (index) (make-list index 0)) (iota depth)))))
      (each (origin) (origins)
        (equal (cadr origin) 'start)
        (equal (eq? (car origin) (list-ref rows (production-id (car origin)))) #t)))))

(defparser-check (wide-traversal-case width)
  (scope ((rules (list (list 'start
            (cons 'sequence (map (lambda (_) '(field item (optional (literal "x")))) (iota width)))))))
    (scope ((rows (lower-rules rules 'start)))
      (equal (length rows) (+ 2 (* 2 width)))
      (equal (production-lhs (last rows)) 'start)
      (equal (length (production-rhs (last rows))) width)
      (equal (map production-id rows) (iota (length rows))))))

(deflanguage-parser-tests lowering-traversal-test "recursive lowering generation and source alignment"
  (engine)
  (scenario "the shared renderer reproduces the complete native traversal proof input"
    (equal (call-with-output-string write-lowering-traversal-data)
      (call-with-input-file "formal/ParserEngine/LoweringTraversalData.lean" read-all-as-string)))
  (matrix "recursive helpers preserve depth-first production and callback order"
    (setup) ((kind '(optional repeat)) (depth '(1 2 3 4 8 16 32))) (bindings)
    (equal (parser-check-result nested-traversal-case kind depth) (void)))
  (matrix "n-ary operands publish all helper rows before the parent row"
    (setup) ((width '(0 1 2 8 32))) (bindings)
    (equal (parser-check-result wide-traversal-case width) (void))))
