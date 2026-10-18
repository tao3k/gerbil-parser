(import :gerbil-parser/language-test-support
        (only-in :std/misc/ports read-all-as-string)
        (only-in :gerbil-parser/t/fixtures/lr1-construction write-lowering-traversal-data)
        (only-in :gerbil-parser/src/compiler/lr lower-rules production-id production-lhs production-rhs base-symbol
          validate-production-semantics))

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

(defparser-check (name-collision-case count references?)
  (scope ((reserved (map (lambda (index) (string->symbol (string-append "$start." (number->string index)))) (iota count)))
          (bindings '()) (events '()))
    (scope ((rules
      (append
        (list (list 'start (cons 'sequence
          (append (if references? (map (lambda (name) (list 'reference name)) reserved) '())
                  '((optional (literal "x")) (optional (literal "x")))))))
        (if references? '() (map (lambda (name) (list name '(empty))) reserved))
        (list (list '$accept '(empty))
              (list (string->symbol (string-append "$accept." (number->string (+ count 2)))) '(empty))))))
      (scope ((rows (lower-rules rules 'start #t #f
        (lambda (kind value expression)
          (set! events (cons kind events))
          (when (eq? kind 'binding) (set! bindings (cons (cons value expression) bindings)))))))
        (equal (map car (reverse bindings))
          (map (lambda (index) (string->symbol (string-append "$start." (number->string index))))
               (list count (+ count 1))))
        (equal (map cdr bindings) '((optional (literal "x")) (optional (literal "x"))))
        (equal (car (reverse events)) 'binding)
        (equal (production-lhs (car rows))
          (string->symbol (string-append "$accept." (number->string (+ count 3)))))
        (equal (map base-symbol (production-rhs (list-ref rows 5)))
          (append (if references? (map (lambda (name) (list 'nonterminal name)) reserved) '())
                  (map (lambda (binding) (list 'nonterminal (car binding))) (reverse bindings))))
        (equal (map production-id rows) (iota (length rows)))))))

(deflanguage-parser-tests lowering-traversal-test "recursive lowering generation and source alignment"
  (engine)
  (scenario "an empty-language helper has a binding without an lhs row and exposes the canonical vocabulary gap"
    (scope ((bindings '())
            (rules '((start (choice (sequence (choice)) (literal "x"))))))
      (scope ((rows (lower-rules rules 'start #t #f
        (lambda (kind value _proof) (when (eq? kind 'binding) (set! bindings (cons value bindings)))))))
        (equal (length bindings) 1)
        (equal (member (car bindings) (map production-lhs rows)) #f)
        (equal (map base-symbol (production-rhs (cadr rows)))
          (list (list 'nonterminal (car bindings))))
        (equal (map base-symbol (production-rhs (last rows))) '((terminal literal "x")))
        ;; Keep this failure visible until the declaration vocabulary is part
        ;; of the canonical producer/consumer contract, not inferred from lhs.
        (raises (validate-production-semantics rows) true))))
  (scenario "the shared renderer reproduces the complete native traversal proof input"
    (equal (call-with-output-string write-lowering-traversal-data)
      (call-with-input-file "formal/ParserEngine/LoweringTraversalData.lean" read-all-as-string)))
  (matrix "recursive helpers preserve depth-first production and callback order"
    (setup) ((kind '(optional repeat)) (depth '(1 2 3 4 8 16 32))) (bindings)
    (equal (parser-check-result nested-traversal-case kind depth) (void)))
  (matrix "n-ary operands publish all helper rows before the parent row"
    (setup) ((width '(0 1 2 8 32))) (bindings)
    (equal (parser-check-result wide-traversal-case width) (void)))
  (matrix "fresh allocation skips authored and undefined reference names before helper use"
    (setup) ((count '(0 1 2 8 32)) (references? '(#f #t))) (bindings)
    (equal (parser-check-result name-collision-case count references?) (void))))
