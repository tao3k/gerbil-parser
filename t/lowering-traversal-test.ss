(import :gerbil-parser/language-test-support
        (only-in :gerbil-parser/src/compiler/lr-compiler compile-lr-spec)
        (only-in :gerbil-parser/src/compiler/rust-runtime rust-runtime-module-source)
        (only-in :gerbil-parser/src/runtime/lr-parser lr-prepare)
        (only-in :std/misc/ports read-all-as-string)
        (only-in :gerbil-parser/t/fixtures/lr1-construction write-lowering-traversal-data)
        (only-in :gerbil-parser/src/compiler/lr lower-rules lower-grammar production-id production-lhs production-rhs base-symbol
          validate-production-semantics))

(def (nested-expression kind depth)
  (if (= depth 0) '(literal "x")
    (list kind (nested-expression kind (- depth 1)))))

(defparser-check (nested-traversal-case kind depth)
  (scope ((origins '()) (bindings '()) (recipes '()) (declared '()) (callback-order? #t)
          (rules (list (list 'start (nested-expression kind depth)))))
    (scope ((rows (lower-rules rules 'start #t
      (lambda (row owner path expression)
        (set! origins (cons (list row owner path expression) origins)))
      (lambda (tag value recipe)
        (case tag
          ((binding)
            (set! callback-order? (and callback-order? (pair? declared) (eq? value (car declared))))
            (set! bindings (cons (cons value recipe) bindings)))
          ((production) (set! recipes (cons recipe recipes)))))
      (lambda (name) (set! declared (cons name declared))))))
      (equal rows (lower-rules rules 'start #t))
      (equal (length (cdr rows)) (* 2 depth))
      (equal (map production-id rows) (iota (+ 1 (* 2 depth))))
      (equal callback-order? #t)
      (equal declared (map car bindings))
      (scope-values (((compiled declarations) (lower-grammar rules 'start #t)))
        (equal compiled rows)
        (equal declarations (append '($accept start) (reverse declared))))
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
          (bindings '()) (events '()) (declared '()))
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
          (when (eq? kind 'binding) (set! bindings (cons (cons value expression) bindings))))
        (lambda (name) (set! declared (cons name declared))))))
        (equal declared (map car bindings))
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
  (scenario "an empty-language helper is declared independently of lhs rows"
    (scope ((bindings '())
            (rules '((start (choice (sequence (choice)) (literal "x"))))))
      (scope ((rows (lower-rules rules 'start #t #f
        (lambda (kind value _proof) (when (eq? kind 'binding) (set! bindings (cons value bindings)))))))
        (equal (length bindings) 1)
        (equal (member (car bindings) (map production-lhs rows)) #f)
        (equal (map base-symbol (production-rhs (cadr rows)))
          (list (list 'nonterminal (car bindings))))
        (equal (map base-symbol (production-rhs (last rows))) '((terminal literal "x")))
        (scope-values (((compiled declarations) (lower-grammar rules 'start)))
          (equal compiled rows)
          (equal (not (not (memq (car bindings) declarations))) #t)
          (equal (validate-production-semantics compiled declarations) rows)
          (raises (validate-production-semantics compiled '(start $accept)) true)))))
  (scenario "a failing declaration callback aborts before proof, recursion and publication"
    (scope ((declared '()) (proofs '()) (emitted '()) (returned? #f)
            (rules '((start (sequence (optional (literal "x")))))))
      (raises
        (begin
          (lower-rules rules 'start #t
            (lambda arguments (set! emitted (cons arguments emitted)))
            (lambda arguments (set! proofs (cons arguments proofs)))
            (lambda (name) (set! declared (cons name declared)) (error "declaration rejected" name)))
          (set! returned? #t)) true)
      (equal declared '($start.0))
      (equal proofs '())
      (equal emitted '())
      (equal returned? #f)
      (scope-values (((rows declarations) (lower-grammar rules 'start)))
        (equal declarations '($accept start $start.0))
        (equal (length rows) 4))))
  (scenario "Scheme preparation and Rust emission consume the same declaration directory"
    (scope ((spec (compile-lr-spec '((start (choice (sequence (choice)) (literal "x")))) 'start)))
      (scope ((ir (list '(schema . "gerbil-parser.parser-ir.v1") '(root-kind . Root)
                       '(syntax-kinds (Root node)) '(terminals) '(lexical-rules) '(extras)
                       (cons 'lr-spec spec))))
        (equal (not (not (lr-prepare spec))) #t)
        (equal (not (not (string-contains
          (rust-runtime-module-source "declaration-probe" "test" "test" "test" ir)
          "Symbol::Nonterminal(\"$start.0\")"))) #t)
        (scope ((missing (filter (lambda (row) (not (eq? (car row) 'declarations))) spec)))
          (raises (lr-prepare missing) true)
          (raises (rust-runtime-module-source "declaration-probe" "test" "test" "test"
                    (cons (cons 'lr-spec missing) (filter (lambda (row) (not (eq? (car row) 'lr-spec))) ir))) true)))))
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
