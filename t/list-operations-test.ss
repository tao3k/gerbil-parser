(import :gerbil-parser/language-test-support
        (only-in :clan/poo/object .ref)
        (only-in :clan/poo/mop Bool)
        (only-in :clan/poo/type Symbol)
        (only-in :gerbil-parser/src/utilities/lists append-unique make-list-operations)
        (only-in :gerbil-parser/src/compiler/lr union-values))

(deflanguage-parser-tests list-operations-test "presence and ordered list merge"
  (engine)
    (scenario "false row is present and conflicting true row delegates once"
      (sequential ((ops (cdr (make-list-operations Bool Symbol))) (calls 0))
        (equal ((.ref ops 'merge-keyed-row) '(#f) #t (lambda (_) 'same)
          (lambda (key) (set! calls (+ calls 1)) (if (eq? key 'same) '(#t) '()))) '(#t))
        (equal calls 1)))
    (scenario "identical false row keeps original list and does not conflict"
      (sequential ((ops (cdr (make-list-operations Bool Symbol))) (rows (list #f)))
        (equal (eq? ((.ref ops 'merge-keyed-row) rows #f (lambda (_) 'same)
          (lambda (_) (error "unexpected collision"))) rows) #t)
        (equal ((.ref ops 'merge-keyed-row) rows #t (lambda (v) (if v 'true 'false))
          (lambda (_) (error "unexpected collision"))) '(#f #t))))
    (scenario "append and union preserve ordering duplicates and no-op identity"
      (sequential ((rows (list 'a 'a 'b)))
        (equal (eq? (append-unique rows 'a) rows) #t)
        (equal (append-unique rows 'c) '(a a b c))
        (equal (union-values rows '(b c c d)) '(a a b c d))
        (equal (eq? (union-values rows '(a b)) rows) #t))))
