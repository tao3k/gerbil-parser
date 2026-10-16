;;; Position-view iterator controls against an independent recipe interpreter.
(import :std/test
        :gerbil-parser/src/runtime/event-program
        (only-in :gerbil-parser/src/runtime/token make-token token-start)
        (only-in :gerbil-parser/src/runtime/recognition make-recognition-child)
        (only-in :gerbil-parser/src/runtime/funcs recognition-sequence-append
                 recognition-sequence-relocate recognition-sequence-for-each))
(export event-iterator-test)

;; Recipes describe semantic frames independently of private rope storage.
(def (compile-recipe recipe)
  (case (car recipe)
    ((token) (cadr recipe))
    ((view) (event-program-relocate (compile-recipe (caddr recipe)) (cadr recipe)))
    ((join) (event-program-append/avl (compile-recipe (cadr recipe)) (compile-recipe (caddr recipe))))
    ((sequence) (foldl (lambda (item code) (event-program-append code (compile-recipe item))) #f (cdr recipe)))
    ((node) (event-program-node-value (cadr recipe) 0 1 (compile-recipe (caddr recipe))))
    ((field) (event-program-field (cadr recipe) 0 1 (compile-recipe (caddr recipe))))
    ((fragment) (event-program-fragment-value 0 1 (compile-recipe (cadr recipe))))))
(def (recipe-events recipe delta moved?)
  (case (car recipe)
    ((token) (list (list 'token (cadr recipe) (token-start (cadr recipe)) delta moved?)))
    ((view) (recipe-events (caddr recipe) (+ delta (cadr recipe)) #t))
    ((join) (append (recipe-events (cadr recipe) delta moved?) (recipe-events (caddr recipe) delta moved?)))
    ((sequence) (apply append (map (lambda (item) (recipe-events item delta moved?)) (cdr recipe))))
    ((node field)
     (append (list (list (if (eq? (car recipe) 'node) 'open-node 'open-field) (cadr recipe) 0 delta moved?))
             (recipe-events (caddr recipe) delta moved?)
             (list (list (if (eq? (car recipe) 'node) 'close-node 'close-field) (cadr recipe) 1 delta moved?))))
    ((fragment) (append (recipe-events (cadr recipe) delta moved?) (list (list 'boundary #f 1 delta moved?))))))
(def (check-recipe recipe)
  (let ((program (compile-recipe recipe)) (actual '()) (inlined '()) (ids '()) (next-id 0))
    (event-program-walk (lambda (op name offset delta moved?)
                          (set! actual (cons (list op name offset delta moved?) actual))) program)
    (check (reverse actual) => (recipe-events recipe 0 #f))
    (event-program-walk/inline
     (lambda (op name offset delta moved? context)
       (set! inlined (cons (list op name offset delta moved?) inlined))
       (case op
         ((open-node) (let (id next-id) (set! next-id (+ next-id 1)) (set! ids (cons id ids)) id))
         ((close-node) (check context => (car ids)) (set! ids (cdr ids))))) program)
    (check ids => '())
    (check (reverse inlined) => (reverse actual))))
(def event-iterator-test
  (test-suite "position views and event rope continuations"
    (test-case "chunk and carry boundaries retain ordered independent sibling views"
      (for-each
       (lambda (width)
         (let* ((items (let loop ((i 0) (items '()))
                         (if (= i width) (reverse items)
                           (loop (+ i 1) (cons (list 'token (make-token 'word "x" i (+ i 1))) items)))))
                (sequence (cons 'sequence items))
                (moved (list 'view -9 (list 'view 9 sequence))))
           (check-recipe (list 'join moved sequence))
           (check-recipe (list 'view 3 (list 'join moved (list 'view -2 sequence))))))
       '(0 1 15 16 17 31 32 33 65 129)))
    (test-case "canonical child views restore deferred sibling context"
      (let* ((left (make-recognition-child 'left (make-token 'word "a" 0 1)))
             (right (make-recognition-child 'right (make-token 'word "b" 1 2)))
             (cancelled (recognition-sequence-relocate
                         (recognition-sequence-relocate (list left) 9) -9))
             (sequence (recognition-sequence-append cancelled (list right)))
             (actual '()))
        (recognition-sequence-for-each
         (lambda (child delta moved?) (set! actual (cons (list child delta moved?) actual))) sequence)
        (check (reverse actual) => (list (list left 0 #t) (list right 0 #f)))
        (check (eq? (caar (reverse actual)) left) => #t)))
    (test-case "recursive and explicit fallback paths preserve fields fragments and closing IDs"
      (for-each
       (lambda (depth)
         (let loop ((n depth) (recipe (list 'token (make-token 'word "x" 0 1))))
           (if (zero? n) (check-recipe recipe)
             (loop (- n 1) (list 'node 'Root (list 'field 'value
                                 (list 'fragment (list 'view 1 (list 'view -1 recipe)))))))))
       '(1 15 16 63 64 65 129)))))
