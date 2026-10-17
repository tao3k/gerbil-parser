;;; Read-only observations of the actual source builder. Primitive wrappers
;;; forward every allocation/write unchanged; negative controls alter receipts.
(import (only-in "event-walk-source.ss" source-procedure)
        (only-in "construction-layout.ss" construction-layout)
        (only-in "prediction-witness.ss" prediction-dependencies)
        :gerbil-parser/src/compiler/lr-automaton
        :gerbil-parser/src/compiler/funcs
        (only-in :gerbil-parser/src/compiler/lr production-index-by-lhs
                 nonterminal-symbol? nonterminal-name production-id)
        (only-in :std/struct/queue make-Queue enqueue! dequeue! queue-empty?))
(export joint-construction-observation)

(def (generations allocations)
  (if (null? allocations) '()
    (cons (list (car allocations) (cadr allocations) (caddr allocations))
          (generations (cdddr allocations)))))

(def (group-writes group writes)
  (filter (lambda (entry) (memq (car entry) group)) writes))

(def (copies-valid? groups writes)
  (or (null? (cdr groups))
    (let* ((before (car groups)) (after (cadr groups))
           (count (vector-length (car before)))
           (actual (group-writes after writes))
           (expected (apply append (map (lambda (index)
             (map (lambda (source target) (list target index (vector-ref source index))) before after))
             (iota count)))))
      (and (>= (length actual) (length expected))
           (andmap (lambda (one two)
             (and (eq? (car one) (car two)) (= (cadr one) (cadr two)) (eq? (caddr one) (caddr two))))
             (take actual (length expected)) expected)
           (copies-valid? (cdr groups) writes)))))

(def (joint-construction-observation productions initial)
  (let* ((path "src/compiler/lr-automaton.ss")
         (table (list->vector productions)) (layout (make-item-layout table '#()))
         (cores (make-core-symbol-catalog table layout)) (allocations '()) (writes '())
         (prediction (source-procedure path 'make-core-prediction-catalog prediction-dependencies))
         (symbols (source-procedure path 'make-lr0-symbol-catalog '()))
         (template (source-procedure path 'make-prediction-template
           '(compiler-index-set-for-each make-core-item compiler-index-set-add)))
         (closure (source-procedure path 'lr0-closure '(compiler-index-set-for-each)))
         (groups (source-procedure path 'lr0-state-kernels
           '(compiler-index-set-for-each compiler-index-set-add compiler-index-set-union)))
         (builder (source-procedure path 'build-lr0-automaton
           '(production-index-by-lhs make-core-prediction-catalog compiler-index-set-add
             make-core-item make-lr0-symbol-catalog make-prediction-template compiler-index-set-for-each
             compiler-index-set-union lr0-closure make-Queue enqueue! dequeue! queue-empty?
             lr0-state-kernels reverse make-vector vector-set!) (construction-layout initial))))
    (let-values (((states rows count _index processed)
      (builder productions table layout cores production-index-by-lhs
        (lambda (core-symbols by-lhs layout)
          (prediction core-symbols by-lhs layout nonterminal-symbol? nonterminal-name production-id
            make-core-item compiler-index-set-add compiler-index-set-difference compiler-index-set-union
            compiler-index-set-for-each make-Queue enqueue! dequeue! queue-empty? make-vector))
        compiler-index-set-add make-core-item symbols
        (lambda (mask layout ids) (template mask layout ids compiler-index-set-for-each make-core-item compiler-index-set-add))
        compiler-index-set-for-each compiler-index-set-union
        (lambda (mask value) (closure mask value compiler-index-set-for-each))
        make-Queue enqueue! dequeue! queue-empty?
        (lambda (mask value ids masks firsts)
          (groups mask value ids masks firsts compiler-index-set-for-each compiler-index-set-add compiler-index-set-union))
        reverse
        (lambda (size fill)
          (let (value (make-vector size fill))
            (when (or (eq? fill #f) (null? fill)) (set! allocations (cons value allocations)))
            value))
        (lambda (store index value)
          (set! writes (cons (list store index value) writes))
          (vector-set! store index value)))))
      (let* ((groups (generations (reverse allocations))) (ordered (reverse writes))
             (final (last groups))
             (aligned (and (= initial (vector-length (caar groups)))
               (andmap (lambda (group)
                 (= (vector-length (car group)) (vector-length (cadr group)) (vector-length (caddr group)))) groups)
               (andmap (lambda (one two) (= (* 2 (vector-length (car one))) (vector-length (car two))))
                 (drop-right groups 1) (cdr groups))
               (eq? states (car final)) (eq? rows (cadr final)))))
        (let-values (((reference-states reference-rows reference-count _reference-index reference-processed)
                      (build-lr0-automaton productions table layout cores)))
          (let* ((parity (and (= count reference-count processed reference-processed)
                    (andmap (lambda (node)
                      (and (equal? (vector-ref states node) (vector-ref reference-states node))
                           (equal? (vector-ref rows node) (vector-ref reference-rows node)))) (iota count))))
                 (controls (or (null? (cdr groups))
                   (let (first (car (group-writes (cadr groups) ordered)))
                     (and (not (copies-valid? groups (filter (lambda (entry) (not (eq? entry first))) ordered)))
                          (not (copies-valid? groups (map (lambda (entry)
                            (if (eq? entry first) (list (car entry) (+ 1 (cadr entry)) (caddr entry)) entry)) ordered)))
                          (not (copies-valid? groups (map (lambda (entry)
                            (if (eq? entry first) (list (car entry) (cadr entry) 'invalid-copy) entry)) ordered))))))))
            (list count (vector-length states) aligned parity (copies-valid? groups ordered) controls)))))))
