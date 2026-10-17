;;; Observe the actual complete LR builder with unchanged queue primitives.
;;; The independent reference groups a full closure, never sparse scratch.
(import (only-in "event-walk-source.ss" source-procedure)
        (only-in "prediction-witness.ss" prediction-dependencies)
        :gerbil-parser/src/compiler/lr-automaton
        :gerbil-parser/src/compiler/funcs
        (only-in :gerbil-parser/src/compiler/lr production-index-by-lhs
                 nonterminal-symbol? nonterminal-name production-id)
        (only-in :std/struct/queue make-Queue enqueue! dequeue! queue-empty?))
(export construction-observation)

(def (full-group-reference closure symbol-ids)
  (let ((groups (make-table test: eq?)) (order '()))
    (for-each (lambda (item)
      (let (id (vector-ref symbol-ids item))
        (when id
          (let (row (table-ref groups id #f))
            (unless row
              (set! row (vector item id 0))
              (set! order (cons id order)) (table-set! groups id row))
            (vector-set! row 2 (compiler-index-set-add (vector-ref row 2) (+ item 1))))))) closure)
    (map (lambda (id) (table-ref groups id)) (reverse order))))

(def (construction-observation productions)
  (let* ((path "src/compiler/lr-automaton.ss")
         (table (list->vector productions)) (layout (make-item-layout table '#()))
         (cores (make-core-symbol-catalog table layout))
         (dequeues '()) (enqueues '()) (publications '()) (current #f)
         (publishing #f) (groups-ok? #t) (calls 0)
         (prediction (source-procedure path 'make-core-prediction-catalog prediction-dependencies))
         (symbols (source-procedure path 'make-lr0-symbol-catalog '()))
         (template (source-procedure path 'make-prediction-template
                      '(compiler-index-set-for-each make-core-item compiler-index-set-add)))
         (closure (source-procedure path 'lr0-closure '(compiler-index-set-for-each)))
         (groups (source-procedure path 'lr0-state-kernels
                    '(compiler-index-set-for-each compiler-index-set-add compiler-index-set-union)))
         (builder (source-procedure path 'build-lr0-automaton
            '(production-index-by-lhs make-core-prediction-catalog compiler-index-set-add
              make-core-item make-lr0-symbol-catalog make-prediction-template
              compiler-index-set-for-each compiler-index-set-union lr0-closure
              make-Queue enqueue! dequeue! queue-empty? lr0-state-kernels reverse vector-set!))))
    (let-values (((states transitions count _index processed)
      (builder productions table layout cores
        production-index-by-lhs
        (lambda (core-symbols by-lhs layout)
          (prediction core-symbols by-lhs layout nonterminal-symbol? nonterminal-name production-id
            make-core-item compiler-index-set-add compiler-index-set-difference
            compiler-index-set-union compiler-index-set-for-each make-Queue enqueue! dequeue! queue-empty? make-vector))
        compiler-index-set-add make-core-item symbols
        (lambda (mask layout ids)
          (template mask layout ids compiler-index-set-for-each make-core-item compiler-index-set-add))
        compiler-index-set-for-each compiler-index-set-union
        (lambda (mask template) (closure mask template compiler-index-set-for-each))
        make-Queue
        (lambda (queue node) (set! enqueues (cons node enqueues)) (enqueue! queue node))
        (lambda (queue)
          (let (node (dequeue! queue))
            (set! current node) (set! dequeues (cons node dequeues)) node))
        queue-empty?
        (lambda (mask template ids masks first-items)
          ;; Arbitrary stale first-item slots are permitted between calls.
          (vector-fill! first-items 999999)
          (let* ((snapshot (map vector->list (vector-ref template 1)))
                 (rows (groups mask template ids masks first-items
                         compiler-index-set-for-each compiler-index-set-add compiler-index-set-union))
                 (complete (closure mask template compiler-index-set-for-each))
                 (reference (full-group-reference complete ids)))
            (set! calls (+ calls 1))
            (set! groups-ok?
              (and groups-ok? (equal? rows reference)
                   (andmap zero? (vector->list masks))
                   (equal? snapshot (map vector->list (vector-ref template 1)))))
            rows))
        (lambda (rows) (set! publishing #t) (reverse rows))
        (lambda (store index value)
          (when publishing
            (set! publications (cons index publications))
            (set! groups-ok? (and groups-ok? (eqv? index current)))
            (set! publishing #f))
          (vector-set! store index value)))))
      (list count processed (reverse dequeues) (reverse enqueues)
            (reverse publications) calls groups-ok?
            (map (lambda (id) (vector-ref transitions id)) (iota count))
            (map (lambda (id) (vector-ref states id)) (iota count))))))
