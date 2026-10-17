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

(def (template-index-valid? template ids)
  (let* ((rows (vector-ref template 1))
         (expected (full-group-reference (vector-ref template 0) ids)))
    (and (equal? (map vector->list rows) (map vector->list expected))
         (andmap (lambda (key)
           (let ((row (find (lambda (row) (= (vector-ref row 1) key)) rows))
                 (indexed (table-ref (vector-ref template 2) key #f)))
             (if row (eq? row indexed) (not indexed))))
           (iota (vector-length ids))))))

(def (template-contents template)
  (list (map identity (vector-ref template 0))
        (map vector->list (vector-ref template 1))
        (map (lambda (row) (eq? row (table-ref (vector-ref template 2) (vector-ref row 1) #f)))
             (vector-ref template 1))))

(def (construction-observation productions (poison? #t))
  (let* ((path "src/compiler/lr-automaton.ss")
         (table (list->vector productions)) (layout (make-item-layout table '#()))
         (cores (make-core-symbol-catalog table layout))
         (dequeues '()) (enqueues '()) (publications '()) (current #f)
         (publishing #f) (groups-ok? #t) (calls 0)
         (catalog #f) (created (make-table test: equal?))
         (template-count 0) (template-requests 0) (cache-ok? #t)
         (contents (make-table test: eq?)) (plans (make-table test: eq?))
         (scratch-owner #f) (first-owner #f) (last-firsts #f)
         (storage-ok? #t) (plan-copies 0) (plan-owner #f) (growths '())
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
          (set! catalog
            (prediction core-symbols by-lhs layout nonterminal-symbol? nonterminal-name production-id
              make-core-item compiler-index-set-add compiler-index-set-difference
              compiler-index-set-union compiler-index-set-for-each make-Queue enqueue! dequeue! queue-empty? make-vector))
          catalog)
        compiler-index-set-add make-core-item symbols
        (lambda (mask layout ids)
          (let (value (template mask layout ids compiler-index-set-for-each make-core-item compiler-index-set-add))
            (set! cache-ok? (and cache-ok? (not (table-ref created mask #f))))
            (table-set! created mask value)
            (table-set! contents value (template-contents value))
            (set! storage-ok? (and storage-ok? (template-index-valid? value ids)))
            (set! template-count (+ template-count 1))
            value))
        compiler-index-set-for-each compiler-index-set-union
        (lambda (mask template)
          (let (prediction-mask 0)
            (compiler-index-set-for-each mask (lambda (item)
              (set! prediction-mask (compiler-index-set-union prediction-mask (vector-ref catalog item)))))
            (set! template-requests (+ template-requests 1))
            (set! cache-ok? (and cache-ok? (eq? template (table-ref created prediction-mask #f))))
            (closure mask template compiler-index-set-for-each)))
        make-Queue
        (lambda (queue node) (set! enqueues (cons node enqueues)) (enqueue! queue node))
        (lambda (queue)
          (let (node (dequeue! queue))
            (set! current node) (set! dequeues (cons node dequeues)) node))
        queue-empty?
        (lambda (mask template ids masks first-items)
          (let (plan (table-ref plans current #f))
            (set! storage-ok?
              (and storage-ok? plan (= (car plan) mask) (eq? (cdr plan) template)
                   (or (not scratch-owner) (eq? masks scratch-owner))
                   (or (not first-owner) (eq? first-items first-owner))
                   (andmap zero? (vector->list masks))
                   (not (eq? masks first-items))
                   (not (eq? masks template)) (not (eq? first-items template))
                   (andmap (lambda (row) (and (not (eq? row masks)) (not (eq? row first-items))))
                           (vector-ref template 1))
                   (andmap identity (caddr (template-contents template)))
                   (or poison? (not last-firsts) (equal? (vector->list first-items) last-firsts))
                   (equal? (template-contents template) (table-ref contents template #f)))))
          (set! scratch-owner masks) (set! first-owner first-items)
          ;; Both adversarial stale values and actual prior-call contents are exercised.
          (when poison? (vector-fill! first-items 999999))
          (let* ((snapshot (template-contents template))
                 (rows (groups mask template ids masks first-items
                         compiler-index-set-for-each compiler-index-set-add compiler-index-set-union))
                 (complete (closure mask template compiler-index-set-for-each))
                 (reference (full-group-reference complete ids)))
            (set! calls (+ calls 1))
            (set! groups-ok?
              (and groups-ok? (equal? rows reference)
                   (andmap zero? (vector->list masks))
                   (equal? snapshot (template-contents template))))
            (set! last-firsts (vector->list first-items))
            rows))
        (lambda (rows) (set! publishing #t) (reverse rows))
        (lambda (store index value)
          ;; A physical plan is the actual (kernel . template-vector) pair.
          ;; Growth copies must retain the original shared template reference.
          (when (and (pair? value) (integer? (car value)) (vector? (cdr value)))
            (when (and plan-owner (not (eq? plan-owner store)))
              (set! growths (cons (list (vector-length plan-owner) (vector-length store)) growths)))
            (set! plan-owner store)
            (let (prior (table-ref plans index #f))
              (when prior
                (set! plan-copies (+ plan-copies 1))
                (set! storage-ok? (and storage-ok? (= (car prior) (car value)) (eq? (cdr prior) (cdr value)))))
            (table-set! plans index (cons (car value) (cdr value)))))
          (when publishing
            (set! publications (cons index publications))
            (set! groups-ok? (and groups-ok? (eqv? index current)))
            (set! publishing #f))
          (vector-set! store index value)))))
      (list count processed (reverse dequeues) (reverse enqueues)
            (reverse publications) calls groups-ok?
            (map (lambda (id) (vector-ref transitions id)) (iota count))
            (map (lambda (id) (vector-ref states id)) (iota count))
            template-count template-requests cache-ok? storage-ok? plan-copies
            (vector-length plan-owner) (reverse growths)))))
