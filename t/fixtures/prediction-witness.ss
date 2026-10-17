;;; Observe unchanged native producer primitives. Values are copied at the
;;; first dequeue; no prediction algorithm is reimplemented in this recorder.
(import :gerbil-parser/src/compiler/lr-automaton
        (only-in :gerbil-parser/src/compiler/lr production-id production-index-by-lhs
                 nonterminal-symbol? nonterminal-name)
        :gerbil-parser/src/compiler/funcs
        (only-in :std/struct/queue make-Queue enqueue! dequeue! queue-empty?))
(export prediction-observation prediction-dependencies prediction-fixtures)

(def prediction-dependencies
  '(nonterminal-symbol? nonterminal-name production-id make-core-item
    compiler-index-set-add compiler-index-set-difference
    compiler-index-set-union compiler-index-set-for-each
    make-Queue enqueue! dequeue! queue-empty? make-vector))

(def (prediction-observation producer productions)
  (let* ((table (list->vector productions))
         (layout (make-item-layout table '#()))
         (symbols (make-core-symbol-catalog table layout))
         (allocated '()) (initial #f) (dequeues '()))
    (def (snapshot)
      (apply (lambda (item-nodes masks pending parents queued . rest)
               (list (vector->list parents) (vector->list masks)))
             (reverse allocated)))
    (let (catalog
      (producer symbols (production-index-by-lhs productions) layout
        nonterminal-symbol? nonterminal-name production-id make-core-item
        compiler-index-set-add compiler-index-set-difference
        compiler-index-set-union compiler-index-set-for-each
        make-Queue enqueue!
        (lambda (queue)
          (unless initial (set! initial (snapshot)))
          (let (node (dequeue! queue)) (set! dequeues (cons node dequeues)) node))
        queue-empty?
        (lambda (size fill)
          (let (store (make-vector size fill))
            (set! allocated (cons store allocated)) store))))
      (unless initial (set! initial (snapshot)))
      (apply (lambda (item-nodes masks pending parents queued . rest)
               (list (car initial) (cadr initial) (reverse dequeues)
                     (vector->list masks) (vector->list catalog) (vector->list item-nodes)))
             (reverse allocated)))))

;;; Distinct cases retain native cycles, empty owners and duplicate rule IDs.
(def prediction-fixtures
  (list
    '((0 start ((nonterminal A)) concat #f)
      (1 A ((nonterminal B)) concat #f)
      (2 B ((nonterminal A)) concat #f)
      (3 B () concat #f) (4 B () concat #f))
    '((0 start ((terminal token a)) concat #f))
    '((0 start ((nonterminal Missing)) concat #f))
    '((0 start ((nonterminal A)) concat #f)
      (1 A ((nonterminal A)) concat #f) (2 A () concat #f))
    '((0 start ((nonterminal A)) concat #f)
      (1 A ((nonterminal B)) concat #f) (2 B ((terminal token b)) concat #f))
    '((0 start ((nonterminal A) (nonterminal Dead)) concat #f)
      (1 A () concat #f) (2 Dead ((nonterminal Dead)) concat #f))))
