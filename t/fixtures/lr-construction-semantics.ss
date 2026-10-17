;;; Execute the actual construction producers. Only queue dependencies are
;;; observed; no copied LR algorithm or replacement catalogue supplies results.
(import (only-in :gerbil-parser/t/fixtures/prediction-witness
                 prediction-observation prediction-dependencies prediction-fixtures)
        (only-in :gerbil-parser/t/fixtures/event-walk-source source-procedure)
        (only-in :gerbil-parser/src/compiler/lr
                 production-id production-index-by-lhs nonterminal-symbol? nonterminal-name)
        :gerbil-parser/src/compiler/lr-automaton
        :gerbil-parser/src/compiler/funcs
        (only-in :std/struct/queue make-Queue enqueue! dequeue! queue-empty?))
(export native-predictions native-state-chain native-recursive-states native-coordinates
        native-template-groups prediction-fixtures)

(def (native-predictions productions)
  (let* ((table (list->vector productions))
         (layout (make-item-layout table '#()))
         (symbols (make-core-symbol-catalog table layout))
         (dequeues '())
         (producer (source-procedure "src/compiler/lr-automaton.ss" 'make-core-prediction-catalog
                     '(nonterminal-symbol? nonterminal-name production-id make-core-item
                       compiler-index-set-add compiler-index-set-difference
                       compiler-index-set-union compiler-index-set-for-each
                       make-Queue enqueue! dequeue! queue-empty?))))
    (let (catalog
      (producer symbols (production-index-by-lhs productions) layout
        nonterminal-symbol? nonterminal-name production-id make-core-item
        compiler-index-set-add compiler-index-set-difference
        compiler-index-set-union compiler-index-set-for-each
        make-Queue enqueue! (lambda (queue) (let (node (dequeue! queue))
          (set! dequeues (cons node dequeues)) node)) queue-empty?))
      (list (vector->list catalog) (reverse dequeues)))))

(def (native-state-chain width)
  (let* ((rhs (map (lambda (id) (list 'terminal 'token id)) (iota width)))
         (productions (list (list 0 'start rhs 'concat #f)))
         (table (list->vector productions))
         (layout (make-item-layout table '#()))
         (symbols (make-core-symbol-catalog table layout)))
    (let-values (((states transitions count _index processed)
                  (build-lr0-automaton productions table layout symbols)))
      (list count processed (vector-length states)
        (andmap (lambda (id) (equal? (vector-ref states id) (list id))) (iota count))
        (andmap (lambda (id)
          (equal? (vector-ref transitions id)
            (if (= id width) '() (list (cons (list 'terminal 'token id) (+ id 1))))))
          (iota count))))))

(def (native-recursive-states)
  (let* ((productions '((0 start ((nonterminal A)) concat #f)
                        (1 A ((terminal token a) (nonterminal A)) concat #f)
                        (2 A () concat #f)))
         (table (list->vector productions))
         (layout (make-item-layout table '#()))
         (symbols (make-core-symbol-catalog table layout)))
    (let-values (((states transitions count _index processed)
                  (build-lr0-automaton productions table layout symbols)))
      (list count processed
        (map (lambda (id) (vector-ref states id)) (iota count))
        (map (lambda (id) (vector-ref transitions id)) (iota count))))))

(def (native-coordinates rows queries)
  (let ((index (source-procedure "src/compiler/lr-lookahead.ss" 'index-lr0-items '()))
        (lookup (source-procedure "src/compiler/lr-lookahead.ss" 'lookahead-node-index '())))
    (let-values (((offsets items count) (index (list->vector rows) (length rows))))
      (list (vector->list offsets) (vector->list items) count
        (map (lambda (query) (lookup offsets items (car query) (cadr query))) queries)))))

;;; Compare sparse shared-group construction with a full sorted-closure walk.
;;; The reference has no template cache, touched scratch or merge machinery.
(def (native-template-groups productions kernel)
  (let* ((table (list->vector productions)) (layout (make-item-layout table '#()))
         (symbols (make-core-symbol-catalog table layout))
         (catalog-builder (source-procedure "src/compiler/lr-automaton.ss" 'make-core-prediction-catalog prediction-dependencies))
         (observed (prediction-observation catalog-builder productions))
         (catalog (list->vector (list-ref observed 4)))
         (kernel-mask (foldl (lambda (item mask) (compiler-index-set-add mask item)) 0 kernel))
         (prediction-mask (foldl (lambda (item mask) (compiler-index-set-union mask (vector-ref catalog item))) 0 kernel))
         (symbol-builder (source-procedure "src/compiler/lr-automaton.ss" 'make-lr0-symbol-catalog '()))
         (template-builder (source-procedure "src/compiler/lr-automaton.ss" 'make-prediction-template
                             '(compiler-index-set-for-each make-core-item compiler-index-set-add)))
         (closure-builder (source-procedure "src/compiler/lr-automaton.ss" 'lr0-closure '(compiler-index-set-for-each)))
         (groups-builder (source-procedure "src/compiler/lr-automaton.ss" 'lr0-state-kernels
                           '(compiler-index-set-for-each compiler-index-set-add compiler-index-set-union))))
    (let-values (((vocabulary symbol-ids) (symbol-builder symbols)))
      (let* ((template (template-builder prediction-mask layout symbol-ids compiler-index-set-for-each make-core-item compiler-index-set-add))
             (closure (closure-builder kernel-mask template compiler-index-set-for-each))
             (masks (make-vector (vector-length vocabulary) 0))
             (first-items (make-vector (vector-length vocabulary) 0))
             (groups (groups-builder kernel-mask template symbol-ids masks first-items
                       compiler-index-set-for-each compiler-index-set-add compiler-index-set-union))
             (reference (make-table test: eq?)) (order '()))
        (for-each (lambda (item)
          (let (id (vector-ref symbol-ids item))
            (when id
              (let (row (table-ref reference id #f))
                (unless row (set! row (vector item id 0))
                  (set! order (cons id order)) (table-set! reference id row))
                (vector-set! row 2 (compiler-index-set-add (vector-ref row 2) (+ item 1))))))) closure)
        (list groups (map (lambda (id) (table-ref reference id)) (reverse order))
              (andmap zero? (vector->list masks))
              (groups-builder kernel-mask template symbol-ids masks first-items
                compiler-index-set-for-each compiler-index-set-add compiler-index-set-union)
              (vector-ref template 0) closure)))))
