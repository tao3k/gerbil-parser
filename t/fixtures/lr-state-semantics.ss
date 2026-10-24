;;; Execute complete unchanged private producer declarations, injecting only
;;; imported dependencies. Assertions remain in the parser test DSL owner.
(import (only-in :gerbil-parser/t/fixtures/event-walk-source source-procedure)
        (only-in :gerbil-parser/src/compiler/lr
                 production-rhs production-terminal-catalog compute-nullable compute-first
                 nonterminal-symbol? nonterminal-name)
        (only-in :gerbil-parser/src/compiler/lr-automaton
                 make-item-layout make-core-symbol-catalog make-core-item core-item-count)
        (only-in :gerbil-parser/src/compiler/funcs
                 compiler-index-set-add compiler-index-set-union compiler-index-set-singleton
                 compiler-index-set-for-each))
(export native-closure-items native-suffix-slots)

(def (native-closure-items kernel predictions)
  (let ((mask (foldl (lambda (item result) (compiler-index-set-add result item)) 0 kernel))
        (closure (source-procedure "src/compiler/lr-automaton.ss" 'lr0-closure
                   '(compiler-index-set-for-each))))
    (closure mask (vector predictions) compiler-index-set-for-each)))

(def (native-suffix-slots)
  (let* ((productions
           '((0 start ((nonterminal A) (terminal token stop)) concat #f)
             (1 A ((nonterminal B) (terminal token c)) concat #f)
             (2 A ((terminal token a) (nonterminal Dead) (nonterminal Dead)) concat #f)
             (3 B () concat #f)
             (4 B ((nonterminal A)) concat #f)
             (5 Dead ((nonterminal Dead)) concat #f)))
         (table (list->vector productions))
         (mask-reader (source-procedure "src/compiler/lr-lookahead.ss" 'terminal-list->mask
                        '(compiler-index-set-add)))
         (builder (source-procedure "src/compiler/lr-lookahead.ss" 'make-core-suffix-catalog
                    '(core-item-count production-rhs make-core-item nonterminal-symbol?
                      nonterminal-name compiler-index-set-singleton compiler-index-set-union
                      terminal-list->mask)))
         (empty-reads 0))
    (let-values (((terminals terminal-index) (production-terminal-catalog productions))
                 ((_names nullable) (compute-nullable productions)))
      (let-values (((_rows first) (compute-first productions nullable)))
        (let* ((layout (make-item-layout table terminals))
               (symbols (make-core-symbol-catalog table layout)))
          (let-values (((masks empty?)
            (builder table layout symbols first nullable terminal-index
              core-item-count production-rhs make-core-item nonterminal-symbol?
              nonterminal-name compiler-index-set-singleton compiler-index-set-union
              (lambda (values index)
                (when (null? values) (set! empty-reads (+ empty-reads 1)))
                (mask-reader values index compiler-index-set-add)))))
            (list (map (lambda (mask empty) (list mask empty))
                       (vector->list masks) (vector->list empty?)) empty-reads)))))))
