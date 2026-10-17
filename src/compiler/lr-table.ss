;;; -*- Gerbil -*-
;;; Canonical LR table admission before indexes or executable products exist.
(import (only-in :std/iter for in-range)
        (prefix-in :std/struct/queue stdq-)
        (only-in ./lr +lr-eof+ canonical-base-symbol?
                 production-action production-lhs production-rhs base-symbol layout-end-action?
                 production-terminal-catalog))
(export validate-lr-tables layout-productions?)

;;; Derive capability from admitted declarations, never from an author flag.
(def (layout-productions? productions)
  (any (lambda (production)
         (or (layout-end-action? (production-action production))
             (any (lambda (operand)
                    (match (base-symbol operand)
                      (['terminal (or 'layout-start 'layout-next) _] #t)
                      (_ #f)))
                  (production-rhs production))))
       productions))

(def (bounded-index? value count)
  (and (fixnum? value) (fx>= value 0) (fx< value count)))

(def (precedence? value)
  (or (not value)
      (match value
        ([(or 'none 'left 'right 'dynamic) (? integer?)] #t)
        (_ #f))))

(def (terminal? value)
  (or (equal? value +lr-eof+)
      (and (pair? value) (eq? (car value) 'terminal)
           (canonical-base-symbol? value))))

(def (make-action-validator states productions layout?)
  (def (reduce? value)
    (match value
      (['reduce id] (bounded-index? id productions))
      (_ #f)))
  (def (shift? value)
    (match value
      (['shift target precedence]
       (and (bounded-index? target states) (precedence? precedence)))
      (_ #f)))
  (def (atomic? value terminal)
    (or (shift? value) (reduce? value)
        (match value
          (['accept] (equal? terminal +lr-eof+))
          (['reject-nonassoc key (? integer?)] (equal? key terminal))
          (_ #f))))
  (def (fork? value branch?)
    ;; The producer flattens forks. Admit two or more atomic alternatives;
    ;; recursive wrappers are neither a table format nor a request protocol.
    (match value
      (['fork first second rest ...]
       (and (branch? first) (branch? second) (every branch? rest)))
      (_ #f)))
  (lambda (action terminal)
    (or (atomic? action terminal)
        (fork? action (lambda (branch) (atomic? branch terminal)))
        (match action
          (['layout-guard primary fallback]
           (and layout? (shift? primary) (or (reduce? fallback) (fork? fallback reduce?))))
          (_ #f)))))

(def (validate-rows rows key? value? kind)
  (for (state (in-range (vector-length rows)))
    (let ((row (vector-ref rows state)) (seen (make-table test: equal?)))
      (unless (list? row) (error "invalid LR table row" kind state row))
      (for-each
       (lambda (entry)
         (unless (and (pair? entry) (key? (car entry))
                      (not (table-ref seen (car entry) #f))
                      (value? (cdr entry) (car entry)))
           (error "invalid LR table entry" kind state entry))
         (table-set! seen (car entry) #t))
       row)))
  rows)

;;; Tables name only the vocabulary of their admitted production IR. Operand
;;; annotations are semantic metadata; membership uses the canonical base.
(def (production-vocabulary productions)
  (def nonterminals (make-table test: eq?))
  (for-each
   (lambda (production)
     (table-set! nonterminals (production-lhs production) #t))
   productions)
  (let-values (((_ terminals) (production-terminal-catalog productions)))
    (values terminals nonterminals)))

(def (for-each-shift-target visit action)
  (match action
    (['shift target _] (visit target))
    (['fork branches ...]
     (for-each (lambda (branch) (for-each-shift-target visit branch)) branches))
    (['layout-guard primary fallback]
     (for-each-shift-target visit primary)
     (for-each-shift-target visit fallback))
    (_ (void))))

(def (accepting-action? action)
  (match action
    (['accept] #t)
    (['fork branches ...] (any accepting-action? branches))
    (_ #f)))

;;; Conflict resolution can disconnect states of the original LR automaton.
;;; Retain their IDs, but require a reachable accepting state. This graph check
;;; is necessary, not proof that reductions have a viable parse stack.
(def (validate-accept-reachability actions gotos)
  (def states (vector-length actions))
  (def seen (make-vector states #f))
  (def pending (stdq-make-Queue))
  (def accepted? #f)
  (def (enqueue! state)
    (unless (vector-ref seen state)
      (vector-set! seen state #t)
      (stdq-enqueue! pending state)))
  (enqueue! 0)
  (let loop ()
    (unless (or accepted? (stdq-queue-empty? pending))
      (let (state (stdq-dequeue! pending))
        (set! accepted? (any (lambda (entry) (accepting-action? (cdr entry)))
                             (vector-ref actions state)))
        (for-each (lambda (entry) (for-each-shift-target enqueue! (cdr entry)))
                  (vector-ref actions state))
        (for-each (lambda (entry) (enqueue! (cdr entry))) (vector-ref gotos state)))
      (loop)))
  (unless accepted? (error "invalid LR table no reachable acceptance")))

(def (validate-lr-tables productions actions gotos)
  ;; Production semantics are admitted by each publishing owner first.
  (def production-count (length productions))
  (def layout? (layout-productions? productions))
  (unless (and (fixnum? production-count) (fx>= production-count 0)
               (vector? actions) (vector? gotos)
               (positive? (vector-length actions))
               (= (vector-length actions) (vector-length gotos)))
    (error "invalid LR table dimensions" production-count actions gotos))
  (let-values (((terminals nonterminals) (production-vocabulary productions)))
    (def states (vector-length actions))
    (validate-rows actions
                   (lambda (key)
                     (and (terminal? key)
                          (table-ref terminals key #f)
                          (or layout? (not (memq (cadr key) '(layout-start layout-next))))))
                   (make-action-validator states production-count layout?) 'actions)
    (validate-rows gotos
                   (lambda (key) (and (symbol? key) (table-ref nonterminals key #f)))
                   (lambda (target _) (bounded-index? target states)) 'gotos))
  (validate-accept-reachability actions gotos)
  (values actions gotos))
