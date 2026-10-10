;;; -*- Gerbil -*-
;;; Canonical LR table admission before indexes or executable products exist.
(import (only-in :std/iter for in-range)
        (only-in ./lr +lr-eof+ canonical-base-symbol?))
(export validate-lr-tables)

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

(def (make-action-validator states productions)
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
           (and (shift? primary) (or (reduce? fallback) (fork? fallback reduce?))))
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

(def (validate-lr-tables production-count actions gotos)
  (unless (and (fixnum? production-count) (fx>= production-count 0)
               (vector? actions) (vector? gotos)
               (positive? (vector-length actions))
               (= (vector-length actions) (vector-length gotos)))
    (error "invalid LR table dimensions" production-count actions gotos))
  (let (states (vector-length actions))
    (validate-rows actions terminal? (make-action-validator states production-count) 'actions)
    (validate-rows gotos symbol?
                   (lambda (target _) (bounded-index? target states)) 'gotos))
  (values actions gotos))
