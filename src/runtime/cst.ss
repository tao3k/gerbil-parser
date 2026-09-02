;;; -*- Gerbil -*-
;;; Backend-neutral immutable CST node values.

(export make-syntax-node
        syntax-node-kind
        syntax-node-start
        syntax-node-end
        syntax-node-fields)

(def (make-syntax-node kind start end . fields)
  (unless (symbol? kind)
    (error "syntax node kind must be a symbol" kind))
  (unless (and (integer? start) (integer? end) (<= 0 start end))
    (error "invalid syntax node span" kind start end))
  (list (cons 'kind kind)
        (cons 'start start)
        (cons 'end end)
        (cons 'fields fields)))

(def (syntax-node-kind value) (cdr (assq 'kind value)))
(def (syntax-node-start value) (cdr (assq 'start value)))
(def (syntax-node-end value) (cdr (assq 'end value)))
(def (syntax-node-fields value) (cdr (assq 'fields value)))
