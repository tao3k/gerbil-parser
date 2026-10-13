;;; Lossless source token representation.
;;; Official inline contracts stage direct calls without changing the plain
;;; vector protocol, first-class procedures or checked vector reads.

(export make-token token? token-kind token-lexeme token-start token-end)

(definline (make-token (kind : :t) (lexeme : :t) (start : :t) (end : :t)) => :t
  (vector kind lexeme start end))

(definline (token? (value : :t)) => :t
  (and (vector? value)
       (= (vector-length value) 4)
       (symbol? (vector-ref value 0))
       (string? (vector-ref value 1))
       (integer? (vector-ref value 2))
       (integer? (vector-ref value 3))
       (<= 0 (vector-ref value 2) (vector-ref value 3))))

(definline (token-kind (token : :t)) => :t (vector-ref token 0))
(definline (token-lexeme (token : :t)) => :t (vector-ref token 1))
(definline (token-start (token : :t)) => :t (vector-ref token 2))
(definline (token-end (token : :t)) => :t (vector-ref token 3))
