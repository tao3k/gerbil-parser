;;; Lossless source token representation.

(export make-token token? token-kind token-lexeme token-start token-end)

(def (make-token kind lexeme start end)
  (vector kind lexeme start end))

(def (token? value)
  (and (vector? value)
       (= (vector-length value) 4)
       (symbol? (vector-ref value 0))
       (string? (vector-ref value 1))
       (integer? (vector-ref value 2))
       (integer? (vector-ref value 3))
       (<= 0 (vector-ref value 2) (vector-ref value 3))))

(def (token-kind token) (vector-ref token 0))
(def (token-lexeme token) (vector-ref token 1))
(def (token-start token) (vector-ref token 2))
(def (token-end token) (vector-ref token 3))
