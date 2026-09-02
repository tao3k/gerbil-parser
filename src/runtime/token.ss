;;; Lossless source token representation.

(export make-token token-kind token-lexeme token-start token-end tokens-source)

(def (make-token kind lexeme start end)
  (vector kind lexeme start end))

(def (token-kind token) (vector-ref token 0))
(def (token-lexeme token) (vector-ref token 1))
(def (token-start token) (vector-ref token 2))
(def (token-end token) (vector-ref token 3))
(def (tokens-source tokens)
  (call-with-output-string
   (lambda (port)
     (for-each (lambda (token) (display (token-lexeme token) port)) tokens))))
