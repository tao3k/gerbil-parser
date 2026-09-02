;;; Tight parser-specific functions shared by generated machines.

(import ../../compiler/normalize
        ../../compiler/parser-ir
        ../../compiler/generate
        ../../runtime/token
        ../../runtime/lexer)
(export (import: ../../compiler/normalize)
        (import: ../../compiler/parser-ir)
        (import: ../../compiler/generate)
        (import: ../../runtime/token)
        (import: ../../runtime/lexer)
        parser-operator-row
        parser-significant-tokens
        make-syntax-node
        syntax-node-start
        syntax-node-end)

(def (parser-operator-row machine section token)
  ((if (eq? section 'prefix-operators)
     (parser-machine-prefix machine)
     (parser-machine-binary machine))
   token))

(def (parser-significant-tokens tokens)
  (let loop ((rest tokens) (found '()))
    (cond
     ((null? rest) (reverse found))
     ((token-trivia? (car rest)) (loop (cdr rest) found))
     (else (loop (cdr rest) (cons (car rest) found))))))

(def (make-syntax-node kind start end . fields)
  (list (cons 'kind kind)
        (cons 'start start)
        (cons 'end end)
        (cons 'fields fields)))

(def (syntax-node-start value) (cdr (assq 'start value)))
(def (syntax-node-end value) (cdr (assq 'end value)))
