;;; Macro-generated parser machine dispatch.
;;; Grammar declarations become direct branches before the runtime hot path.

(import ../runtime/token)
(export defparser-machine
        parser-machine?
        parser-machine-ir
        parser-machine-prefix
        parser-machine-binary)

(defstruct parser-machine (ir prefix binary) transparent: #t)

(defrules defparser-machine (prefix-operators binary-operators)
  ((_ binding parser-ir
      (prefix-operators
       ((prefix-kind prefix-lexeme) prefix-precedence prefix-associativity) ...)
      (binary-operators
       ((binary-kind binary-lexeme) binary-precedence binary-associativity) ...))
   (def binding
     (make-parser-machine
      parser-ir
      (lambda (token)
        (cond
         ((and (eq? (token-kind token) 'prefix-kind)
               (string=? (token-lexeme token) prefix-lexeme))
          (list 'prefix-kind prefix-lexeme
                prefix-precedence 'prefix-associativity)) ...
         (else #f)))
      (lambda (token)
        (cond
         ((and (eq? (token-kind token) 'binary-kind)
               (string=? (token-lexeme token) binary-lexeme))
          (list 'binary-kind binary-lexeme
                binary-precedence 'binary-associativity)) ...
         (else #f)))))))
