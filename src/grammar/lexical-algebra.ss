;;; -*- Gerbil -*-
;;; Canonical LexicalExpr v1 constructors.

(export lexical-expression
        lexical-expression?)

(def (strings? values)
  (let loop ((rest values))
    (or (null? rest)
        (and (string? (car rest))
             (positive? (string-length (car rest)))
             (loop (cdr rest))))))

(def (lexical-expression? value)
  (and (list? value)
       (case (car value)
         ((whitespace+ decimal-digit+ identifier fallback)
          (null? (cdr value)))
         ((literals)
          (and (pair? (cdr value))
               (strings? (cdr value))))
         (else #f))))

(def (lexical-primitive kind)
  (unless (memq kind '(whitespace+ decimal-digit+ identifier fallback))
    (error "unknown lexical primitive" kind))
  (list kind))

(def (lexical-literals values)
  (unless (and (pair? values) (strings? values))
    (error "lexical literals require non-empty strings" values))
  (cons 'literals values))

(defrules lexical-expression
  (whitespace+ decimal-digit+ identifier literals fallback)
  ((_ (whitespace+))
   (lexical-primitive 'whitespace+))
  ((_ (decimal-digit+))
   (lexical-primitive 'decimal-digit+))
  ((_ (identifier))
   (lexical-primitive 'identifier))
  ((_ (literals value ...))
   (lexical-literals (list value ...)))
  ((_ (fallback))
   (lexical-primitive 'fallback)))
