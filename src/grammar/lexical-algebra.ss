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
         ((whitespace+ horizontal-whitespace+ newline+
           decimal-digit+ number identifier heredoc fallback)
          (null? (cdr value)))
         ((quoted-string)
          (and (pair? (cdr value)) (strings? (cdr value))))
         ((line-comment)
          (and (pair? (cdr value)) (strings? (cdr value))))
         ((block-comment)
          (and (= (length value) 3) (strings? (cdr value))))
         ((literals)
          (and (pair? (cdr value))
               (strings? (cdr value))))
         (else #f))))

(def (lexical-primitive kind)
  (unless (memq kind '(whitespace+ horizontal-whitespace+ newline+
                       decimal-digit+ number identifier heredoc fallback))
    (error "unknown lexical primitive" kind))
  (list kind))

(def (lexical-literals values)
  (unless (and (pair? values) (strings? values))
    (error "lexical literals require non-empty strings" values))
  (cons 'literals values))

(defrules lexical-expression
  (whitespace+ horizontal-whitespace+ newline+ decimal-digit+ number identifier
   heredoc
   quoted-string line-comment block-comment literals fallback)
  ((_ (whitespace+))
   (lexical-primitive 'whitespace+))
  ((_ (horizontal-whitespace+))
   (lexical-primitive 'horizontal-whitespace+))
  ((_ (newline+))
   (lexical-primitive 'newline+))
  ((_ (decimal-digit+))
   (lexical-primitive 'decimal-digit+))
  ((_ (number))
   (lexical-primitive 'number))
  ((_ (identifier))
   (lexical-primitive 'identifier))
  ((_ (quoted-string delimiter ...))
   (cons 'quoted-string (list delimiter ...)))
  ((_ (heredoc))
   (lexical-primitive 'heredoc))
  ((_ (line-comment start ...))
   (cons 'line-comment (list start ...)))
  ((_ (block-comment start finish))
   (list 'block-comment start finish))
  ((_ (literals value ...))
   (lexical-literals (list value ...)))
  ((_ (fallback))
   (lexical-primitive 'fallback)))
