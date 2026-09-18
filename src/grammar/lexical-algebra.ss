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

(def (lexical-expressions? values)
  (let loop ((rest values))
    (or (null? rest)
        (and (lexical-expression? (car rest))
             (loop (cdr rest))))))

(def (lexical-expression? value)
  (and (list? value)
       (case (car value)
         ((whitespace+ horizontal-whitespace+ newline+
           decimal-digit+ number identifier heredoc fallback)
          (null? (cdr value)))
         ((number-literal)
          (and (= (length value) 6)
               (pair? (cadr value))
               (strings? (cadr value))
               (string? (caddr value))
               (= (string-length (caddr value)) 1)
               (strings? (cadddr value))
               (boolean? (car (cddddr value)))
               (boolean? (cadr (cddddr value)))))
         ((quoted-string)
          (and (pair? (cdr value)) (strings? (cdr value))))
         ((line-comment)
          (and (pair? (cdr value)) (strings? (cdr value))))
         ((block-comment nested-block-comment)
          (and (= (length value) 3) (strings? (cdr value))))
         ((literals)
          (and (pair? (cdr value))
               (strings? (cdr value))))
         ((choice)
          (and (pair? (cdr value))
               (lexical-expressions? (cdr value))))
         ((precedence)
          (and (= (length value) 3)
               (integer? (cadr value))
               (lexical-expression? (caddr value))))
         ((external)
          (and (= (length value) 3)
               (or (symbol? (cadr value)) (string? (cadr value)))
               (symbol? (caddr value))))
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

(def (lexical-precedence rank expression)
  (unless (integer? rank)
    (error "lexical precedence rank must be an integer" rank))
  (unless (lexical-expression? expression)
    (error "lexical precedence requires a lexical expression" expression))
  (list 'precedence rank expression))

(def (lexical-external version scanner)
  (unless (or (symbol? version) (string? version))
    (error "external scanner version must be a symbol or string" version))
  (unless (symbol? scanner)
    (error "external scanner identity must be a symbol" scanner))
  (list 'external version scanner))

;;; Expands the closed lexical algebra into validated canonical data.
;;; Scanner execution remains in runtime/scan.ss, outside the syntax phase.
;; lexical-expression
;;   : (-> Syntax Syntax)
;;   | doc m%
;;       `lexical-expression` expands one declarative lexical expression.
;;
;;       # Examples
;;
;;       ```scheme
;;       (lexical-expression (identifier))
;;       ;; => (identifier)
;;       ```
;;     %
(defrules lexical-expression
  (whitespace+ horizontal-whitespace+ newline+ decimal-digit+ number identifier
   heredoc number-literal
   quoted-string line-comment block-comment nested-block-comment
   choice literals fallback precedence external)
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
  ((_ (number-literal (prefix ...) separator (suffix ...)
                      leading-period? trailing-period?))
   (list 'number-literal (list prefix ...) separator (list suffix ...)
         leading-period? trailing-period?))
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
  ((_ (nested-block-comment start finish))
   (list 'nested-block-comment start finish))
  ((_ (choice expression ...))
   (cons 'choice (list (lexical-expression expression) ...)))
  ((_ (precedence rank expression))
   (lexical-precedence rank (lexical-expression expression)))
  ((_ (external version scanner))
   (lexical-external version 'scanner))
  ((_ (literals value ...))
   (lexical-literals (list value ...)))
  ((_ (fallback))
   (lexical-primitive 'fallback)))
