;;; -*- Gerbil -*-
;;; Macro-generated parser-machine specialization.
;;; Grammar declarations become direct branches before the runtime hot path.

(import ../runtime/cst
        ../runtime/scan
        ../runtime/token)
(export defparser-machine
        parser-machine?
        parser-machine-ir
        parser-machine-lex
        parser-machine-trivia
        parser-machine-prefix
        parser-machine-binary
        parser-machine-parse)

(defstruct parser-machine (ir lex trivia prefix binary parse) transparent: #t)

;; The first admitted specialization recognizes the canonical expression
;; algebra shape. Every node kind, field, token, delimiter, and operator comes
;; from the grammar declaration; generated code contains no language names or
;; lexemes of its own.
(defrules defparser-machine
  (lexical-rules whitespace+ decimal-digit+ identifier literals fallback
   rules alias field reference choice seq literal token extras
   prefix-operators binary-operators)
  ((_ binding parser-ir
      (lexical-rules
       (whitespace-token (whitespace+))
       (number-lexical-token (decimal-digit+))
       (identifier-lexical-token (identifier))
       (punctuation-token (literals punctuation-literal ...))
       (unknown-token (fallback)))
      (rules
       (source-rule
        (alias source-kind
          (field source-field (reference source-expression-rule))))
       (expression-rule
        (choice
         (reference expression-prefix-rule)
         (reference expression-binary-rule)
         (reference expression-name-rule)
         (reference expression-number-rule)
         (seq (literal open-delimiter)
              (reference grouped-expression-rule)
              (literal close-delimiter))))
       (prefix-rule
        (alias prefix-node-kind
          (seq
           (field prefix-operator-field
             (choice (literal prefix-literal) ...))
           (field prefix-operand-field
             (reference prefix-operand-rule)))))
       (binary-rule
        (alias binary-node-kind
          (seq
           (field binary-left-field (reference binary-left-rule))
           (field binary-operator-field
             (choice (literal binary-literal) ...))
           (field binary-right-field (reference binary-right-rule)))))
       (name-rule
        (alias name-node-kind
          (field name-field (token name-token))))
       (number-rule
        (alias number-node-kind
          (field number-field (token number-token)))))
      (extras extra-name ...)
      (prefix-operators
       ((prefix-kind prefix-lexeme) prefix-precedence prefix-associativity) ...)
      (binary-operators
       ((binary-kind binary-lexeme) binary-precedence binary-associativity) ...))
   (def binding
     (letrec
         ((lex
           (lambda (source)
             (let (length (string-length source))
               (let loop ((offset 0) (byte-offset 0) (tokens '()))
                 (if (= offset length)
                   (reverse tokens)
                   (cond
                    ((scan-whitespace source offset)
                     => (lambda (end)
                          (let (output-token
                                (scan-emit source 'whitespace-token
                                           offset end byte-offset))
                            (loop end (token-end output-token)
                                  (cons output-token tokens)))))
                    ((scan-decimal-digits source offset)
                     => (lambda (end)
                          (let (output-token
                                (scan-emit source 'number-lexical-token
                                           offset end byte-offset))
                            (loop end (token-end output-token)
                                  (cons output-token tokens)))))
                    ((scan-identifier source offset)
                     => (lambda (end)
                          (let (output-token
                                (scan-emit source 'identifier-lexical-token
                                           offset end byte-offset))
                            (loop end (token-end output-token)
                                  (cons output-token tokens)))))
                    ((scan-longest-literal
                      source offset '(punctuation-literal ...))
                     => (lambda (matched)
                          (let (end (+ offset (string-length matched)))
                            (let (output-token
                                  (scan-emit source 'punctuation-token
                                             offset end byte-offset))
                              (loop end (token-end output-token)
                                    (cons output-token tokens))))))
                    (else
                     (let (end (+ offset 1))
                       (let (output-token
                             (scan-emit source 'unknown-token
                                        offset end byte-offset))
                         (loop end (token-end output-token)
                               (cons output-token tokens)))))))))))
          (trivia?
           (lambda (input-token)
             (memq (token-kind input-token) '(extra-name ...))))
          (prefix-row
           (lambda (input-token)
             (cond
              ((and (eq? (token-kind input-token) 'prefix-kind)
                    (string=? (token-lexeme input-token) prefix-lexeme))
               (list 'prefix-kind prefix-lexeme
                     prefix-precedence 'prefix-associativity)) ...
              (else #f))))
          (binary-row
           (lambda (input-token)
             (cond
              ((and (eq? (token-kind input-token) 'binary-kind)
                    (string=? (token-lexeme input-token) binary-lexeme))
               (list 'binary-kind binary-lexeme
                     binary-precedence 'binary-associativity)) ...
              (else #f))))
          (parse-expression
           (lambda (tokens minimum-precedence)
             (let-values (((left rest) (parse-prefix tokens)))
               (let loop ((lhs left) (remaining rest))
                 (if (null? remaining)
                   (values lhs remaining)
                   (let (row (binary-row (car remaining)))
                     (if (or (not row)
                             (< (caddr row) minimum-precedence))
                       (values lhs remaining)
                       (let* ((operator (car remaining))
                              (precedence (caddr row))
                              (associativity (cadddr row))
                              (right-minimum
                               (if (eq? associativity 'left)
                                 (+ precedence 1)
                                 precedence)))
                         (let-values
                             (((rhs tail)
                               (parse-expression
                                (cdr remaining) right-minimum)))
                           (loop
                            (make-syntax-node
                             'binary-node-kind
                             (syntax-node-start lhs)
                             (syntax-node-end rhs)
                             (cons 'binary-left-field lhs)
                             (cons 'binary-operator-field operator)
                             (cons 'binary-right-field rhs))
                            tail))))))))))
          (parse-prefix
           (lambda (tokens)
             (when (null? tokens) (error "expected expression"))
             (let* ((input-token (car tokens))
                    (prefix (prefix-row input-token)))
               (cond
                (prefix
                 (let-values
                     (((operand rest)
                       (parse-expression
                        (cdr tokens) (caddr prefix))))
                   (values
                    (make-syntax-node
                     'prefix-node-kind
                     (token-start input-token)
                     (syntax-node-end operand)
                     (cons 'prefix-operator-field input-token)
                     (cons 'prefix-operand-field operand))
                    rest)))
                ((eq? (token-kind input-token) 'number-token)
                 (values
                  (make-syntax-node
                   'number-node-kind
                   (token-start input-token)
                   (token-end input-token)
                   (cons 'number-field input-token))
                  (cdr tokens)))
                ((eq? (token-kind input-token) 'name-token)
                 (values
                  (make-syntax-node
                   'name-node-kind
                   (token-start input-token)
                   (token-end input-token)
                   (cons 'name-field input-token))
                  (cdr tokens)))
                ((string=? (token-lexeme input-token) open-delimiter)
                 (let-values
                     (((expression rest)
                       (parse-expression (cdr tokens) 0)))
                   (if (and (pair? rest)
                            (string=? (token-lexeme (car rest))
                                      close-delimiter))
                     (values expression (cdr rest))
                     (error "expected closing delimiter"
                            close-delimiter))))
                (else
                 (error "unexpected token"
                        (token-lexeme input-token)))))))
          (parse-root
           (lambda (tokens)
             (let-values (((expression rest)
                           (parse-expression tokens 0)))
               (values
                (make-syntax-node
                 'source-kind
                 (syntax-node-start expression)
                 (syntax-node-end expression)
                 (cons 'source-field expression))
                rest)))))
       (make-parser-machine parser-ir lex trivia?
                            prefix-row binary-row parse-root)))))
