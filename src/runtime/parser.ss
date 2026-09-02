;;; Pure Scheme Pratt runtime. POO is deliberately absent from this hot path.

(import ../compiler/parser-ir
        ../compiler/generate
        ../modules/parser/funcs
        ./token
        ./lexer)
(export parse-source parse-receipt-ref parse-success? parse-roundtrip)

(def (parse-receipt-ref receipt key)
  (let (entry (assq key receipt))
    (and entry (cdr entry))))

(def (parse-expression machine tokens minimum-precedence)
  (let-values (((left rest) (parse-prefix machine tokens)))
    (let loop ((lhs left) (remaining rest))
      (if (null? remaining)
        (values lhs remaining)
        (let (row (parser-operator-row machine 'binary-operators (car remaining)))
          (if (or (not row) (< (caddr row) minimum-precedence))
            (values lhs remaining)
            (let* ((operator (car remaining))
                   (precedence (caddr row))
                   (associativity (cadddr row))
                   (right-minimum
                    (if (eq? associativity 'left) (+ precedence 1) precedence)))
              (let-values (((rhs tail)
                            (parse-expression machine (cdr remaining) right-minimum)))
                (loop (make-syntax-node 'BinaryExpression
                            (syntax-node-start lhs)
                            (syntax-node-end rhs)
                            lhs operator rhs)
                      tail)))))))))

(def (parse-prefix machine tokens)
  (when (null? tokens) (error "expected expression"))
  (let* ((token (car tokens))
         (prefix (parser-operator-row machine 'prefix-operators token)))
    (cond
     (prefix
      (let-values (((operand rest)
                    (parse-expression machine (cdr tokens) (caddr prefix))))
        (values (make-syntax-node 'PrefixExpression
                      (token-start token) (syntax-node-end operand)
                      token operand)
                rest)))
     ((eq? (token-kind token) 'number)
      (values (make-syntax-node 'NumberExpression
                    (token-start token) (token-end token) token)
              (cdr tokens)))
     ((eq? (token-kind token) 'identifier)
      (values (make-syntax-node 'NameExpression
                    (token-start token) (token-end token) token)
              (cdr tokens)))
     ((and (eq? (token-kind token) 'punctuation)
           (string=? (token-lexeme token) "("))
      (let-values (((expression rest)
                    (parse-expression machine (cdr tokens) 0)))
        (if (and (pair? rest)
                 (string=? (token-lexeme (car rest)) ")"))
          (values expression (cdr rest))
          (error "expected closing parenthesis"))))
     (else (error "unexpected token" (token-lexeme token))))))

(def (diagnostic machine condition)
  (let* ((recoveries (parser-ir-ref (parser-machine-ir machine) 'recoveries))
         (row (and (pair? recoveries) (car recoveries))))
    (list (cons 'schema "gerbil-parser.diagnostic.v1")
          (cons 'code (if row (cadr row) "GERBIL-PARSER-ERROR"))
          (cons 'message (error-message condition)))))

(def (parse-source machine source)
  (let (tokens (lex-source source))
    (with-catch
     (lambda (condition)
       (list (cons 'schema "gerbil-parser.parse-receipt.v1")
             (cons 'source source)
             (cons 'tokens tokens)
             (cons 'tree #f)
             (cons 'diagnostics (list (diagnostic machine condition)))))
     (lambda ()
       (let-values (((tree rest)
                     (parse-expression machine
                                       (parser-significant-tokens tokens)
                                       0)))
         (unless (null? rest)
           (error "unexpected trailing token" (token-lexeme (car rest))))
         (list (cons 'schema "gerbil-parser.parse-receipt.v1")
               (cons 'source source)
               (cons 'tokens tokens)
               (cons 'tree tree)
               (cons 'diagnostics '())))))))

(def (parse-success? receipt)
  (null? (parse-receipt-ref receipt 'diagnostics)))

(def (parse-roundtrip receipt)
  (tokens-source (parse-receipt-ref receipt 'tokens)))
