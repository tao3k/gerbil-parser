;;; -*- Gerbil -*-
;;; Thin execution boundary over an AOT-generated parser machine.

(import ../compiler/parser-ir
        ../compiler/generate
        ../modules/parser/funcs
        ./token
        ./lexer)
(export parse-source parse-receipt-ref parse-success? parse-roundtrip)

(def (parse-receipt-ref receipt key)
  (let (entry (assq key receipt))
    (and entry (cdr entry))))

(def (diagnostic machine condition)
  (let* ((recoveries (parser-ir-ref (parser-machine-ir machine) 'recoveries))
         (row (and (pair? recoveries) (car recoveries))))
    (list (cons 'schema "gerbil-parser.diagnostic.v1")
          (cons 'code (if row (cadr row) "GERBIL-PARSER-ERROR"))
          (cons 'message (error-message condition)))))

(def (parse-source machine source)
  (let (tokens (lex-source machine source))
    (with-catch
     (lambda (condition)
       (list (cons 'schema "gerbil-parser.parse-receipt.v1")
             (cons 'source source)
             (cons 'tokens tokens)
             (cons 'tree #f)
             (cons 'diagnostics (list (diagnostic machine condition)))))
     (lambda ()
       (let-values
           (((tree rest)
             ((parser-machine-parse machine)
              (parser-significant-tokens machine tokens))))
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
