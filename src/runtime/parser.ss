;;; -*- Gerbil -*-
;;; Thin request boundary over generated machines and ParseArtifact admission.

(import ../compiler/parser-ir
        ../compiler/machine
        ../modules/parser/funcs
        ./artifact
        ./lexer
        ./token)
(export parse-source)

(def (diagnostic machine condition)
  (let* ((recoveries (parser-ir-ref (parser-machine-ir machine) 'recoveries))
         (row (and (pair? recoveries) (car recoveries))))
    (list (cons 'schema +diagnostic-schema-v1+)
          (cons 'code (if row (cadr row) "GERBIL-PARSER-ERROR"))
          (cons 'reasonKind 'parse-rejected)
          (cons 'message (error-message condition)))))

(def (parse-source machine source)
  (unless (string? source)
    (error "parse source must be a string" source))
  (let ((grammar-digest (parser-machine-grammar-digest machine))
        (tokens '()))
    (with-catch
     (lambda (condition)
       (make-failure-parse-artifact
        grammar-digest source tokens (diagnostic machine condition)))
     (lambda ()
       (set! tokens (lex-source machine source))
       (let-values
           (((root rest)
             ((parser-machine-parse machine)
              (parser-significant-tokens machine tokens))))
         (unless (null? rest)
           (error "unexpected trailing token" (token-lexeme (car rest))))
         (make-success-parse-artifact
          grammar-digest source tokens root
          (parser-machine-trivia machine)))))))
