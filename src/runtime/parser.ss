;;; -*- Gerbil -*-
;;; Thin request boundary over generated machines and ParseArtifact admission.

(import ../compiler/parser-ir
        ../compiler/machine
        ./artifact
        ./lexer
        ./significant
        ./token)
(export parse-source)

(def (diagnostic machine condition)
  (let* ((recoveries (parser-ir-ref (parser-machine-ir machine) 'recoveries))
         (row (and (pair? recoveries) (car recoveries))))
    (list (cons 'schema +diagnostic-schema-v1+)
          (cons 'code (if row (cadr row) "GERBIL-PARSER-ERROR"))
          (cons 'reasonKind 'parse-rejected)
          (cons 'message (error-message condition)))))

(def (failure-artifact machine grammar-digest source tokens condition)
  (make-failure-parse-artifact
   grammar-digest source tokens (diagnostic machine condition)))

(def (parse-tokenized machine grammar-digest source tokens)
  (with-catch
   (lambda (condition)
     (failure-artifact machine grammar-digest source tokens condition))
   (lambda ()
     (let-values
         (((root rest)
           ((parser-machine-parse machine)
            (parser-significant-tokens machine tokens))))
       (unless (null? rest)
         (error "unexpected trailing token" (token-lexeme (car rest))))
       (make-success-parse-artifact
        grammar-digest source tokens root
        (parser-machine-trivia machine))))))

(def (parse-source machine source)
  (unless (string? source)
    (error "parse source must be a string" source))
  (let (grammar-digest (parser-machine-grammar-digest machine))
    ;; Lexing is atomic: a lexer either returns its immutable complete token
    ;; list or throws before publication.  Keeping its failure boundary
    ;; separate avoids boxing a mutable token accumulator across the parser's
    ;; exception continuation on every successful request.
    (with-catch
     (lambda (condition)
       (failure-artifact machine grammar-digest source '() condition))
     (lambda ()
       (parse-tokenized machine grammar-digest source
                        (lex-source machine source))))))
