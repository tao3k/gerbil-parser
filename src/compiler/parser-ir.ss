;;; Grammar IR to immutable parser-machine IR compilation.

(import ./normalize)
(export compile-parser
        parser-ir-ref
        parser-ir-canonical)

(def (require-root syntax-kinds)
  (if (null? syntax-kinds)
    (error "parser grammar requires a root syntax kind")
    (caar syntax-kinds)))

(def (compile-parser grammar)
  (let* ((grammar-ir (compile-grammar grammar))
         (syntax-kinds (grammar-ir-ref grammar-ir 'syntax-kinds))
         (flow (grammar-ir-ref grammar-ir 'flow)))
    (when (or (null? flow)
              (not (equal? (car flow) '(source lexical)))
              (not (equal? (car (reverse flow)) '(expression cst))))
      (error "parser flow must connect source through lexical to cst"))
    (list
     (cons 'schema "gerbil-parser.parser-ir.v1")
     (cons 'grammar (grammar-ir-ref grammar-ir 'grammar))
     (cons 'root-kind (require-root syntax-kinds))
     (cons 'syntax-kinds syntax-kinds)
     (cons 'keywords (grammar-ir-ref grammar-ir 'keywords))
     (cons 'prefix-operators (grammar-ir-ref grammar-ir 'prefix-operators))
     (cons 'binary-operators (grammar-ir-ref grammar-ir 'binary-operators))
     (cons 'parser-entrypoints (grammar-ir-ref grammar-ir 'parser-entrypoints))
     (cons 'recoveries (grammar-ir-ref grammar-ir 'recoveries))
     (cons 'flow flow))))

(def (parser-ir-ref ir key)
  (let (entry (assq key ir))
    (and entry (cdr entry))))

(def (parser-ir-canonical ir)
  (call-with-output-string
   (lambda (port) (write ir port))))
