;;; Grammar IR to immutable parser-machine IR compilation.

(import ./normalize
        ../grammar/algebra
        ../grammar/lexical-algebra)
(export compile-parser
        parser-ir-ref
        parser-ir-canonical)

(def (require-root syntax-kinds)
  (if (null? syntax-kinds)
    (error "parser grammar requires a root syntax kind")
    (caar syntax-kinds)))

(def (row-names rows)
  (map car rows))

(def (require-known names known owner)
  (for-each
   (lambda (name)
     (unless (memq name known)
       (error "unresolved grammar identity" owner name)))
   names))

(def (validate-terminals terminals syntax-kinds)
  (for-each
   (lambda (terminal)
     (let* ((kind (cadr terminal))
            (syntax-kind (assq kind syntax-kinds)))
       (unless syntax-kind
         (error "terminal references an unknown syntax kind"
                (car terminal) kind))
       (unless (eq? (cadr syntax-kind) 'token)
         (error "terminal syntax kind must be categorized as token"
                (car terminal) kind))))
   terminals))

(def (validate-lexical-rules lexical-rules terminals)
  (let ((lexical-names (row-names lexical-rules))
        (terminal-names (row-names terminals)))
    (require-known lexical-names terminal-names 'lexical-rules)
    (require-known terminal-names lexical-names 'terminals)
    (for-each
     (lambda (row)
       (unless (lexical-expression? (cadr row))
         (error "invalid normalized lexical expression" (car row))))
     lexical-rules)))

(def (validate-rules rules terminals)
  (when (null? rules)
    (error "parser grammar requires at least one production"))
  (let ((rule-names (row-names rules))
        (terminal-names (row-names terminals)))
    (for-each
     (lambda (row)
       (let (expression (cadr row))
         (unless (grammar-expression? expression)
           (error "invalid normalized grammar production" (car row)))
         (require-known
          (grammar-expression-references expression)
          rule-names
          (car row))
         (require-known
          (grammar-expression-terminals expression)
          terminal-names
          (car row))))
     rules)))

(def (validate-extras extras terminals)
  (require-known (row-names extras) (row-names terminals) 'extras))

(def (flow-connected? flow)
  (and (pair? flow)
       (equal? (car flow) '(source lexical))
       (equal? (cadr (car (reverse flow))) 'cst)
       (let loop ((rest flow))
         (or (null? (cdr rest))
             (and (equal? (cadr (car rest)) (car (cadr rest)))
                  (loop (cdr rest)))))))

(def (require-entrypoint entrypoints rules)
  (when (null? entrypoints)
    (error "parser grammar requires an entrypoint"))
  (let (root-rule (caar entrypoints))
    (require-known (list root-rule) (row-names rules) 'entrypoint)
    root-rule))

(def (compile-parser grammar)
  (let* ((grammar-ir (compile-grammar grammar))
         (syntax-kinds (grammar-ir-ref grammar-ir 'syntax-kinds))
         (terminals (grammar-ir-ref grammar-ir 'terminals))
         (lexical-rules (grammar-ir-ref grammar-ir 'lexical-rules))
         (rules (grammar-ir-ref grammar-ir 'rules))
         (extras (grammar-ir-ref grammar-ir 'extras))
         (entrypoints (grammar-ir-ref grammar-ir 'parser-entrypoints))
         (flow (grammar-ir-ref grammar-ir 'flow)))
    (validate-terminals terminals syntax-kinds)
    (validate-lexical-rules lexical-rules terminals)
    (validate-rules rules terminals)
    (validate-extras extras terminals)
    (unless (flow-connected? flow)
      (error "parser flow must connect source through lexical to cst"))
    (list
     (cons 'schema "gerbil-parser.parser-ir.v1")
     (cons 'grammar (grammar-ir-ref grammar-ir 'grammar))
     (cons 'root-kind (require-root syntax-kinds))
     (cons 'root-rule (require-entrypoint entrypoints rules))
     (cons 'syntax-kinds syntax-kinds)
     (cons 'terminals terminals)
     (cons 'lexical-rules lexical-rules)
     (cons 'rules rules)
     (cons 'extras extras)
     (cons 'keywords (grammar-ir-ref grammar-ir 'keywords))
     (cons 'prefix-operators (grammar-ir-ref grammar-ir 'prefix-operators))
     (cons 'binary-operators (grammar-ir-ref grammar-ir 'binary-operators))
     (cons 'parser-entrypoints entrypoints)
     (cons 'recoveries (grammar-ir-ref grammar-ir 'recoveries))
     (cons 'flow flow))))

(def (parser-ir-ref ir key)
  (let (entry (assq key ir))
    (and entry (cdr entry))))

(def (parser-ir-canonical ir)
  (call-with-output-string
   (lambda (port) (write ir port))))
