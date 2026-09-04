;;; -*- Gerbil -*-
;;; Single declarative grammar-to-parser generation boundary.

(import (for-syntax ../compiler/parser-ir)
        ../compiler/machine
        ./descriptor)
(export deflanguage-grammar)

;; This lower-level expander is deliberately private. Language packs have one
;; authoring surface: deflanguage-grammar.
(defrules assemble-language-parser
  (syntax-kinds terminals lexical-rules rules extras keywords
   parser-entrypoints recoveries flow)
  ((_ grammar-binding ir-binding parser-binding grammar-value ir-value
      (syntax-kinds (kind-name kind-category (field-name ...)) ...)
      (lexical-rules (lexical-name lexical-expression-value) ...)
      (rules (rule-name rule-expression) ...)
      (extras extra-name ...)
      (parser-entrypoints (entry-name entry-action entry-effect) ...)
      remainder ...)
   (begin
     (def grammar-binding 'grammar-value)
     (def ir-binding 'ir-value)
     (defgeneral-parser-machine parser-binding ir-binding
       (lexical-rules (lexical-name lexical-expression-value) ...)
       (rules (rule-name rule-expression) ...)
       (extras extra-name ...)
       (parser-entrypoints
       (entry-name entry-action entry-effect) ...)))))

(defsyntax (deflanguage-grammar stx)
  (def (grammar-expression-datum expression)
    (unless (and (pair? expression) (symbol? (car expression)))
      (raise-syntax-error #f "invalid GrammarExpr declaration" stx))
    (case (car expression)
      ((empty literal token reference) expression)
      ((seq sequence)
       (cons 'sequence (map grammar-expression-datum (cdr expression))))
      ((choice)
       (cons 'choice (map grammar-expression-datum (cdr expression))))
      ((optional repeat repeat1)
       (list (car expression)
             (grammar-expression-datum (cadr expression))))
      ((field alias)
       (list (car expression) (cadr expression)
             (grammar-expression-datum (caddr expression))))
      ((prec precedence)
       (list 'precedence (cadr expression) (caddr expression)
             (grammar-expression-datum (cadddr expression))))
      (else
       (raise-syntax-error #f "unknown GrammarExpr constructor" stx))))
  (def (compile-language-declaration
        grammar-name syntax-rows terminal-rows lexical-rows rule-rows
        extra-names keyword-rows entry-rows recovery-rows flow-rows
        conflict-policy case-insensitive?)
     (let* ((grammar
             (list
              (cons 'schema "gerbil-parser.grammar-ir.v1")
              (cons 'grammar (syntax->datum grammar-name))
              (cons 'syntax-kinds (syntax->datum syntax-rows))
              (cons 'terminals (syntax->datum terminal-rows))
              (cons 'lexical-rules (syntax->datum lexical-rows))
              (cons 'rules
                    (map (lambda (row)
                           (list (car row)
                                 (grammar-expression-datum (cadr row))))
                         (syntax->datum rule-rows)))
              (cons 'extras
                    (map list (syntax->datum extra-names)))
              (cons 'keywords (syntax->datum keyword-rows))
              (cons 'parser-entrypoints (syntax->datum entry-rows))
              (cons 'recoveries (syntax->datum recovery-rows))
              (cons 'conflict-policy conflict-policy)
              (cons 'case-insensitive? case-insensitive?)
              (cons 'flow (syntax->datum flow-rows))))
            (compiled (compile-parser grammar))
            (materialized
             (cons (car compiled)
                   (cons '(materialization . aot-expansion)
                         (cdr compiled)))))
       (values grammar materialized)))

;; Identity, grammar, canonical IR, and generated parser machine are owned by
;; one declaration. parser.ss consumes the descriptor and adds no authority.
  (syntax-case stx
      (identity syntax-kinds terminals lexical-rules rules extras keywords
                parser-entrypoints recoveries conflicts case-insensitive flow)
    ((_ prefix
        (identity language-value version-value contract-value)
        (syntax-kinds syntax-row ...)
        (terminals terminal-row ...)
        (lexical-rules lexical-row ...)
        (rules rule-row ...)
        (extras extra-name ...)
        (keywords keyword-row ...)
        (parser-entrypoints entry-row ...)
        (recoveries recovery-row ...)
        (conflicts conflict-policy-value)
        (case-insensitive case-insensitive-value)
        (flow flow-row ...))
     (identifier? #'prefix)
     (let* ((stem (symbol->string (syntax->datum #'prefix)))
            (binding
             (lambda (suffix)
               (datum->syntax
                #'prefix
                (string->symbol (string-append stem suffix))))))
       (with-syntax ((grammar-binding (binding "-grammar"))
                     (ir-binding (binding "-parser-ir"))
                     (machine-binding (binding "-parser"))
                     (language-binding (binding "-language-grammar")))
         (let-values (((grammar-value ir-value)
                       (compile-language-declaration
                        #'grammar-binding
                        #'(syntax-row ...)
                        #'(terminal-row ...)
                        #'(lexical-row ...)
                        #'(rule-row ...)
                        #'(extra-name ...)
                        #'(keyword-row ...)
                        #'(entry-row ...)
                        #'(recovery-row ...)
                        #'(flow-row ...)
                        (syntax->datum #'conflict-policy-value)
                        (syntax->datum #'case-insensitive-value))))
           (with-syntax ((grammar-value grammar-value)
                         (ir-value ir-value))
             #'(begin
                 (assemble-language-parser
                  grammar-binding ir-binding machine-binding
                  grammar-value ir-value
                  (syntax-kinds syntax-row ...)
                  (lexical-rules lexical-row ...)
                  (rules rule-row ...)
                  (extras extra-name ...)
                  (parser-entrypoints entry-row ...))
                 (def language-binding
                   (make-language-grammar
                    "gerbil-parser.language-grammar.v1"
                    language-value version-value contract-value
                    grammar-binding ir-binding machine-binding))))))))
    ((_ prefix
        (identity language-value version-value contract-value)
        (syntax-kinds syntax-row ...)
        (terminals terminal-row ...)
        (lexical-rules lexical-row ...)
        (rules rule-row ...)
        (extras extra-name ...)
        (keywords keyword-row ...)
        (parser-entrypoints entry-row ...)
        (recoveries recovery-row ...)
        (flow flow-row ...))
     (identifier? #'prefix)
     (let* ((stem (symbol->string (syntax->datum #'prefix)))
            (binding
             (lambda (suffix)
               (datum->syntax
                #'prefix
                (string->symbol (string-append stem suffix))))))
       (with-syntax ((grammar-binding (binding "-grammar"))
                     (ir-binding (binding "-parser-ir"))
                     (machine-binding (binding "-parser"))
                     (language-binding (binding "-language-grammar")))
         (let-values (((grammar-value ir-value)
                       (compile-language-declaration
                        #'grammar-binding
                        #'(syntax-row ...)
                        #'(terminal-row ...)
                        #'(lexical-row ...)
                        #'(rule-row ...)
                        #'(extra-name ...)
                        #'(keyword-row ...)
                        #'(entry-row ...)
                        #'(recovery-row ...)
                        #'(flow-row ...)
                        'reject #f)))
           (with-syntax ((grammar-value grammar-value)
                         (ir-value ir-value))
             #'(begin
                 (assemble-language-parser
                  grammar-binding ir-binding machine-binding
                  grammar-value ir-value
                  (syntax-kinds syntax-row ...)
                  (lexical-rules lexical-row ...)
                  (rules rule-row ...)
                  (extras extra-name ...)
                  (parser-entrypoints entry-row ...))
                 (def language-binding
                   (make-language-grammar
                    "gerbil-parser.language-grammar.v1"
                    language-value version-value contract-value
                    grammar-binding ir-binding machine-binding))))))))
    (_ (raise-syntax-error #f "invalid language grammar declaration" stx))))
