;;; -*- Gerbil -*-
;;; Single declarative grammar-to-parser generation boundary.

(import ../modules/parser/objects
        ../modules/parser/funcs
        ../compiler/machine
        ./descriptor)
(export +language-grammar-schema-v1+
        deflanguage-grammar
        language-grammar?
        language-grammar-language
        language-grammar-version
        language-grammar-contract
        language-grammar-grammar
        language-grammar-ir
        language-grammar-machine)

;; This lower-level expander is deliberately private. Language packs have one
;; authoring surface: deflanguage-grammar.
(defrules assemble-language-parser
  (syntax-kinds terminals lexical-rules rules extras keywords
   parser-entrypoints recoveries flow)
  ((_ role-binding grammar-binding ir-binding parser-binding
      (syntax-kinds syntax-row ...)
      (terminals terminal-row ...)
      (lexical-rules lexical-row ...)
      (rules rule-row ...)
      (extras extra-name ...)
      (keywords keyword-row ...)
      (parser-entrypoints entry-row ...)
      (recoveries recovery-row ...)
      (flow flow-row ...))
   (begin
     (defgrammar-role role-binding
       (syntax-kinds syntax-row ...)
       (terminals terminal-row ...)
       (lexical-rules lexical-row ...)
       (rules rule-row ...)
       (extras extra-name ...)
       (keywords keyword-row ...)
       (parser-entrypoints entry-row ...)
       (recoveries recovery-row ...)
       (flow flow-row ...))
     (defgrammar grammar-binding
       (supers)
       (roles role-binding))
     (def ir-binding (compile-parser grammar-binding))
     (defgeneral-parser-machine parser-binding ir-binding
       (lexical-rules lexical-row ...)
       (rules rule-row ...)
       (extras extra-name ...)
       (parser-entrypoints entry-row ...)))))

;; Identity, grammar, canonical IR, and generated parser machine are owned by
;; one declaration. parser.ss consumes the descriptor and adds no authority.
(defsyntax (deflanguage-grammar stx)
  (syntax-case stx
      (identity syntax-kinds terminals lexical-rules rules extras keywords
                parser-entrypoints recoveries flow)
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
       (with-syntax ((role-binding (binding "-role"))
                     (grammar-binding (binding "-grammar"))
                     (ir-binding (binding "-parser-ir"))
                     (machine-binding (binding "-parser"))
                     (language-binding (binding "-language-grammar")))
         #'(begin
             (assemble-language-parser
              role-binding grammar-binding ir-binding machine-binding
              (syntax-kinds syntax-row ...)
              (terminals terminal-row ...)
              (lexical-rules lexical-row ...)
              (rules rule-row ...)
              (extras extra-name ...)
              (keywords keyword-row ...)
              (parser-entrypoints entry-row ...)
              (recoveries recovery-row ...)
              (flow flow-row ...))
             (def language-binding
               (make-language-grammar
                +language-grammar-schema-v1+
                language-value version-value contract-value
                grammar-binding ir-binding machine-binding))))))
    (_ (raise-syntax-error #f "invalid language grammar declaration" stx))))
