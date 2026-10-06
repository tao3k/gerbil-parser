;;; -*- Gerbil -*-
;;; Internal assembly of already admitted immutable compiler products.

(import (only-in ../compiler/machine defgeneral-parser-machine)
        (only-in ../runtime/language-artifact load-compiled-language-artifact/embedded))
(export assemble-language-parser)

;; Internal runtime emission shared by native and imported declarations.
;; Expansion materializes immutable grammar and parser IR once; runtime code
;; only receives the generated machine and never invokes the compiler.
;; assemble-language-parser
;;   : (-> Syntax Syntax)
;;   | doc m%
;;       `assemble-language-parser` expands validated grammar data into bindings.
;;
;;       # Examples
;;
;;       ```scheme
;;       (assemble-language-parser grammar ir parser grammar-data ir-data ...)
;;       ;; => immutable grammar, IR, and parser bindings
;;       ```
;;     %
(defrules assemble-language-parser
  (syntax-kinds terminals lexical-rules rules extras keywords
   parser-entrypoints recoveries flow)
  ((_ grammar-binding bound-binding ir-binding parser-binding
      grammar-encoded grammar-payload
      bound-encoded bound-payload
      ir-encoded ir-payload
      (syntax-kinds (kind-name kind-category (field-name ...)) ...)
      (lexical-rules (lexical-name lexical-expression-value) ...)
      (rules (rule-name rule-expression) ...)
      (extras extra-name ...)
      (parser-entrypoints (entry-name entry-action entry-effect) ...)
      remainder ...)
   (begin
     (def grammar-binding
       (load-compiled-language-artifact/embedded
        "gerbil-parser.grammar-ir.v1" 'grammar-encoded grammar-payload))
     (def bound-binding
       (load-compiled-language-artifact/embedded
        "gerbil-parser.bound-grammar-ir.v1" 'bound-encoded bound-payload))
     (def ir-binding
       (load-compiled-language-artifact/embedded
        "gerbil-parser.parser-ir.v1" 'ir-encoded ir-payload))
     (defgeneral-parser-machine parser-binding ir-binding
       (grammar-digest (cadr 'ir-encoded))
       (lexical-rules (lexical-name lexical-expression-value) ...)
       (rules (rule-name rule-expression) ...)
       (extras extra-name ...)
       (parser-entrypoints
       (entry-name entry-action entry-effect) ...)))))

