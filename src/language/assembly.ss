;;; -*- Gerbil -*-
;;; Internal assembly of already admitted immutable compiler products.

(import (only-in ../compiler/machine defgeneral-parser-machine parser-machine-ir)
        (only-in ../runtime/language-artifact load-compiled-language-artifact/embedded))
(export assemble-language-parser)

;; Internal runtime emission shared by native and imported declarations.
;; Expansion publishes grammar and backend instructions once. Native binding
;; loads the admitted common product and publishes the machine-owned instruction view.
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
      program-encoded program-payload
      (syntax-kinds (kind-name kind-category (field-name ...)) ...)
      (lexical-rules (lexical-name lexical-expression-value) ...)
      (rules (rule-name rule-expression) ...)
      (extras extra-name ...)
      (parser-entrypoints (entry-name entry-action entry-effect) ...)
      remainder ...)
   (begin
     (def grammar-binding
       (load-compiled-language-artifact/embedded
        "gerbil-parser.grammar-ir.v1" 'grammar-encoded 'grammar-payload))
     (def bound-binding
       (load-compiled-language-artifact/embedded
        "gerbil-parser.bound-grammar-ir.v1" 'bound-encoded 'bound-payload))
     (defgeneral-parser-machine parser-binding
       (load-compiled-language-artifact/embedded
        "gerbil-parser.contextual-parser-ir.v2" 'program-encoded 'program-payload)
       (lexical-rules (lexical-name lexical-expression-value) ...)
       (rules (rule-name rule-expression) ...)
       (extras extra-name ...)
       (parser-entrypoints
       (entry-name entry-action entry-effect) ...))
     (def ir-binding (parser-machine-ir parser-binding)))))

