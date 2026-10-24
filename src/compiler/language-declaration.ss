;;; Author-time service; runtime grammar interfaces retain no compiler imports.
(import (only-in ./parser-ir compile-parser)
        (only-in ./bound-ir bind-grammar-ir)
        (only-in ./concise-language expand-lexical-language-syntax)
        (only-in ./language-artifact expand-language-grammar-syntax))
(export expand-language-declaration expand-canonical-language-declaration)

;; Runtime syntax bindings remain owned by the public hygienic macro. Keeping
;; them as operands also prevents compiler-service bindings from leaking into
;; the generated parser's dependency graph.
(def (expand-language-declaration stx expand-text-rows
                                  assembly descriptor author begin-binding define-binding)
  (expand-lexical-language-syntax
   stx expand-text-rows compile-parser bind-grammar-ir
   assembly descriptor author begin-binding define-binding))

(def (expand-canonical-language-declaration stx assembly descriptor begin-binding define-binding)
  (expand-language-grammar-syntax
   stx compile-parser bind-grammar-ir assembly descriptor begin-binding define-binding))
