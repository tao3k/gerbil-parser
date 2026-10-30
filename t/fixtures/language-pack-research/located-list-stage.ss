;;; -*- Gerbil -*-
;;; Research adapter: phase-1 POO composition -> existing verbose declaration.
(import (only-in "../../../src/compiler/language-expander" compile-language)
        (for-syntax "located-list-roles"
                    (only-in "../../../src/compiler/normalize"
                             compile-grammar/receipt grammar-ir-ref)))
(export deflocated-list-study)

(defsyntax (deflocated-list-study stx)
  (syntax-case stx ()
    ((_ prefix composed-binding receipt-binding)
     (let-values (((ir receipt) (compile-grammar/receipt (make-located-list-grammar))))
       (with-syntax
           ((composed-value (datum->syntax #'prefix (list 'quote ir)))
            (receipt-value (datum->syntax #'prefix (list 'quote receipt)))
            (source-map (datum->syntax #'prefix (located-list-source-map receipt)))
            ((kind-row ...) (datum->syntax #'prefix (grammar-ir-ref ir 'syntax-kinds)))
            ((terminal-row ...) (datum->syntax #'prefix (grammar-ir-ref ir 'terminals)))
            ((lexical-row ...) (datum->syntax #'prefix (grammar-ir-ref ir 'lexical-rules)))
            ((rule-row ...) (datum->syntax #'prefix (grammar-ir-ref ir 'rules)))
            ((extra ...) (datum->syntax #'prefix (map car (grammar-ir-ref ir 'extras))))
            ((entry-row ...) (datum->syntax #'prefix (grammar-ir-ref ir 'parser-entrypoints)))
            ((flow-row ...) (datum->syntax #'prefix (grammar-ir-ref ir 'flow))))
         #'(begin
             (def composed-binding composed-value)
             (def receipt-binding receipt-value)
             (compile-language prefix
               (identity "list-study" "v1" "list-study.local.v1")
               (syntax-kinds kind-row ...)
               (terminals terminal-row ...)
               (lexical-rules lexical-row ...)
               (rules rule-row ...)
               (extras extra ...) (keywords)
               (parser-entrypoints entry-row ...)
               (recoveries)
               (source-ownership source-map)
               (lineage deflocated-list-study deflocated-list make-nonempty-list-role)
               (flow flow-row ...))))))))
