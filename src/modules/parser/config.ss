;;; Hygienic parser-module assembly.
;;; One declaration owns both canonical IR and generated hot-path dispatch.

(import ./objects
        ./funcs
        ../../compiler/machine)
(export defparser-config)

(defrules defparser-config
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
