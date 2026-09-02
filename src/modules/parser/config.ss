;;; Hygienic parser-module assembly.
;;; One declaration owns both canonical IR and generated hot-path dispatch.

(import ./objects
        ./funcs)
(export defparser-config)

(defrules defparser-config
  (syntax-kinds keywords prefix-operators binary-operators
   parser-entrypoints recoveries flow)
  ((_ role-binding grammar-binding ir-binding parser-binding
      (syntax-kinds syntax-row ...)
      (keywords keyword-row ...)
      (prefix-operators prefix-row ...)
      (binary-operators binary-row ...)
      (parser-entrypoints entry-row ...)
      (recoveries recovery-row ...)
      (flow flow-row ...))
   (begin
     (defgrammar-role role-binding
       (syntax-kinds syntax-row ...)
       (keywords keyword-row ...)
       (prefix-operators prefix-row ...)
       (binary-operators binary-row ...)
       (parser-entrypoints entry-row ...)
       (recoveries recovery-row ...)
       (flow flow-row ...))
     (defgrammar grammar-binding
       (supers)
       (roles role-binding))
     (def ir-binding (compile-parser grammar-binding))
     (defparser-machine parser-binding ir-binding
       (prefix-operators prefix-row ...)
       (binary-operators binary-row ...)))))
