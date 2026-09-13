;;; -*- Gerbil -*-
;;; Boundary: hygienic declarations for checked parser POO objects.
;;; Invariant: syntax validation lowers once to named constructors; the macro
;;; layer owns no runtime merge, lookup, validation, or projection behavior.

(import (only-in ./objects make-grammar-role make-grammar)
        (only-in ../../grammar/algebra grammar-expression)
        (only-in ../../grammar/lexical-algebra lexical-expression))
(export defgrammar-role
        defgrammar
        defgrammar-compose)

;; defgrammar-role
;;   : (-> Syntax Syntax)
;;   | contract: accepts one complete role declaration and lowers it to the
;;       checked GrammarRole constructor without hiding declaration sections.
;;   | doc m%
;;       `defgrammar-role` expands one declarative grammar role.
;;
;;       # Examples
;;
;;       ```scheme
;;       (defgrammar-role core-role ...)
;;       ;; => admitted GrammarRole
;;       ```
;;     %
(defsyntax (defgrammar-role stx)
  (syntax-case stx
      (syntax-kinds terminals lexical-rules rules extras keywords
                    parser-entrypoints recoveries flow)
    ((_ binding
        (syntax-kinds (kind-name kind-category (field-name ...)) ...)
        (terminals (terminal-name terminal-kind) ...)
        (lexical-rules (lexical-name lexical-expression-value) ...)
        (rules (rule-name rule-expression) ...)
        (extras extra-name ...)
        (keywords (keyword-name keyword-text) ...)
        (parser-entrypoints (entry-keyword entry-action entry-effect) ...)
        (recoveries (recovery-site recovery-code recovery-strategy) ...)
        (flow (flow-source flow-target) ...))
     (identifier? #'binding)
     #'(def binding
         (make-grammar-role
          'binding
          (list (list 'kind-name 'kind-category (list 'field-name ...)) ...)
          (list (list 'terminal-name 'terminal-kind) ...)
          (list (list 'lexical-name
                      (lexical-expression lexical-expression-value)) ...)
          (list (list 'rule-name (grammar-expression rule-expression)) ...)
          (list (list 'extra-name) ...)
          (list (list 'keyword-name keyword-text) ...)
          (list (list 'entry-keyword 'entry-action 'entry-effect) ...)
          (list (list 'recovery-site recovery-code 'recovery-strategy) ...)
          (list (list 'flow-source 'flow-target) ...))))
    (_ (raise-syntax-error #f "invalid grammar role declaration" stx))))

;; defgrammar
;;   : (-> Syntax Syntax)
;;   | contract: preserves declared parent and role order while lowering to the
;;       checked Grammar constructor.
;;   | doc m%
;;       `defgrammar` composes admitted parent grammars and grammar roles.
;;
;;       # Examples
;;
;;       ```scheme
;;       (defgrammar language-grammar (supers base) (roles core-role))
;;       ;; => admitted Grammar
;;       ```
;;     %
(defsyntax (defgrammar stx)
  (syntax-case stx (supers roles)
    ((_ binding (supers parent ...) (roles role ...))
     (identifier? #'binding)
     #'(def binding
         (make-grammar 'binding (list parent ...) (list role ...))))
    (_ (raise-syntax-error #f "invalid grammar declaration" stx))))

;;; Declares ordered, explicit POO composition without an implicit last-wins
;;; rule. The compiler validates each operation against the state produced by
;;; the preceding operation and emits the composition receipt.
;; defgrammar-compose
;;   : (-> Syntax Syntax)
;;   | doc m%
;;       Declares ordered explicit composition over checked POO roles.
;;
;;       # Examples
;;
;;       ```scheme
;;       (defgrammar-compose grammar (supers base)
;;         (compose (append additions) (override corrections)))
;;       ;; => checked Grammar with ordered composition steps
;;       ```
;;     %
(defsyntax (defgrammar-compose stx)
  (syntax-case stx (supers compose)
    ((_ binding (supers parent ...) (compose (operator role) ...))
     (and (identifier? #'binding)
          (andmap (lambda (value)
                    (memq value '(append override remove)))
                  (syntax->datum #'(operator ...))))
     #'(def binding
         (make-grammar
          'binding
          (list parent ...)
          '()
          (list (cons 'operator role) ...))))
    (_ (raise-syntax-error #f "invalid explicit grammar composition" stx))))
