;;; POO-backed grammar object families and hygienic declarations.

(import :clan/poo/object
        :poo-flow/src/core/object-syntax)
(export +grammar-role-kind+
        +grammar-kind+
        defgrammar-role
        defgrammar
        grammar-role?
        grammar-role-name
        grammar-role-ref
        grammar-role->alist
        grammar?
        grammar-name
        grammar-parents
        grammar-roles
        grammar->alist)

(def +grammar-role-kind+ 'gerbil-parser-grammar-role)
(def +grammar-kind+ 'gerbil-parser-grammar)

(def grammar-role-prototype
  (poo-core-role-object
   (slots ((kind +grammar-role-kind+)))
   (supers)))

(def grammar-prototype
  (poo-core-role-object
   (slots ((kind +grammar-kind+)
           (schema "gerbil-parser.grammar.v1")))
   (supers)))

(defsyntax (defgrammar-role stx)
  (syntax-case stx
      (syntax-kinds keywords prefix-operators binary-operators
                    parser-entrypoints recoveries flow)
    ((_ binding
        (syntax-kinds (kind-name kind-category (field-name ...)) ...)
        (keywords (keyword-name keyword-text) ...)
        (prefix-operators
         ((prefix-kind prefix-lexeme) prefix-precedence prefix-associativity) ...)
        (binary-operators
         ((binary-kind binary-lexeme) binary-precedence binary-associativity) ...)
        (parser-entrypoints (entry-keyword entry-action entry-effect) ...)
        (recoveries (recovery-site recovery-code recovery-strategy) ...)
        (flow (flow-source flow-target) ...))
     (identifier? #'binding)
     #'(def binding
         (poo-core-role-object
          (slots
           ((kind +grammar-role-kind+)
            (name 'binding)
            (syntax-kinds
             (list (list 'kind-name 'kind-category (list 'field-name ...)) ...))
            (keywords (list (list 'keyword-name keyword-text) ...))
            (prefix-operators
             (list (list 'prefix-kind prefix-lexeme
                         prefix-precedence 'prefix-associativity) ...))
            (binary-operators
             (list (list 'binary-kind binary-lexeme
                         binary-precedence 'binary-associativity) ...))
            (parser-entrypoints
             (list (list 'entry-keyword 'entry-action 'entry-effect) ...))
            (recoveries
             (list (list 'recovery-site recovery-code 'recovery-strategy) ...))
            (flow (list (list 'flow-source 'flow-target) ...))))
          (supers grammar-role-prototype))))
    (_ (raise-syntax-error #f "invalid grammar role declaration" stx))))

(defsyntax (defgrammar stx)
  (syntax-case stx (supers roles)
    ((_ binding (supers parent ...) (roles role ...))
     (identifier? #'binding)
     #'(def binding
         (poo-core-role-object
          (slots
           ((kind +grammar-kind+)
            (name 'binding)
            (parents (list parent ...))
            (roles (list role ...))))
          (supers grammar-prototype))))
    (_ (raise-syntax-error #f "invalid grammar declaration" stx))))

(def (grammar-role? value)
  (and (object? value)
       (with-catch
        (lambda (_failure) #f)
        (lambda ()
          (eq? (.ref value 'kind) +grammar-role-kind+)))))

(def (grammar-role-name value)
  (.ref value 'name))

(def (grammar-role->alist value)
  (list (cons 'kind (.ref value 'kind))
        (cons 'name (.ref value 'name))
        (cons 'syntax-kinds (.ref value 'syntax-kinds))
        (cons 'keywords (.ref value 'keywords))
        (cons 'prefix-operators (.ref value 'prefix-operators))
        (cons 'binary-operators (.ref value 'binary-operators))
        (cons 'parser-entrypoints (.ref value 'parser-entrypoints))
        (cons 'recoveries (.ref value 'recoveries))
        (cons 'flow (.ref value 'flow))))

(def (grammar-role-ref role field)
  (.ref role field))

(def (grammar? value)
  (and (object? value)
       (with-catch
        (lambda (_failure) #f)
        (lambda ()
          (eq? (.ref value 'kind) +grammar-kind+)))))

(def (grammar-name value)
  (.ref value 'name))

(def (grammar-parents value)
  (.ref value 'parents))

(def (grammar-roles value)
  (.ref value 'roles))

(def (grammar->alist value)
  (list (cons 'schema (.ref value 'schema))
        (cons 'name (.ref value 'name))
        (cons 'parents (.ref value 'parents))
        (cons 'roles (.ref value 'roles))))
