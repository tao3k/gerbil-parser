;;; Boundary: pure final projections from admitted parser POO objects.
;;; Invariant: alists are emitted only at an explicit projection boundary and
;;; never become the semantic owner of grammar composition or behavior.

(import (only-in ./objects
                 grammar-role-name
                 grammar-role-ref
                 grammar-schema
                 grammar-name
                 grammar-parents
                 grammar-roles
                 grammar-composition))
(export grammar-role->alist
        grammar->alist)

;; : (-> GrammarRole Alist)
(def (grammar-role->alist role)
  (list
   (cons 'kind (grammar-role-ref role 'kind))
   (cons 'name (grammar-role-name role))
   (cons 'syntax-kinds (grammar-role-ref role 'syntax-kinds))
   (cons 'terminals (grammar-role-ref role 'terminals))
   (cons 'lexical-rules (grammar-role-ref role 'lexical-rules))
   (cons 'rules (grammar-role-ref role 'rules))
   (cons 'extras (grammar-role-ref role 'extras))
   (cons 'keywords (grammar-role-ref role 'keywords))
   (cons 'parser-entrypoints (grammar-role-ref role 'parser-entrypoints))
   (cons 'recoveries (grammar-role-ref role 'recoveries))
   (cons 'flow (grammar-role-ref role 'flow))))

;; : (-> Grammar Alist)
(def (grammar->alist grammar)
  (list
   (cons 'schema (grammar-schema grammar))
   (cons 'name (grammar-name grammar))
   (cons 'parents (grammar-parents grammar))
   (cons 'roles (grammar-roles grammar))
   (cons 'composition (grammar-composition grammar))))
