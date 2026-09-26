;;; -*- Gerbil -*-
;;; Boundary: checked POO grammar objects and their open behavior protocol.
;;; Invariant: constructors extend contract-owned prototypes; section lookup
;;; dispatches through a prototype method rather than a representation helper.

(import (only-in :clan/poo/object .o .ref)
        (only-in :clan/poo/mop .defgeneric element? validate)
        (only-in :poo-flow-foundation/module-system/object-family/interface
                 defpoo-object-family)
        (only-in ./types
                 +grammar-role-kind+
                 +grammar-kind+
                 +grammar-schema+
                 GrammarRoleContract
                 GrammarContract))
(export +grammar-role-kind+
        +grammar-kind+
        GrammarRole.
        Grammar.
        make-grammar-role
        make-grammar
        grammar-role?
        grammar-role-name
        grammar-role-ref
        grammar?
        grammar-schema
        grammar-name
        grammar-parents
        grammar-roles
        grammar-composition)

;;; Boundary: role section behavior is an open POO protocol owned by the receiver.
;; : (forall (a) (-> GrammarRole Symbol a))
(.defgeneric (grammar-role-ref role field) slot: .section)

;; Resolve the contract prototype once at owner load.  POO Flow Observability
;; treats prototype lookup inside repeated .o composition as a cold-boundary
;; authoring defect.
;; : PooObject
(def GrammarRoleContract. (.ref GrammarRoleContract 'proto))

;; : PooObject
(def GrammarRole.
  (.o (:: self GrammarRoleContract.)
      (.section (lambda (field) (.ref self field)))))

;; : PooObject
(def Grammar. (.ref GrammarContract 'proto))

;; : (-> Symbol List List List List List List List List List List GrammarRole)
(def (make-grammar-role name-value syntax-kinds-value terminals-value
                        lexical-rules-value rules-value extras-value
                        keywords-value parser-entrypoints-value
                        recoveries-value flow-value)
  (validate
   GrammarRoleContract
   (.o (:: @ GrammarRole.)
       kind: +grammar-role-kind+
       name: name-value
       syntax-kinds: syntax-kinds-value
       terminals: terminals-value
       lexical-rules: lexical-rules-value
       rules: rules-value
       extras: extras-value
       keywords: keywords-value
       parser-entrypoints: parser-entrypoints-value
       recoveries: recoveries-value
       flow: flow-value)))

;; : (-> Symbol (List Grammar) (List GrammarRole) Grammar)
(def (make-grammar name-value parent-values role-values
                   (composition-values '()))
  (validate
   GrammarContract
   (.o (:: @ Grammar.)
       kind: +grammar-kind+
       schema: +grammar-schema+
       name: name-value
       parents: parent-values
       roles: role-values
       composition: composition-values)))

(defpoo-object-family
  (accessors
   (grammar-role-name name))
  (projections))

;; : (-> Object Boolean)
(def (grammar-role? value)
  (element? GrammarRoleContract value))

(defpoo-object-family
  (accessors
   (grammar-schema schema)
   (grammar-name name)
   (grammar-parents parents)
   (grammar-roles roles)
   (grammar-composition composition))
  (projections))

;; : (-> Object Boolean)
(def (grammar? value)
  (element? GrammarContract value))
