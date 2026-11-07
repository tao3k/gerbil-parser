;;; -*- Gerbil -*-
;;; Boundary: checked POO grammar objects and their open behavior protocol.
;;; Invariant: constructors extend contract-owned prototypes; section lookup
;;; dispatches through a prototype method rather than a representation helper.

(import (only-in :clan/poo/object .o .ref)
        (only-in :clan/poo/mop .defgeneric element? validate)
        (only-in :yuanxu/object-family/interface
                 defpoo-object-family)
        (only-in ./declaration-types
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
(def (make-grammar-role name syntax-kinds terminals lexical-rules rules extras
                        keywords parser-entrypoints recoveries flow)
  (validate
   GrammarRoleContract
   ;; Native lexical slots capture existing values once. Receiver formulas are
   ;; reserved for fields that actually depend on the effective receiver.
   (let (kind +grammar-role-kind+)
     (.o (:: @ GrammarRole.)
         (kind) (name) (syntax-kinds) (terminals) (lexical-rules) (rules)
         (extras) (keywords) (parser-entrypoints) (recoveries) (flow)))))

;; : (-> Symbol (List Grammar) (List GrammarRole) Grammar)
(def (make-grammar name parents roles (composition '()))
  (validate
   GrammarContract
   (let ((kind +grammar-kind+) (schema +grammar-schema+))
     (.o (:: @ Grammar.) (kind) (schema) (name) (parents) (roles) (composition)))))

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
