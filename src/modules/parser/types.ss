;;; Boundary: POO-native parser-domain types and responsibility contracts.
;;; Invariant: grammar values prove prototype ancestry and every public slot
;;; before object behavior or compiler normalization can observe them.

(import (only-in :clan/poo/object .o .ref)
        (only-in :clan/poo/mop define-type element?)
        (only-in :poo-flow/src/module-system/types
                 PooFlowContract.
                 PooFlowNativeObjectContract.
                 poo-flow-classification-evidence))

(export +grammar-role-kind+
        +grammar-kind+
        +grammar-schema+
        +grammar-ir-schema+
        +parser-ir-schema+
        +parse-artifact-schema+
        +diagnostic-schema+
        ParserSymbol
        ParserList
        GrammarRoleKind
        GrammarKind
        GrammarSchemaV1
        GrammarRoleContract
        GrammarContract)

;; : Symbol
(def +grammar-role-kind+ 'gerbil-parser-grammar-role)
;; : Symbol
(def +grammar-kind+ 'gerbil-parser-grammar)
;; : String
(def +grammar-schema+ "gerbil-parser.grammar.v1")
;; : String
(def +grammar-ir-schema+ "gerbil-parser.grammar-ir.v1")
;; : String
(def +parser-ir-schema+ "gerbil-parser.parser-ir.v1")
;; : String
(def +parse-artifact-schema+ "gerbil-parser.parse-artifact.v1")
;; : String
(def +diagnostic-schema+ "gerbil-parser.diagnostic.v1")

;; : (-> Symbol Procedure Object Object PooFlowClassificationEvidence)
(def (parser-scalar-classify identity predicate candidate context)
  (let (accepted? (predicate candidate))
    (poo-flow-classification-evidence
     identity candidate accepted?
     (if accepted? '() (list (list 'expected identity)))
     context)))

;;; Invariant: parser-domain identities remain canonical Scheme symbols.
(define-type (ParserSymbol @ PooFlowContract.)
  identity: 'gerbil-parser/symbol
  .classify: (lambda (candidate context)
               (parser-scalar-classify
                'gerbil-parser/symbol symbol? candidate context)))

;;; Invariant: declaration sections are proper lists before normalization.
(define-type (ParserList @ PooFlowContract.)
  identity: 'gerbil-parser/list
  .classify: (lambda (candidate context)
               (parser-scalar-classify
                'gerbil-parser/list list? candidate context)))

;;; Invariant: a role cannot forge another parser object family identity.
(define-type (GrammarRoleKind @ PooFlowContract.)
  identity: 'gerbil-parser/grammar-role-kind
  .classify: (lambda (candidate context)
               (parser-scalar-classify
                'gerbil-parser/grammar-role-kind
                (lambda (value) (eq? value +grammar-role-kind+))
                candidate context)))

;;; Invariant: a composed grammar has exactly the stable grammar kind.
(define-type (GrammarKind @ PooFlowContract.)
  identity: 'gerbil-parser/grammar-kind
  .classify: (lambda (candidate context)
               (parser-scalar-classify
                'gerbil-parser/grammar-kind
                (lambda (value) (eq? value +grammar-kind+))
                candidate context)))

;;; Invariant: public grammar objects publish only the frozen v1 schema.
(define-type (GrammarSchemaV1 @ PooFlowContract.)
  identity: 'gerbil-parser/grammar-schema-v1
  .classify: (lambda (candidate context)
               (parser-scalar-classify
                'gerbil-parser/grammar-schema-v1
                (lambda (value) (equal? value +grammar-schema+))
                candidate context)))

;; : (-> Unit POOObject)
(def (parser-empty-prototype) (.o))

;;; Boundary: a grammar role owns all declaration sections as typed POO responsibilities.
(define-type (GrammarRoleContract @ PooFlowNativeObjectContract.)
  identity: 'gerbil-parser/grammar-role
  proto: (parser-empty-prototype)
  responsibilities:
  (.o kind: GrammarRoleKind
      name: ParserSymbol
      syntax-kinds: ParserList
      terminals: ParserList
      lexical-rules: ParserList
      rules: ParserList
      extras: ParserList
      keywords: ParserList
      parser-entrypoints: ParserList
      recoveries: ParserList
      flow: ParserList))

;; : (-> Object Object [Object])
(def (grammar-object-obligations candidate _context)
  (append
   (if (andmap (lambda (parent) (element? GrammarContract parent))
               (.ref candidate 'parents))
     '()
     '(invalid-grammar-parent))
   (if (andmap (lambda (role) (element? GrammarRoleContract role))
               (.ref candidate 'roles))
     '()
     '(invalid-grammar-role))
   (if (andmap
        (lambda (step)
          (and (pair? step)
               (memq (car step) '(append override remove))
               (element? GrammarRoleContract (cdr step))))
        (.ref candidate 'composition))
     '()
     '(invalid-grammar-composition))))

;;; Boundary: grammar composition admits only typed parent grammars and role objects.
(define-type (GrammarContract @ PooFlowNativeObjectContract.)
  identity: 'gerbil-parser/grammar
  proto: (parser-empty-prototype)
  responsibilities:
  (.o kind: GrammarKind
      schema: GrammarSchemaV1
      name: ParserSymbol
      parents: ParserList
      roles: ParserList
      composition: ParserList)
  .obligations: grammar-object-obligations)
