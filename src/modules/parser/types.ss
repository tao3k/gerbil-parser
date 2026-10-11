;;; Boundary: POO-native parser-domain types and responsibility contracts.
;;; Invariant: grammar values prove prototype ancestry and every public slot
;;; before object behavior or compiler normalization can observe them.

(import (only-in :clan/poo/object .o .ref .slot? object? compute-precedence-list!)
        (only-in :clan/poo/mop define-type element? raise-type-error)
        (only-in :core/types
                 PooFlowContract.
                 PooFlowNativeObjectContract.
                 poo-flow-classification-evidence
                 poo-flow-contract-admit
                 poo-flow-validation-evidence-accepted?))

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
      flow: ParserList)
  .element?: (cut parser-object-element? @ <>)
  .validate: (cut parser-object-validate @ <>))

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
  .obligations: grammar-object-obligations
  .element?: (cut parser-object-element? @ <>)
  .validate: (cut parser-object-validate @ <>))

;; Capture exact builtin descriptors privately, rather than following any
;; subsequent rebinding of an exported contract to a refinement.
(def +grammar-role-boolean-contract+ GrammarRoleContract)
(def +grammar-boolean-contract+ GrammarContract)

;;; Boolean membership and successful validation do not need responsibility
;;; evidence objects. Explicit .admit still belongs to the full Core protocol.
;;; Only these exact builtin descriptors use the specialization; refinements
;;; keep their own classifiers, responsibilities and obligations.
(def (parser-slot-matches? candidate slot predicate)
  (and (.slot? candidate slot) (predicate (.ref candidate slot))))

(def (parser-native-ancestry? descriptor candidate)
  (and (object? candidate)
       (if (memq (.ref descriptor 'proto) (compute-precedence-list! candidate)) #t #f)))

(def (parser-role-slots? candidate)
  (and (parser-slot-matches? candidate 'kind (lambda (value) (eq? value +grammar-role-kind+)))
       (parser-slot-matches? candidate 'name symbol?)
       (andmap (lambda (slot) (parser-slot-matches? candidate slot list?))
               '(syntax-kinds terminals lexical-rules rules extras keywords
                 parser-entrypoints recoveries flow))))

(def (parser-grammar-slots? candidate)
  (and (parser-slot-matches? candidate 'kind (lambda (value) (eq? value +grammar-kind+)))
       (parser-slot-matches? candidate 'schema (lambda (value) (equal? value +grammar-schema+)))
       (parser-slot-matches? candidate 'name symbol?)
       (andmap (lambda (slot) (parser-slot-matches? candidate slot list?))
               '(parents roles composition))
       (null? (grammar-object-obligations candidate #f))))

(def (parser-object-element? descriptor candidate)
  (cond
   ((eq? descriptor +grammar-role-boolean-contract+)
    (and (parser-native-ancestry? descriptor candidate) (parser-role-slots? candidate)))
   ((eq? descriptor +grammar-boolean-contract+)
    (and (parser-native-ancestry? descriptor candidate) (parser-grammar-slots? candidate)))
   (else
    (poo-flow-validation-evidence-accepted?
     (poo-flow-contract-admit descriptor candidate #f)))))

(def (parser-object-validate descriptor candidate)
  (if (and (or (eq? descriptor +grammar-role-boolean-contract+)
               (eq? descriptor +grammar-boolean-contract+))
           (parser-object-element? descriptor candidate))
    candidate
    ;; A refined descriptor is admitted exactly once. Its classifier and
    ;; obligations belong to the open protocol, including invocation count.
    (let (evidence (poo-flow-contract-admit descriptor candidate #f))
      (if (poo-flow-validation-evidence-accepted? evidence)
        candidate
        (raise-type-error descriptor candidate (.ref evidence 'diagnostics))))))
