;;; Boundary: POO-native parser-domain types and responsibility contracts.
;;; Invariant: grammar values prove prototype ancestry and every public slot
;;; before object behavior or compiler normalization can observe them.

(import (only-in ../../schema +grammar-schema+ +grammar-ir-schema+
                 +parser-ir-schema+ +parse-artifact-schema+ +diagnostic-schema+)
        (only-in :clan/poo/object .o .ref .slot? object? compute-precedence-list!)
        (only-in :clan/poo/mop define-type element? raise-type-error)
        (only-in :yuanxu/object-types/contracts
                 YuanxuContract.
                 YuanxuObjectContract.
                 yuanxu-classification-evidence
                 yuanxu-contract-admit
                 yuanxu-validation-evidence-accepted?))

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
        GrammarSchema
        GrammarRoleContract
        GrammarContract)

;; : Symbol
(def +grammar-role-kind+ 'gerbil-parser-grammar-role)
;; : Symbol
(def +grammar-kind+ 'gerbil-parser-grammar)
;; : (-> Symbol Procedure Object Object YuanxuClassificationEvidence)
(def (parser-scalar-classify identity predicate candidate context)
  (let (accepted? (predicate candidate))
    (yuanxu-classification-evidence
     identity candidate accepted?
     (if accepted? '() (list (list 'expected identity)))
     context)))

;;; Invariant: parser-domain identities remain canonical Scheme symbols.
(define-type (ParserSymbol @ YuanxuContract.)
  identity: 'gerbil-parser/symbol
  .classify: (lambda (candidate context)
               (parser-scalar-classify
                'gerbil-parser/symbol symbol? candidate context)))

;;; Invariant: declaration sections are proper lists before normalization.
(define-type (ParserList @ YuanxuContract.)
  identity: 'gerbil-parser/list
  .classify: (lambda (candidate context)
               (parser-scalar-classify
                'gerbil-parser/list list? candidate context)))

;;; Invariant: a role cannot forge another parser object family identity.
(define-type (GrammarRoleKind @ YuanxuContract.)
  identity: 'gerbil-parser/grammar-role-kind
  .classify: (lambda (candidate context)
               (parser-scalar-classify
                'gerbil-parser/grammar-role-kind
                (lambda (value) (eq? value +grammar-role-kind+))
                candidate context)))

;;; Invariant: a composed grammar has exactly the stable grammar kind.
(define-type (GrammarKind @ YuanxuContract.)
  identity: 'gerbil-parser/grammar-kind
  .classify: (lambda (candidate context)
               (parser-scalar-classify
                'gerbil-parser/grammar-kind
                (lambda (value) (eq? value +grammar-kind+))
                candidate context)))

;;; Invariant: public grammar objects publish the schema declared in shared metadata.
(define-type (GrammarSchema @ YuanxuContract.)
  identity: 'gerbil-parser/grammar-schema
  .classify: (lambda (candidate context)
               (parser-scalar-classify
                'gerbil-parser/grammar-schema
                (lambda (value) (equal? value +grammar-schema+))
                candidate context)))

;; : (-> Unit POOObject)
(def (parser-empty-prototype) (.o))
;; Family identities are values, not receiver-local prototype suppliers.
;; Refining a Contract must retain its family unless proto is overridden.
(def grammar-role-prototype (parser-empty-prototype))
(def grammar-prototype (parser-empty-prototype))

;;; Boundary: a grammar role owns all declaration sections as typed POO responsibilities.
(define-type (GrammarRoleContract @ YuanxuObjectContract.)
  identity: 'gerbil-parser/grammar-role
  proto: grammar-role-prototype
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
(define-type (GrammarContract @ YuanxuObjectContract.)
  identity: 'gerbil-parser/grammar
  proto: grammar-prototype
  responsibilities:
  (.o kind: GrammarKind
      schema: GrammarSchema
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
    (yuanxu-validation-evidence-accepted?
     (yuanxu-contract-admit descriptor candidate #f)))))

(def (parser-object-validate descriptor candidate)
  (if (and (or (eq? descriptor +grammar-role-boolean-contract+)
               (eq? descriptor +grammar-boolean-contract+))
           (parser-object-element? descriptor candidate))
    candidate
    ;; A refined descriptor is admitted exactly once. Its classifier and
    ;; obligations belong to the open protocol, including invocation count.
    (let (evidence (yuanxu-contract-admit descriptor candidate #f))
      (if (yuanxu-validation-evidence-accepted? evidence)
        candidate
        (raise-type-error descriptor candidate (.ref evidence 'diagnostics))))))
