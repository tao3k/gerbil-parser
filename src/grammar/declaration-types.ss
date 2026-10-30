;;; Boundary: POO-native parser-domain types and responsibility contracts.
;;; Invariant: grammar values prove prototype ancestry and every public slot
;;; before object behavior or compiler normalization can observe them.

(import (only-in ../schema +grammar-schema+ +grammar-ir-schema+
                 +parser-ir-schema+ +parse-artifact-schema+ +diagnostic-schema+)
        (only-in :clan/poo/object .ref .cc)
        (only-in :clan/poo/mop define-type element? Type.)
        (only-in :clan/poo/type Record Symbol Exactly)
        (only-in :yuanxu/object-types/contracts
                 YuanxuObjectContract. yuanxu-predicate-contract))

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
;; Compatibility Contracts are also consumed by contextual responsibilities.
;; Core owns their classification/evidence protocol; Parser owns the predicates.
(def ParserSymbol (yuanxu-predicate-contract 'gerbil-parser/symbol symbol?))
(def ParserList (yuanxu-predicate-contract 'gerbil-parser/list list?))
(def GrammarRoleKind (yuanxu-predicate-contract 'gerbil-parser/grammar-role-kind
                       (lambda (v) (eq? v +grammar-role-kind+))))
(def GrammarKind (yuanxu-predicate-contract 'gerbil-parser/grammar-kind
                   (lambda (v) (eq? v +grammar-kind+))))
(def GrammarSchema (yuanxu-predicate-contract 'gerbil-parser/grammar-schema
                     (lambda (v) (equal? v +grammar-schema+))))

;; Sections are opaque proper-list declarations here. Their individual domain
;; languages are checked by the normalizer, rather than pretending to know a
;; universal element Type (or introducing an Any parameter).
(def DeclarationList (.cc Type. sexp: 'DeclarationList '.element? list?))
(def GrammarRoleValue
  (Record kind: [(Exactly +grammar-role-kind+)] name: [Symbol]
          syntax-kinds: [DeclarationList] terminals: [DeclarationList]
          lexical-rules: [DeclarationList] rules: [DeclarationList]
          extras: [DeclarationList] keywords: [DeclarationList]
          parser-entrypoints: [DeclarationList] recoveries: [DeclarationList]
          flow: [DeclarationList]))
(def GrammarValue
  (Record kind: [(Exactly +grammar-kind+)] schema: [(Exactly +grammar-schema+)]
          name: [Symbol] parents: [DeclarationList] roles: [DeclarationList]
          composition: [DeclarationList]))

;; The value Type owns slots and normalization. Core owns ancestry, contextual
;; responsibilities and the actual normalized candidate returned by validate.
(define-type (GrammarRoleContract @ YuanxuObjectContract.)
  identity: 'gerbil-parser/grammar-role
  proto: (.ref GrammarRoleValue 'proto)
  value-contract: GrammarRoleValue)

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


;;; Composition is a Parser domain obligation, not a second value validator.
(define-type (GrammarContract @ YuanxuObjectContract.)
  identity: 'gerbil-parser/grammar
  proto: (.ref GrammarValue 'proto)
  value-contract: GrammarValue
  .obligations: grammar-object-obligations)
