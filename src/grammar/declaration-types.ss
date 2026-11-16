;;; Boundary: POO-native parser-domain types and responsibility contracts.
;;; Invariant: grammar values prove prototype ancestry and every public slot
;;; before object behavior or compiler normalization can observe them.

(import (only-in ../schema +grammar-schema+ +grammar-ir-schema+
                 +parser-ir-schema+ +parse-artifact-schema+ +diagnostic-schema+)
        (only-in :clan/poo/object .ref .cc .o .all-slots
                 object? object-revision object-slots object-declarations .slot?
                 object-protocol object-override-specifications compute-precedence-list!
                 $constant-slot-spec? $constant-slot-spec-value)
        (only-in :clan/poo/slot-observation %slot-read-active?)
        (only-in :clan/poo/mop Class. Slot)
        (only-in :std/list/list delete-duplicates/hash)
        (only-in :clan/poo/type define-type element? Type. Record Symbol Exactly)
        (only-in :tao3k/yuanxu/types/contracts
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
;; These Records have identity validators and ordinary typed fields. Prepare
;; their POO Type predicates once. Plain receivers already carry the complete
;; field set, so both membership and validation can check current values directly.
(def (declaration-record record)
  (def descriptor
    (.o (:: self record)
      (.element? (inherited)
        (lambda (candidate)
          (cond ((and (eq? self descriptor) (plain-declaration-specs candidate))
                 => fields-match?)
                (else ((inherited) candidate)))))
      (.validate (inherited)
        (lambda (candidate)
          (let (specs (and (eq? self descriptor) (plain-declaration-specs candidate)))
            (if (and specs (fields-match? specs))
              candidate
              ((inherited) candidate)))))))
  (def (plain-declaration-specs candidate)
    (and (not (%slot-read-active?))
         (object? candidate)
         (stock-descriptors?)
         (null? (object-override-specifications candidate))
         (let (chain (compute-precedence-list! candidate))
           (and (memq proto chain)
                (not (object-protocol candidate))
                (andmap (lambda (owner) (null? (object-declarations owner))) chain)))
         (let (specs (object-slots candidate))
           (and (andmap (lambda (entry) ($constant-slot-spec? (cdr entry))) specs)
                (let (required (map (cut assq <> specs) names))
                  (and (andmap pair? required) required))))))
  (def (fields-match? specs)
    (andmap (lambda (entry check) (check ($constant-slot-spec-value (cdr entry))))
            specs checks))
  (def proto (.ref descriptor 'proto))
  (def slots (.ref descriptor 'effective-slots))
  (def names (.all-slots slots))
  (def fields (map (cut .ref slots <>) names))
  (def types (map (cut .ref <> 'type) fields))
  (def checks (map (cut .ref <> '.element?) types))
  (def typed-fields?
    (and (not (.ref descriptor 'sealed))
         (andmap (lambda (field) (not (.slot? field 'constant))) fields)))
  (def owners
    (delete-duplicates/hash
     (append (list Type. Class. Slot record descriptor slots) fields types)
     table: (make-hash-table-eq)))
  ;; Use inherited POO dispatch after any edit to a checker or descriptor.
  (def (stock-descriptors?)
    (and typed-fields?
         (andmap (lambda (owner) (zero? (object-revision owner))) owners)))
  descriptor)

(def GrammarRoleValue
  (declaration-record
   (Record kind: [(Exactly +grammar-role-kind+)] name: [Symbol]
           syntax-kinds: [DeclarationList] terminals: [DeclarationList]
           lexical-rules: [DeclarationList] rules: [DeclarationList]
           extras: [DeclarationList] keywords: [DeclarationList]
           parser-entrypoints: [DeclarationList] recoveries: [DeclarationList]
           flow: [DeclarationList])))
(def GrammarValue
  (declaration-record
   (Record kind: [(Exactly +grammar-kind+)] schema: [(Exactly +grammar-schema+)]
           name: [Symbol] parents: [DeclarationList] roles: [DeclarationList]
           composition: [DeclarationList])))

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
