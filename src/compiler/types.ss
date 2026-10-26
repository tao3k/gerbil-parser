;;; The native Record owns field types, inheritance, defaults and normalization.
(import (only-in :clan/poo/object .cc .get)
        (only-in :clan/poo/mop define-type Fun Any Object Type. element?)
        (only-in :clan/poo/type Record List Pair Symbol Exactly Or)
        (only-in :yuanxu/object-types/contracts YuanxuContract.))
(import (only-in ../schema +grammar-schema+ +grammar-ir-schema+)
        (only-in ../modules/parser/types +grammar-kind+))
(export CompilerGrammar GrammarIR CompilerScope CompilerScopeContract)

;; Compiler consumes the open structural protocol, not source-local ancestry.
;; Full composition/semantic checks remain in the authoritative normalizer.
(define-type CompilerGrammar
  (Record kind: [(Exactly +grammar-kind+)] schema: [(Exactly +grammar-schema+)]
          name: [Symbol] parents: [(List Object)] roles: [(List Object)]
          composition: [(List (Pair Symbol Object))]))
;; Heterogeneous section payloads remain compiler-owned; keys are symbols.
(def GrammarIRRows (List (Pair Symbol Any)))
(define-type (GrammarIR @ Type.)
  .element?: (lambda (value)
    (and (element? GrammarIRRows value)
         (let (schema (assq 'schema value))
           (and schema (equal? (cdr schema) +grammar-ir-schema+)))
         (let (name (assq 'grammar value)) (and name (symbol? (cdr name)))))))

(define-type CompilerScope
  (Record compile-grammar: [(Fun GrammarIR <- (Or CompilerGrammar GrammarIR))]))

;; Core adds contextual admission/evidence; it does not redeclare the fields.
(def CompilerScopeContract
  (.cc YuanxuContract. identity: 'compiler/scope
       proto: (.get CompilerScope proto) value-contract: CompilerScope))
