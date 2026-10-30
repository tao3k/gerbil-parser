;;; The native Record owns field types, inheritance, defaults and normalization.
(import (only-in :std/list/list delete-duplicates/hash)
        (only-in :clan/poo/object .cc .get .ref)
        (only-in :clan/poo/mop define-type Fun Object Bool Type. element?)
        (only-in :clan/poo/type Record List Pair Symbol String Exactly Or)
        (only-in :yuanxu/object-types/contracts YuanxuContract.))
(import (only-in ../grammar/declaration-types GrammarContract +grammar-ir-schema+ ParserList)
        (only-in ../grammar/algebra grammar-expression?)
        (only-in ../grammar/lexical-algebra lexical-expression?))
(export CompilerGrammar GrammarIR CompilerScope CompilerScopeContract)

;; Compiler consumes the open structural protocol, not source-local ancestry.
;; Inherit the one grammar field declaration, refining only compiler inputs.
;; Full composition/semantic checks remain in the authoritative normalizer.
(define-type CompilerGrammar
  (Record (list (.ref GrammarContract 'value-contract))
          parents: [(List Object)] roles: [(List Object)]
          composition: [(List (Pair Symbol Object))]))
;; Declaration data excludes opaque objects and procedures at section boundaries.
(define-type (GrammarExpression @ Type.) .element?: grammar-expression?)
(define-type (LexicalExpression @ Type.) .element?: lexical-expression?)
(def GrammarIRRows (List (Pair Symbol (Or String Symbol Bool ParserList))))
(def GrammarRuleRows (List (Pair Symbol (Pair GrammarExpression (Exactly '())))))
(def LexicalRuleRows (List (Pair Symbol (Pair LexicalExpression (Exactly '())))))
(def SectionRows (List (Pair Symbol ParserList)))
(def (grammar-ir-section? entry)
  (let (type
    (case (car entry)
      ((schema compositionDigest) String)
      ((grammar) Symbol)
      ((conflict-policy) (Or (Exactly 'reject) (Exactly 'glr)))
      ((case-insensitive?) Bool)
      ((rules) GrammarRuleRows)
      ((lexical-rules) LexicalRuleRows)
      ((extras) (List (Pair Symbol (Exactly '()))))
      ((syntax-kinds terminals keywords parser-entrypoints recoveries flow) SectionRows)
      (else #f)))
    (and type (element? type (cdr entry)))))
(define-type (GrammarIR @ Type.)
  .element?: (lambda (value)
    (and (element? GrammarIRRows value)
         (= (length value) (length (delete-duplicates/hash (map car value))))
         (andmap grammar-ir-section? value)
         (let (schema (assq 'schema value))
           (and schema (equal? (cdr schema) +grammar-ir-schema+)))
         (let (name (assq 'grammar value)) (and name (symbol? (cdr name)))))))

(define-type CompilerScope
  (Record compile-grammar: [(Fun GrammarIR <- (Or CompilerGrammar GrammarIR))]))

;; Core adds contextual admission/evidence; it does not redeclare the fields.
(def CompilerScopeContract
  (.cc YuanxuContract. identity: 'compiler/scope
       proto: (.get CompilerScope proto) value-contract: CompilerScope))
