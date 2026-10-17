#!/usr/bin/env gxi
;;; -*- Gerbil -*-
(import "list-roles" "list-origins"
        (only-in "../../../src/compiler/normalize" compile-grammar/receipt)
        (only-in "../../../src/compiler/bound-ir"
                 bind-grammar-ir bound-grammar-ir-ref bound-grammar-ir-binding)
        (only-in "../../../src/compiler/parser-ir" compile-parser parser-ir-ref)
        (only-in "../../../src/compiler/language-artifact"
                 materialize-compiled-language-artifact/output-dirs
                 compile-language-declaration-artifacts/output-dirs
                 make-admitted-language-declaration compile-admitted-language-declaration
                 compiled-language-declaration-grammar
                 compiled-language-declaration-grammar-locator
                 compiled-language-declaration-bound-locator
                 compiled-language-declaration-parser-locator)
        (only-in "../../../src/runtime/language-artifact"
                 load-compiled-language-artifact/roots))
(def checked 0)
(def (probe label actual expected)
  (unless (equal? actual expected) (error "provenance study mismatch" label actual expected))
  (set! checked (+ checked 1))
  (display "PROVENANCE-CASE-OK: ") (display label) (newline) (force-output))
(def (ref value key) (let (row (assq key value)) (and row (cdr row))))
(def (without value key) (filter (lambda (row) (not (eq? (car row) key))) value))
(let-values (((grammar receipt) (compile-grammar/receipt (make-list-study-grammar))))
  (let* ((origin "list-study-provider")
         (lineage '(deflist-study make-nonempty-list-role))
         (sources (list-study-source-map receipt))
         (ordinary (bind-grammar-ir grammar origin lineage))
         (owned (bind-grammar-ir grammar origin lineage sources))
         (entry (bound-grammar-ir-binding owned 'rule 'arguments))
         (helper (bound-grammar-ir-binding owned 'rule 'call-arguments/tail))
         (old-helper (bound-grammar-ir-binding ordinary 'rule 'call-arguments/tail))
         (old-entry (bound-grammar-ir-binding ordinary 'rule 'arguments)))
    (probe 'binding-identity-stable (ref helper 'bindingId) (ref old-helper 'bindingId))
    (probe 'reference-identity-stable (ref owned 'referenceDigest) (ref ordinary 'referenceDigest))
    (probe 'grammar-digest-stable (ref owned 'grammarDigest) (ref ordinary 'grammarDigest))
    (probe 'source-sensitive-identity
      (equal? (ref owned 'identityDigest) (ref ordinary 'identityDigest)) #f)
    (probe 'source-sensitive-declaration
      (equal? (ref entry 'declarationId) (ref old-entry 'declarationId)) #f)
    (probe 'helper-generated (ref (ref helper 'source) 'generated?) #t)
    (probe 'helper-component-owner (ref (ref helper 'source) 'componentOwner) 'call-arguments)
    (probe 'entry-component-owner (ref (ref entry 'source) 'componentOwner) 'call-arguments)
    (probe 'base-not-generated
      (ref (ref (bound-grammar-ir-binding owned 'rule 'source-file) 'source) 'generated?) #f)
    (probe 'lineage-preserved (ref helper 'expansionLineage) lineage)
    (let* ((roots '("/private/tmp/gerbil-parser-provenance-study"))
           (locator (materialize-compiled-language-artifact/output-dirs owned roots))
           (loaded (load-compiled-language-artifact/roots
                     "gerbil-parser.bound-grammar-ir.v1" locator roots)))
      (probe 'materialized-bound-artifact loaded owned))
    (let* ((flattened (without grammar 'compositionDigest))
           (composed-parser (compile-parser grammar))
           (flattened-parser (compile-parser flattened))
           (flattened-bound (bind-grammar-ir flattened origin lineage sources)))
      (probe 'parser-retains-composition
        (parser-ir-ref composed-parser 'compositionDigest) (ref receipt 'compositionDigest))
      (probe 'flattening-loses-composition (parser-ir-ref flattened-parser 'compositionDigest) #f)
      (probe 'lr-spec-stable-on-composition-metadata
        (parser-ir-ref composed-parser 'lr-spec) (parser-ir-ref flattened-parser 'lr-spec))
      (probe 'parser-difference-is-composition-field
        (without composed-parser 'compositionDigest) (without flattened-parser 'compositionDigest))
      (probe 'bound-identity-independent-of-composition-metadata
        (ref owned 'identityDigest) (ref flattened-bound 'identityDigest))
      (probe 'grammar-digest-covers-composition-metadata
        (equal? (ref owned 'grammarDigest) (ref flattened-bound 'grammarDigest)) #f))))
;;; Engine-owned declaration caching for the same composed package.
;;; A fresh root is supplied by the bounded research runner; no timing claim.
(let-values (((grammar receipt) (compile-grammar/receipt (make-list-study-grammar))))
  (let* ((roots (list (getenv "PARSER_RESEARCH_ARTIFACT_ROOT")))
         (sources (list-study-source-map receipt))
         (lineage '(deflist-study make-nonempty-list-role))
         (bound-calls 0) (parser-calls 0))
    (def (publish origin)
      (compile-language-declaration-artifacts/output-dirs
       (list origin lineage sources) grammar
       (lambda ()
         (set! bound-calls (+ bound-calls 1))
         (bind-grammar-ir grammar origin lineage sources))
       (lambda ()
         (set! parser-calls (+ parser-calls 1))
         (compile-parser grammar))
       roots))
    (let-values (((g1 b1 p1 status1) (publish "cache-owner-A")))
      (probe 'declaration-first-miss status1 'miss)
      (probe 'first-compile-calls (list bound-calls parser-calls) '(1 1))
      (probe 'reload-exact-canonical-grammar
        (load-compiled-language-artifact/roots "gerbil-parser.grammar-ir.v1" g1 roots) grammar)
      (probe 'reload-parser-composition
        (parser-ir-ref
         (load-compiled-language-artifact/roots "gerbil-parser.parser-ir.v1" p1 roots)
         'compositionDigest)
        (ref receipt 'compositionDigest))
      (let-values (((g2 b2 p2 status2) (publish "cache-owner-A")))
        (probe 'declaration-repeat-hit status2 'hit)
        (probe 'repeat-skips-compilers (list bound-calls parser-calls) '(1 1))
        (probe 'repeat-identical-locators (list g2 b2 p2) (list g1 b1 p1)))
      (let-values (((g3 b3 p3 status3) (publish "cache-owner-B")))
        (probe 'context-change-declaration-miss status3 'miss)
        (probe 'context-change-rebinds-reuses-parser (list bound-calls parser-calls) '(2 1))
        (probe 'context-change-same-grammar-locator g3 g1)
        (probe 'context-change-same-parser-locator p3 p1)
        (probe 'context-change-different-bound-locator (equal? b3 b1) #f)
        (let* ((bound (load-compiled-language-artifact/roots
                       "gerbil-parser.bound-grammar-ir.v1" b3 roots))
               (entry (bound-grammar-ir-binding bound 'rule 'arguments)))
          (probe 'context-change-loaded-origin (ref entry 'originModule) "cache-owner-B")
          (probe 'context-change-retains-component-owner
            (ref (ref entry 'source) 'componentOwner) 'call-arguments))))))
;;; The same publication stage accepts normalized POO data and its effective
;;; source context directly, without reconstructing a DSL declaration.
(let-values (((grammar receipt) (compile-grammar/receipt (make-list-study-grammar))))
  (let* ((roots (list (getenv "PARSER_RESEARCH_ARTIFACT_ROOT")))
         (origin "canonical-poo-stage")
         (lineage '(deflist-study make-nonempty-list-role))
         (sources (list-study-source-map receipt))
         (compiled
          (compile-admitted-language-declaration
           (make-admitted-language-declaration grammar origin lineage sources)
           compile-parser bind-grammar-ir output-dirs: roots))
         (loaded-grammar
          (load-compiled-language-artifact/roots
           "gerbil-parser.grammar-ir.v1"
           (compiled-language-declaration-grammar-locator compiled) roots))
         (loaded-bound
          (load-compiled-language-artifact/roots
           "gerbil-parser.bound-grammar-ir.v1"
           (compiled-language-declaration-bound-locator compiled) roots))
         (loaded-parser
          (load-compiled-language-artifact/roots
           "gerbil-parser.parser-ir.v1"
           (compiled-language-declaration-parser-locator compiled) roots)))
    (probe 'canonical-stage-retains-input-object
      (eq? (compiled-language-declaration-grammar compiled) grammar) #t)
    (probe 'canonical-stage-grammar-reload loaded-grammar grammar)
    (probe 'canonical-stage-bound-reload loaded-bound
      (bind-grammar-ir grammar origin lineage sources))
    (probe 'canonical-stage-parser-materialization
      (parser-ir-ref loaded-parser 'materialization) 'aot-expansion)
    (probe 'canonical-stage-parser-reload
      (without loaded-parser 'materialization) (compile-parser grammar))
    (probe 'canonical-stage-retains-composition
      (parser-ir-ref loaded-parser 'compositionDigest) (ref receipt 'compositionDigest))
    (probe 'canonical-stage-retains-effective-owner
      (ref (ref (bound-grammar-ir-binding loaded-bound 'rule 'arguments) 'source)
           'componentOwner)
      'call-arguments)))
(display "PROVENANCE-STUDY-OK: ") (display checked)
(display " checks passed") (newline) (force-output)
(exit 0)
