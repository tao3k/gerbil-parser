#!/usr/bin/env gxi
(import :gerbil-parser/language-test-support)
;;; -*- Gerbil -*-
;;; Section-local declaration and reference indexes preserve Bound IR v1.

(import (only-in :std/test check check-exception test-case test-suite)
        (only-in :std/list/list-builder with-list-builder)
        (only-in :gerbil-parser/src/compiler/bound-ir
                 bind-grammar-ir bound-grammar-ir-ref
                 bound-grammar-ir-section bound-grammar-ir-binding)
        (only-in :gerbil-parser/src/runtime/identity sha256-text))


(def (grammar-ir syntax-kinds terminals)
  (list (cons 'schema "gerbil-parser.grammar-ir.v1")
        (cons 'grammar 'bound-index-witness)
        (cons 'syntax-kinds syntax-kinds)
        (cons 'terminals terminals)
        (cons 'lexical-rules '())
        (cons 'rules '())))

;;; Independent text writer keeps the previous canonical identity contract.
(def (text-digest datum)
  (sha256-text (call-with-output-string (lambda (port) (write datum port)))))

(deflanguage-parser-tests bound-ir-index-test "Bound IR indexed admission"
  (engine)
    (scenario "empty reference digest reuse keeps independent caller-owned strings"
      (sequential ((grammar (grammar-ir '((Left node ()) (Right node ())) '()))
             (result (bind-grammar-ir grammar "empty-reference-copy" '()))
             (left (bound-grammar-ir-ref (bound-grammar-ir-binding result 'syntax-kind 'Left) 'referenceDigest))
             (right (bound-grammar-ir-ref (bound-grammar-ir-binding result 'syntax-kind 'Right) 'referenceDigest))
             (expected (text-digest '())))
        (equal left  expected) (equal right  expected)
        (equal (eq? left right)  #f)
        (act (string-set! left 0 (if (char=? (string-ref left 0) #\0) #\1 #\0)))
        (equal right  expected)
        (scope ((again (bind-grammar-ir grammar "empty-reference-copy" '())))
          (equal (bound-grammar-ir-ref (bound-grammar-ir-binding again 'syntax-kind 'Left) 'referenceDigest)
                  expected))))
    (scenario "byte publication preserves Unicode and every canonical digest"
      (sequential ((grammar (grammar-ir '((|根| node (|值|)) (Word token ()))
                                 '((word Word))))
             (origin "source/λ/中文")
             (lineage '(author "\"escaped\"\nline" #u8(0 255)))
             (result (bind-grammar-ir grammar origin lineage))
             (sections (bound-grammar-ir-ref result 'sections))
             (references
              (with-list-builder (emit)
                (for-each
                 (lambda (section)
                   (for-each
                    (lambda (binding)
                      (for-each emit (bound-grammar-ir-ref binding 'references)))
                    (cdr section)))
                 sections))))
        (equal (bound-grammar-ir-ref result 'grammarDigest)  (text-digest grammar))
        (equal (bound-grammar-ir-ref result 'identityDigest)  (text-digest sections))
        (equal (bound-grammar-ir-ref result 'referenceDigest)  (text-digest references))
        (each (section) (sections)
           (each (binding) ((cdr section))
              (sequential ((namespace (car section))
                     (name (bound-grammar-ir-ref binding 'name))
                     (row (if (eq? namespace 'field)
                            (list name (bound-grammar-ir-ref binding 'scope))
                            (assq name (cdr (assq (case namespace
                                                   ((syntax-kind) 'syntax-kinds)
                                                   ((terminal) 'terminals)) grammar)))))
                     (refs (bound-grammar-ir-ref binding 'references)))
                (equal (bound-grammar-ir-ref binding 'declarationId)
                        (text-digest (list 'bound-index-witness namespace row
                                            (bound-grammar-ir-ref binding 'source) lineage refs)))
                (equal (bound-grammar-ir-ref binding 'referenceDigest)  (text-digest refs)))))))
    (property "closed references preserve declaration order and canonical IR"
      (bindings (source (grammar-ir '((SourceFile node ()) (Word token ()))
                                 '((word Word)))) (first (bind-grammar-ir source "bound-index-test" '())) (second (bind-grammar-ir source "bound-index-test" '())))
      (equal first  second)
      (equal (bound-grammar-ir-ref first 'bindingCount)  3)
      (equal (bound-grammar-ir-ref first 'referenceCount)  1)
      (equal (map (lambda (binding)
                      (bound-grammar-ir-ref binding 'name))
                    (bound-grammar-ir-section first 'syntax-kind))
                '(SourceFile Word)))
    (scenario "duplicate names fail before Bound IR publication"
      (raises
       (bind-grammar-ir
        (grammar-ir '((SourceFile node ()) (SourceFile node ())) '())
        "bound-index-test" '())
       true))
    (property "source indexing retains first entries qualified fields and false values"
      (bindings (source (grammar-ir '((Left node (value)) (Right node (value))) '())) (locations '((syntax-kind (Left . #f) (Left . ignored)
                                      (Right . ((path . "right"))))
                          (field ((Left value) . ((path . "left-field")))
                                 ((Right value) . ((path . "right-field")))))) (result (bind-grammar-ir source "fallback" '() locations)))
      (equal (bound-grammar-ir-ref
                (bound-grammar-ir-binding result 'syntax-kind 'Left) 'source)  #f)
      (equal (bound-grammar-ir-ref
                (bound-grammar-ir-binding result 'field 'value 'Left) 'source)
                '((path . "left-field")))
      (equal (bound-grammar-ir-ref
                (bound-grammar-ir-binding result 'field 'value 'Right) 'source)
                '((path . "right-field")))
      (equal (bound-grammar-ir-ref result 'referenceCount)  4)
      (equal locations  '((syntax-kind (Left . #f) (Left . ignored)
                                         (Right . ((path . "right"))))
                             (field ((Left value) . ((path . "left-field")))
                                    ((Right value) . ((path . "right-field")))))))
    (property "empty first source section wins and missing entries keep defaults"
      (bindings (source (grammar-ir '((Root node ())) '())) (result (bind-grammar-ir source "fallback" '(expansion)
                       '((syntax-kind) (syntax-kind (Root . ignored))))))
      (equal (bound-grammar-ir-ref
                (bound-grammar-ir-binding result 'syntax-kind 'Root) 'source)
                '((path . "fallback") (location . "fallback") (generated? . #f))))
    (property "large binding catalogs avoid variadic reference flattening"
      (bindings (names (map (lambda (index) (string->symbol (number->string index))) (iota 20000))) (source (grammar-ir (map (lambda (name) (list name 'node '())) names) '())) (result (bind-grammar-ir source "large-bound-source" '())))
      (equal (bound-grammar-ir-ref result 'bindingCount)  20000)
      (equal (bound-grammar-ir-ref result 'referenceCount)  0)
      (equal (map (lambda (binding) (bound-grammar-ir-ref binding 'name))
                    (bound-grammar-ir-section result 'syntax-kind))  names))
    (scenario "unresolved references fail closed"
      (raises
       (bind-grammar-ir
        (grammar-ir '((SourceFile node ())) '((word Missing)))
        "bound-index-test" '())
       true)))

;; The 20000-binding scalability Case owns its CPU/GC interval. Other owners
;; must not consume that interval while its real work is silent.
