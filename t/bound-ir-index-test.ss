#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Section-local declaration and reference indexes preserve Bound IR v1.

(import (only-in :std/test check check-exception test-case test-suite)
        (only-in :gerbil-parser/src/compiler/bound-ir
                 bind-grammar-ir bound-grammar-ir-ref
                 bound-grammar-ir-section bound-grammar-ir-binding))
(export bound-ir-index-test)

(def (grammar-ir syntax-kinds terminals)
  (list (cons 'schema "gerbil-parser.grammar-ir.v1")
        (cons 'grammar 'bound-index-witness)
        (cons 'syntax-kinds syntax-kinds)
        (cons 'terminals terminals)
        (cons 'lexical-rules '())
        (cons 'rules '())))

(def bound-ir-index-test
  (test-suite "Bound IR indexed admission"
    (test-case "closed references preserve declaration order and canonical IR"
      (let* ((source (grammar-ir '((SourceFile node ()) (Word token ()))
                                 '((word Word))))
             (first (bind-grammar-ir source "bound-index-test" '()))
             (second (bind-grammar-ir source "bound-index-test" '())))
        (check first => second)
        (check (bound-grammar-ir-ref first 'bindingCount) => 3)
        (check (bound-grammar-ir-ref first 'referenceCount) => 1)
        (check (map (lambda (binding)
                      (bound-grammar-ir-ref binding 'name))
                    (bound-grammar-ir-section first 'syntax-kind))
               => '(SourceFile Word))))
    (test-case "duplicate names fail before Bound IR publication"
      (check-exception
       (bind-grammar-ir
        (grammar-ir '((SourceFile node ()) (SourceFile node ())) '())
        "bound-index-test" '())
       true))
    (test-case "source indexing retains first entries qualified fields and false values"
      (let* ((source (grammar-ir '((Left node (value)) (Right node (value))) '()))
             (locations '((syntax-kind (Left . #f) (Left . ignored)
                                      (Right . ((path . "right"))))
                          (field ((Left value) . ((path . "left-field")))
                                 ((Right value) . ((path . "right-field"))))))
             (result (bind-grammar-ir source "fallback" '() locations)))
        (check (bound-grammar-ir-ref
                (bound-grammar-ir-binding result 'syntax-kind 'Left) 'source) => #f)
        (check (bound-grammar-ir-ref
                (bound-grammar-ir-binding result 'field 'value 'Left) 'source)
               => '((path . "left-field")))
        (check (bound-grammar-ir-ref
                (bound-grammar-ir-binding result 'field 'value 'Right) 'source)
               => '((path . "right-field")))
        (check (bound-grammar-ir-ref result 'referenceCount) => 4)
        (check locations => '((syntax-kind (Left . #f) (Left . ignored)
                                         (Right . ((path . "right"))))
                             (field ((Left value) . ((path . "left-field")))
                                    ((Right value) . ((path . "right-field"))))))))
    (test-case "empty first source section wins and missing entries keep defaults"
      (let* ((source (grammar-ir '((Root node ())) '()))
             (result (bind-grammar-ir source "fallback" '(expansion)
                       '((syntax-kind) (syntax-kind (Root . ignored))))))
        (check (bound-grammar-ir-ref
                (bound-grammar-ir-binding result 'syntax-kind 'Root) 'source)
               => '((path . "fallback") (location . "fallback") (generated? . #f)))))
    (test-case "large binding catalogs avoid variadic reference flattening"
      (let* ((names (map (lambda (index) (string->symbol (number->string index))) (iota 20000)))
             (source (grammar-ir (map (lambda (name) (list name 'node '())) names) '()))
             (result (bind-grammar-ir source "large-bound-source" '())))
        (check (bound-grammar-ir-ref result 'bindingCount) => 20000)
        (check (bound-grammar-ir-ref result 'referenceCount) => 0)
        (check (map (lambda (binding) (bound-grammar-ir-ref binding 'name))
                    (bound-grammar-ir-section result 'syntax-kind)) => names)))
    (test-case "unresolved references fail closed"
      (check-exception
       (bind-grammar-ir
        (grammar-ir '((SourceFile node ())) '((word Missing)))
        "bound-index-test" '())
       true))))
