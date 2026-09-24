#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Section-local declaration and reference indexes preserve Bound IR v1.

(import (only-in :std/test check check-exception test-case test-suite)
        (only-in :gerbil-parser/src/compiler/bound-ir
                 bind-grammar-ir bound-grammar-ir-ref
                 bound-grammar-ir-section))
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
    (test-case "unresolved references fail closed"
      (check-exception
       (bind-grammar-ir
        (grammar-ir '((SourceFile node ())) '((word Missing)))
        "bound-index-test" '())
       true))))
