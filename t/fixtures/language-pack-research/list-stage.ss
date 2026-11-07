;;; -*- Gerbil -*-
;;; Phase-1 POO normalization enters canonical compilation without DSL replay.
(import (only-in "../../../src/language/assembly" assemble-language-parser)
        (only-in "../../../src/language/descriptor" make-language-grammar)
        (for-syntax "list-roles" "list-origins"
                    (only-in "../../../src/compiler/normalize"
                             compile-grammar/context normalized-grammar-ir
                             normalized-grammar-receipt normalized-grammar-source-map)
                    (only-in "../../../src/compiler/language-artifact"
                             make-language-declaration make-admitted-language-declaration
                             expand-admitted-language-declaration-syntax)
                    (only-in "../../../src/compiler/parser-ir" compile-parser)
                    (only-in "../../../src/compiler/bound-ir" bind-grammar-ir)))
(export deflist-study defsingle-list-study defnative-list-study defnative-single-list-study)

(begin-syntax
(def (expand-list-study stx composed identities lineage)
  (syntax-case stx ()
    ((_ prefix composed-binding receipt-binding)
     (let* ((normalized (compile-grammar/context composed list-study-row-source))
            (ir (normalized-grammar-ir normalized))
            (receipt (normalized-grammar-receipt normalized))
            (origin (let (source (stx-source stx))
                      (if source (call-with-output-string (lambda (port) (display source port)))
                          "<unknown>")))
            (sources (normalized-grammar-source-map normalized origin))
            (declaration
             (make-language-declaration
              stx #'prefix (syntax->list (datum->syntax #'prefix identities))
              '() 'reject #f lineage sources)))
       (with-syntax
           ((composed-value (datum->syntax #'prefix (list 'quote ir)))
            (receipt-value (datum->syntax #'prefix (list 'quote receipt)))
            (compiled
             (expand-admitted-language-declaration-syntax
              declaration (make-admitted-language-declaration ir origin lineage sources)
              compile-parser bind-grammar-ir
              #'assemble-language-parser #'make-language-grammar #'begin #'def)))
         #'(begin
             (def composed-binding composed-value)
             (def receipt-binding receipt-value)
             compiled))))))

)

(defsyntax (deflist-study stx)
  (expand-list-study stx (make-list-study-grammar)
    '("list-study" "v1" "list-study.local.v1")
    '(deflist-study make-nonempty-list-role)))

(defsyntax (defsingle-list-study stx)
  (expand-list-study stx (make-single-argument-list-grammar)
    '("list-study-single" "v1" "list-study-single.local.v1")
    '(defsingle-list-study make-single-argument-list-grammar)))

(defsyntax (defnative-list-study stx)
  (expand-list-study stx (make-native-list-grammar)
    '("native-list-study" "v1" "native-list-study.local.v1")
    '(defnative-list-study native-poo-slot-composition)))

(defsyntax (defnative-single-list-study stx)
  (expand-list-study stx (make-native-single-list-grammar)
    '("native-list-study-single" "v1" "native-list-study-single.local.v1")
    '(defnative-single-list-study native-poo-inherited-computation)))
