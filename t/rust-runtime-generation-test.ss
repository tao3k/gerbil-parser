;;; -*- Gerbil -*-
;;; Parser IR v1 is the sole authority for deterministic Rust output.

(import (only-in :std/io/tempfile make-temporary-file-name)
        (only-in :std/test check check-exception test-case test-suite)
        (only-in :gerbil-parser/languages/arithmetic/parser
                 +arithmetic-language-version+
                 +arithmetic-syntax-contract+
                 arithmetic-language-grammar
                 arithmetic-parser-ir
                 arithmetic-parser)
        (only-in :gerbil-parser/languages/gql/parser
                 +gql-syntax-contract+
                 gql-parser-ir
                 gql-parser)
        (only-in :gerbil-parser/src/compiler/machine
                 parser-machine-grammar-digest)
        (only-in :gerbil-parser/src/runtime/lr-parser lr-prepare)
        (only-in :gerbil-parser/rust-runtime-support
                 generate-language-rust-runtime-module)
        (only-in :gerbil-parser/src/compiler/rust-runtime
                 rust-runtime-module-source generate-rust-runtime-module))
(export rust-runtime-generation-test)

(def (arithmetic-rust-source)
  (rust-runtime-module-source
   "arithmetic"
   +arithmetic-language-version+
   +arithmetic-syntax-contract+
   (parser-machine-grammar-digest arithmetic-parser)
   arithmetic-parser-ir))

;; Append a dead canonical rule while preserving the real language tables.
(def (arithmetic-ir-with-production tail (id #f))
  (map (lambda (entry)
         (if (eq? (car entry) 'lr-spec)
           (cons 'lr-spec
                 (map (lambda (row)
                        (if (eq? (car row) 'productions)
                          (cons 'productions
                                (append (cdr row)
                                        (list (cons (or id (length (cdr row))) tail))))
                          row))
                      (cdr entry)))
           entry))
       arithmetic-parser-ir))

(def rust-runtime-generation-test
  (test-suite "Rust AOT generation"
    (test-case "Rust target admits exact i32 precedence boundaries"
      (for-each
       (lambda (value)
         (let (source (rust-runtime-module-source
                       "arithmetic" +arithmetic-language-version+ +arithmetic-syntax-contract+
                       (parser-machine-grammar-digest arithmetic-parser)
                       (arithmetic-ir-with-production (list 'unused '() 'concat (list 'dynamic value)))))
           (check (and (string-contains source
                        (string-append "dynamic_precedence: " (number->string value))) #t) => #t)))
       '(-2147483648 0 2147483647)))
    (test-case "Rust target failures preserve an existing output before emission"
      (for-each
       (lambda (tail)
         (let ((path (make-temporary-file-name "unrepresentable-parser"))
               (ir (arithmetic-ir-with-production tail)))
           ;; Target limits do not redefine canonical Scheme legality.
           (lr-prepare (cdr (assq 'lr-spec ir)))
           (try
            (begin
              (call-with-output-file path (lambda (port) (display "preserved\n" port)))
              (check-exception
               (generate-rust-runtime-module path "arithmetic" +arithmetic-language-version+
                                             +arithmetic-syntax-contract+
                                             (parser-machine-grammar-digest arithmetic-parser)
                                             ir)
               (lambda (condition) (string-prefix? "Rust LR" (error-message condition))))
              (check (call-with-input-file path read-line) => "preserved"))
            (finally (when (file-exists? path) (delete-file path))))))
       '((unused () concat (dynamic -2147483649))
         (unused () concat (dynamic 2147483648))
         (unused () concat (dynamic 1267650600228229401496703205376))
         (unused () layout-end #f)
         (unused ((terminal layout-start "{")) concat #f)
         (unused ((marked (terminal layout-next ";") ((field separator)))) concat #f))))
    (test-case "Rust kind capacity is checked before file creation"
      (let ((path (make-temporary-file-name "oversized-parser-catalog"))
            (ir (map (lambda (entry)
                       (if (eq? (car entry) 'syntax-kinds)
                         (cons 'syntax-kinds (make-list 65537 (car (cdr entry))))
                         entry)) arithmetic-parser-ir)))
        (try
         (begin
           (check-exception
            (generate-rust-runtime-module path "arithmetic" +arithmetic-language-version+
                                          +arithmetic-syntax-contract+
                                          (parser-machine-grammar-digest arithmetic-parser) ir)
            (lambda (condition)
              (equal? (error-message condition) "Rust LR kind catalog exceeds u16 capacity")))
           (check (file-exists? path) => #f))
         (finally (when (file-exists? path) (delete-file path))))))
    (test-case "raw Rust publication rejects invalid table domains and layout capability"
      (for-each
       (lambda (tables)
         (let ((path (make-temporary-file-name "invalid-parser-table"))
               (ir (map (lambda (entry)
                          (if (eq? (car entry) 'lr-spec)
                            (cons 'lr-spec
                                  (map (lambda (row)
                                         (case (car row)
                                           ((actions) (cons 'actions (car tables)))
                                           ((gotos) (cons 'gotos (cadr tables)))
                                           (else row)))
                                       (cdr entry)))
                            entry)) arithmetic-parser-ir)))
           (try
            (begin
              (check-exception
               (generate-rust-runtime-module path "arithmetic" +arithmetic-language-version+
                                             +arithmetic-syntax-contract+
                                             (parser-machine-grammar-digest arithmetic-parser) ir)
               (lambda (condition) (string-prefix? "invalid LR table" (error-message condition))))
              (check (file-exists? path) => #f))
            (finally (when (file-exists? path) (delete-file path))))))
       '((#((((terminal token number) shift 1 #f))) #(()))
         (#(()) #(((root . 1))))
         (#((((terminal token number) shift 0 #f) ((terminal token number) reduce 0))) #(()))
         (#((((terminal token number) layout-guard (shift 0 #f) (reduce 0)))) #(()))
         (#((((terminal layout-start "{") shift 0 #f))) #(()))
         (#((((terminal layout-next ";") shift 0 #f))) #(())))))
    (test-case "malformed dead productions preserve an existing output file"
      (for-each
       (lambda (tail)
         (let (path (make-temporary-file-name "invalid-parser-production"))
           (try
            (begin
              (call-with-output-file path (lambda (port) (display "preserved\n" port)))
              (check-exception
               (generate-rust-runtime-module path "arithmetic" +arithmetic-language-version+
                                             +arithmetic-syntax-contract+
                                             (parser-machine-grammar-digest arithmetic-parser)
                                             (arithmetic-ir-with-production tail))
               (lambda (condition)
                 (equal? (error-message condition) "invalid canonical LR production")))
              (check (call-with-input-file path read-line) => "preserved"))
            (finally (when (file-exists? path) (delete-file path))))))
       '((unused () pass #f)
         (unused ((terminal token number)) layout-end #f)
         (unused () (layout-end "") #f)
         (unused () concat (left "10"))
         ("unused" () concat #f)))
      (check-exception
       (rust-runtime-module-source "arithmetic" +arithmetic-language-version+
                                   +arithmetic-syntax-contract+
                                   (parser-machine-grammar-digest arithmetic-parser)
                                   (arithmetic-ir-with-production '(unused () concat #f) 0))
       (lambda (condition) (equal? (error-message condition) "invalid canonical LR production"))))
    (test-case "invalid action names in unused rules fail before file publication"
      (for-each
       (lambda (actions)
         (let* ((ir (arithmetic-ir-with-production
                    (list 'unused (list (list 'marked '(terminal token number) actions)) 'concat #f)))
                (path (make-temporary-file-name "invalid-parser-actions")))
           (try
            (begin
              (check-exception
               (generate-rust-runtime-module path "arithmetic" +arithmetic-language-version+
                                             +arithmetic-syntax-contract+
                                             (parser-machine-grammar-digest arithmetic-parser) ir)
               (lambda (condition)
                 (and (equal? (error-message condition) "invalid LR operand actions")
                      (equal? (error-irritants condition) (list actions)))))
              (check (file-exists? path) => #f))
            (finally (when (file-exists? path) (delete-file path))))))
       '(((field "left")) ((field #f)) ((alias 1)) ((alias (Item)))
         ((field Name extra)) ((field Name) . invalid))))
    (test-case "one Parser IR produces one deterministic Rust module"
      (let ((first (arithmetic-rust-source))
            (second (arithmetic-rust-source)))
        (check first => second)
        (check (and
                (string-contains first
                 "Source authority: canonical Grammar IR v1")
                (string-contains first
                 "use gerbil_parser_runtime::{")
                (string-contains first
                 (parser-machine-grammar-digest arithmetic-parser))
                (string-contains first "ParserAction::Shift(9)")
                (string-contains first "OperandAction::Alias(0)")
                (string-contains first
                 "LexicalExpr::Literals(&[\"+\", \"-\", \"*\"")
                #t)
               => #t)))
    (test-case "GQL lowers its lexical algebra and selective-GLR tables"
      (let (source
            (rust-runtime-module-source
             "gql"
             "edition-1-2024-04"
             +gql-syntax-contract+
             (parser-machine-grammar-digest gql-parser)
             gql-parser-ir))
        (check (and (string-contains source "LexicalExpr::NumberLiteral")
                    (string-contains source "LexicalExpr::Choice")
                    (string-contains source "ParserAction::Fork(&[")
                    (string-contains source "dynamic_precedence:")
                    #t)
               => #t)))
    (test-case "whole-line lexical IR lowers to the generic Rust engine"
      (let* ((line-ir
              (map (lambda (entry)
                     (if (eq? (car entry) 'lexical-rules)
                       (cons 'lexical-rules
                             (map (lambda (row)
                                    (if (eq? (car row) 'number)
                                      '(number (line))
                                      row))
                                  (cdr entry)))
                       entry))
                   arithmetic-parser-ir))
             (source
              (rust-runtime-module-source
               "arithmetic" +arithmetic-language-version+
               +arithmetic-syntax-contract+
               (parser-machine-grammar-digest arithmetic-parser)
               line-ir)))
        (check (and (string-contains source
                                     "LexicalRule { terminal: \"number\", expression: LexicalExpr::Line")
                    #t)
               => #t)))
    (test-case "bounded character runs lower without language-specific code"
      (let* ((run-ir
              (map (lambda (entry)
                     (if (eq? (car entry) 'lexical-rules)
                       (cons 'lexical-rules
                             (map (lambda (row)
                                    (if (eq? (car row) 'number)
                                      '(number (character-run "-" 4))
                                      row))
                                  (cdr entry)))
                       entry))
                   arithmetic-parser-ir))
             (source
              (rust-runtime-module-source
               "arithmetic" +arithmetic-language-version+
               +arithmetic-syntax-contract+
               (parser-machine-grammar-digest arithmetic-parser)
               run-ir)))
        (check (and
                (string-contains
                 source
                 "LexicalExpr::CharacterRun { character: \"-\", minimum: 4 }")
                #t)
               => #t)))
    (test-case "the language descriptor is the complete generation input"
      (let (path (make-temporary-file-name "gerbil-parser-runtime"))
        (try
         (begin
           (generate-language-rust-runtime-module
            path arithmetic-language-grammar)
           (check (call-with-input-file
                   path (lambda (port) (read-line port)))
                  => "// @generated by gerbil-parser/src/compiler/rust-runtime.ss"))
         (finally
          (when (file-exists? path) (delete-file path))))))))
