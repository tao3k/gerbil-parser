(import :gerbil-parser/language-test-support)
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
        (only-in :gerbil-parser/src/runtime/machine
                 parser-machine-grammar-digest)
        (only-in :gerbil-parser/src/runtime/lr-parser lr-prepare)
        (only-in :gerbil-parser/rust-runtime-support
                 generate-language-rust-runtime-module)
        (only-in :gerbil-parser/src/compiler/rust-runtime
                 rust-runtime-module-source generate-rust-runtime-module))
;;; GQL emission builds a large transient Rust string in the shared VM. Keep
;;; its allocation phase separate from other large compiler/index owners.

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
                        (cond
                         ((eq? (car row) 'productions)
                          (cons 'productions
                                (append (cdr row)
                                        (list (cons (or id (length (cdr row))) tail)))))
                         ((eq? (car row) 'declarations)
                          ;; Keep the declaration directory well formed even
                          ;; when the fixture deliberately corrupts the lhs.
                          (cons 'declarations
                                (cons (if (symbol? (car tail)) (car tail) 'unused)
                                      (cdr row))))
                         (else row)))
                      (cdr entry)))
           entry))
       arithmetic-parser-ir))

(deflanguage-parser-tests rust-runtime-generation-test "Rust AOT generation"
  (engine)
    (scenario "Rust target admits exact i32 precedence boundaries"
      (each (value) ('(-2147483648 0 2147483647))
         (scope ((source (rust-runtime-module-source
                       "arithmetic" +arithmetic-language-version+ +arithmetic-syntax-contract+
                       (parser-machine-grammar-digest arithmetic-parser)
                       (arithmetic-ir-with-production (list 'unused '() 'concat (list 'dynamic value))))))
           (equal (and (string-contains source
                        (string-append "dynamic_precedence: " (number->string value))) #t)  #t))))
    (scenario "Rust target failures preserve an existing output before emission"
      (each (tail) ('((unused () concat (dynamic -2147483649))
         (unused () concat (dynamic 2147483648))
         (unused () concat (dynamic 1267650600228229401496703205376))
         (unused () layout-end #f)
         (unused ((terminal layout-start "{")) concat #f)
         (unused ((marked (terminal layout-next ";") ((field separator)))) concat #f)))
         (scope ((path (make-temporary-file-name "unrepresentable-parser"))
               (ir (arithmetic-ir-with-production tail)))
           ;; Target limits do not redefine canonical Scheme legality.
           (act (lr-prepare (cdr (assq 'lr-spec ir))))
           (protect (body
            (group
              (act (call-with-output-file path (lambda (port) (display "preserved\n" port))))
              (raises
               (generate-rust-runtime-module path "arithmetic" +arithmetic-language-version+
                                             +arithmetic-syntax-contract+
                                             (parser-machine-grammar-digest arithmetic-parser)
                                             ir)
               (lambda (condition) (string-prefix? "Rust LR" (error-message condition))))
              (equal (call-with-input-file path read-line)  "preserved"))
            ) (cleanup (when (file-exists? path) (act (delete-file path))))))))
    (scenario "Rust kind capacity is checked before file creation"
      (scope ((path (make-temporary-file-name "oversized-parser-catalog"))
            (ir (map (lambda (entry)
                       (if (eq? (car entry) 'syntax-kinds)
                         (cons 'syntax-kinds (make-list 65537 (car (cdr entry))))
                         entry)) arithmetic-parser-ir)))
        (protect (body
         (group
           (raises
            (generate-rust-runtime-module path "arithmetic" +arithmetic-language-version+
                                          +arithmetic-syntax-contract+
                                          (parser-machine-grammar-digest arithmetic-parser) ir)
            (lambda (condition)
              (equal? (error-message condition) "Rust LR kind catalog exceeds u16 capacity")))
           (equal (file-exists? path)  #f))
         ) (cleanup (when (file-exists? path) (act (delete-file path)))))))
    (scenario "raw Rust publication rejects invalid table domains and layout capability"
      (each (tables) ('((#((((terminal token number) shift 1 #f))) #(()))
         (#((((terminal token undeclared) reduce 0) ((terminal eof) accept))) #(()))
         (#((((terminal eof) accept))) #(((undeclared . 0))))
         (#(() ()) #(() ()))
         (#(() (((terminal token number) shift 1 #f) ((terminal eof) accept))) #(() ()))
         (#(()) #(((root . 1))))
         (#((((terminal token number) shift 0 #f) ((terminal token number) reduce 0))) #(()))
         (#((((terminal token number) layout-guard (shift 0 #f) (reduce 0)))) #(()))
         (#((((terminal layout-start "{") shift 0 #f))) #(()))
         (#((((terminal layout-next ";") shift 0 #f))) #(()))))
         (scope ((path (make-temporary-file-name "invalid-parser-table"))
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
           (protect (body
            (group
(locals ((reject! (parser-check ()
                (raises
                 (generate-rust-runtime-module path "arithmetic" +arithmetic-language-version+
                                               +arithmetic-syntax-contract+
                                               (parser-machine-grammar-digest arithmetic-parser) ir)
                 (lambda (condition) (string-prefix? "invalid LR table" (error-message condition)))))))

              (verify reject! )
              (equal (file-exists? path)  #f)
              (act (call-with-output-file path (lambda (port) (display "preserve-existing-output" port))))
              (verify reject! )
              (equal (call-with-input-file path read-line)  "preserve-existing-output")))
            ) (cleanup (when (file-exists? path) (act (delete-file path))))))))
    (scenario "unresolved dead references cannot create or overwrite Rust products"
      (each (operand) ('((nonterminal missing) (marked (nonterminal missing) ((field value) (alias Item)))))
         (scope ((ir (arithmetic-ir-with-production (list 'unused (list operand) 'pass #f)))
               (path (make-temporary-file-name "unresolved-parser-reference")))
           (protect (body
            (group
(locals ((reject! (parser-check ()
                (raises
                 (generate-rust-runtime-module path "arithmetic" +arithmetic-language-version+
                                               +arithmetic-syntax-contract+
                                               (parser-machine-grammar-digest arithmetic-parser) ir)
                 (lambda (condition)
                   (equal? (error-message condition) "invalid canonical LR unresolved reference"))))))

              (verify reject! )
              (equal (file-exists? path)  #f)
              (act (call-with-output-file path (lambda (port) (display "preserved" port))))
              (verify reject! )
              (equal (call-with-input-file path read-line)  "preserved")))
            ) (cleanup (when (file-exists? path) (act (delete-file path))))))))
    (scenario "malformed dead productions preserve an existing output file"
      (each (tail) ('((unused () pass #f)
         (unused ((terminal token number)) layout-end #f)
         (unused () (layout-end "") #f)
         (unused () concat (left "10"))
         ("unused" () concat #f)))
         (scope ((path (make-temporary-file-name "invalid-parser-production")))
           (protect (body
            (group
              (act (call-with-output-file path (lambda (port) (display "preserved\n" port))))
              (raises
               (generate-rust-runtime-module path "arithmetic" +arithmetic-language-version+
                                             +arithmetic-syntax-contract+
                                             (parser-machine-grammar-digest arithmetic-parser)
                                             (arithmetic-ir-with-production tail))
               (lambda (condition)
                 (equal? (error-message condition) "invalid canonical LR production")))
              (equal (call-with-input-file path read-line)  "preserved"))
            ) (cleanup (when (file-exists? path) (act (delete-file path)))))))
      (raises
       (rust-runtime-module-source "arithmetic" +arithmetic-language-version+
                                   +arithmetic-syntax-contract+
                                   (parser-machine-grammar-digest arithmetic-parser)
                                   (arithmetic-ir-with-production '(unused () concat #f) 0))
       (lambda (condition) (equal? (error-message condition) "invalid canonical LR production"))))
    (scenario "invalid action names in unused rules fail before file publication"
      (each (actions) ('(((field "left")) ((field #f)) ((alias 1)) ((alias (Item)))
         ((field Name extra)) ((field Name) . invalid)))
         (sequential ((ir (arithmetic-ir-with-production
                    (list 'unused (list (list 'marked '(terminal token number) actions)) 'concat #f)))
                (path (make-temporary-file-name "invalid-parser-actions")))
           (protect (body
            (group
              (raises
               (generate-rust-runtime-module path "arithmetic" +arithmetic-language-version+
                                             +arithmetic-syntax-contract+
                                             (parser-machine-grammar-digest arithmetic-parser) ir)
               (lambda (condition)
                 (and (equal? (error-message condition) "invalid LR operand actions")
                      (equal? (error-irritants condition) (list actions)))))
              (equal (file-exists? path)  #f))
            ) (cleanup (when (file-exists? path) (act (delete-file path))))))))
    (scenario "one Parser IR produces one deterministic Rust module"
      (scope ((first (arithmetic-rust-source))
            (second (arithmetic-rust-source)))
        (equal first  second)
        (equal (and
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
                #t)))
    (property "GQL lowers its lexical algebra and selective-GLR tables"
      (bindings (source
            (rust-runtime-module-source
             "gql"
             "edition-1-2024-04"
             +gql-syntax-contract+
             (parser-machine-grammar-digest gql-parser)
             gql-parser-ir)))
      (equal (and (string-contains source "LexicalExpr::NumberLiteral")
                    (string-contains source "LexicalExpr::Choice")
                    (string-contains source "ParserAction::Fork(&[")
                    (string-contains source "dynamic_precedence:")
                    #t)
                #t))
    (property "whole-line lexical IR lowers to the generic Rust engine"
      (bindings (line-ir
              (map (lambda (entry)
                     (if (eq? (car entry) 'lexical-rules)
                       (cons 'lexical-rules
                             (map (lambda (row)
                                    (if (eq? (car row) 'number)
                                      '(number (line))
                                      row))
                                  (cdr entry)))
                       entry))
                   arithmetic-parser-ir)) (source
              (rust-runtime-module-source
               "arithmetic" +arithmetic-language-version+
               +arithmetic-syntax-contract+
               (parser-machine-grammar-digest arithmetic-parser)
               line-ir)))
      (equal (and (string-contains source
                                     "LexicalRule { terminal: \"number\", expression: LexicalExpr::Line")
                    #t)
                #t))
    (property "bounded character runs lower without language-specific code"
      (bindings (run-ir
              (map (lambda (entry)
                     (if (eq? (car entry) 'lexical-rules)
                       (cons 'lexical-rules
                             (map (lambda (row)
                                    (if (eq? (car row) 'number)
                                      '(number (character-run "-" 4))
                                      row))
                                  (cdr entry)))
                       entry))
                   arithmetic-parser-ir)) (source
              (rust-runtime-module-source
               "arithmetic" +arithmetic-language-version+
               +arithmetic-syntax-contract+
               (parser-machine-grammar-digest arithmetic-parser)
               run-ir)))
      (equal (and
                (string-contains
                 source
                 "LexicalExpr::CharacterRun { character: \"-\", minimum: 4 }")
                #t)
                #t))
    (scenario "the language descriptor is the complete generation input"
      (scope ((path (make-temporary-file-name "gerbil-parser-runtime")))
        (protect (body
         (group
           (act (generate-language-rust-runtime-module
            path arithmetic-language-grammar))
           (equal (call-with-input-file
                   path (lambda (port) (read-line port)))
                   "// @generated by gerbil-parser/src/compiler/rust-runtime.ss"))
         ) (cleanup
          (when (file-exists? path) (act (delete-file path))))))))
