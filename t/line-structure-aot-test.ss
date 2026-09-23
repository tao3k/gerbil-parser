;;; -*- Gerbil -*-
;;; POO declaration owns contextual-line AOT admission.

(import (only-in :std/test check check-exception test-case test-suite)
        (only-in :gerbil-parser/languages/arithmetic/v1/grammar
                 arithmetic-language-grammar)
        (only-in :gerbil-parser/src/modules/parser/line-structure-objects
                 line-structure? make-line-structure make-heading-line
                 make-heading-fields
                 make-block-line make-block-header make-key-value-line
                 make-inline-link make-text-line make-table-line)
        (only-in :gerbil-parser/src/compiler/line-structure-rowan
                 line-structure-parser-digest line-structure-rowan-source
                 line-structure-rowan-syntax)
        (only-in :gerbil-parser/src/compiler/rust-syntax
                 rust-module-form-item rust-static-form-value
                 rust-struct-form? rust-struct-form-fields rust-field-name))
(export line-structure-aot-test)

(def (fixture-structure (section 'GroupedExpression))
  (make-line-structure
   (make-heading-line "*" " " section 'Expression 'Punctuation)
   (list (make-block-line
          "BEGIN" "END" #f #t
          'PrefixExpression 'Punctuation 'Number 'Punctuation
          'close-at-eof #f #f))
   (make-text-line 'NameExpression 'Number)))

(def line-structure-aot-test
  (test-suite "POO structural-line AOT"
    (test-case "one checked POO declaration resolves canonical parser kinds"
      (let* ((structure (fixture-structure))
             (source (line-structure-rowan-source
                      arithmetic-language-grammar structure)))
        (check (line-structure? structure) => #t)
        (check (and (string-contains source "pub static STRUCTURE: LineStructureSpec")
                    (string-contains source "opening: \"BEGIN\"")
                    (string-contains source "grammar_digest: \"sha256:")
                    (string-contains source "parser_digest: \"sha256:")
                    #t)
               => #t)))
    (test-case "parser identity changes when the POO strategy changes"
      (let* ((original (fixture-structure))
             (changed
              (make-line-structure
               (make-heading-line "*" " " 'GroupedExpression
                                  'Expression 'Punctuation)
               (list (make-block-line
                      "BEGIN" "END" #f #t
                      'PrefixExpression 'Punctuation 'Number 'Punctuation
                      'recover-as-text #f #f))
               (make-text-line 'NameExpression 'Number))))
        (check (line-structure-parser-digest
                arithmetic-language-grammar original)
               => (line-structure-parser-digest
                   arithmetic-language-grammar original))
        (check (equal? (line-structure-parser-digest
                        arithmetic-language-grammar original)
                       (line-structure-parser-digest
                        arithmetic-language-grammar changed))
               => #f)))
    (test-case "parser identity includes the heading boundary policy"
      (let* ((original (fixture-structure))
             (bounded
              (make-line-structure
               (make-heading-line "*" " " 'GroupedExpression
                                  'Expression 'Punctuation)
               (list (make-block-line
                      "BEGIN" "END" #f #t
                      'PrefixExpression 'Punctuation 'Number 'Punctuation
                      'close-at-eof #t #f))
               (make-text-line 'NameExpression 'Number))))
        (check (equal? (line-structure-parser-digest
                        arithmetic-language-grammar original)
                       (line-structure-parser-digest
                        arithmetic-language-grammar bounded))
               => #f)))
    (test-case "recursive block contents is Scheme-owned and digest-bound"
      (let* ((recursive
              (make-line-structure
               (make-heading-line "*" " " 'GroupedExpression
                                  'Expression 'Punctuation)
               (list (make-block-line
                      "BEGIN" "END" #f #t
                      'PrefixExpression 'Punctuation 'Number 'Punctuation
                      'close-at-eof #f #f #f 'elements))
               (make-text-line 'NameExpression 'Number)))
             (source (line-structure-rowan-source
                      arithmetic-language-grammar recursive)))
        (check (and (string-contains source "contents: BlockContents::Elements") #t)
               => #t)
        (check (equal? (line-structure-parser-digest
                        arithmetic-language-grammar recursive)
                       (line-structure-parser-digest
                        arithmetic-language-grammar (fixture-structure)))
               => #f)))
    (test-case "recursive block contents rejects key-value body semantics"
      (check-exception
       (make-block-line
        "BEGIN" "END" #f #t
        'PrefixExpression 'Punctuation 'Number 'Punctuation
        'recover-as-text #f
        (make-key-value-line ":" 'NameExpression
                             'Number 'Number 'Punctuation)
        #f 'elements)
       true))
    (test-case "typed heading fields are projected and bound to identity"
      (let* ((structure
              (make-line-structure
               (make-heading-line "*" " " 'GroupedExpression
                                  'Expression 'Punctuation
                                  (make-heading-fields 'Number 'Punctuation))
               (list (make-block-line
                      "BEGIN" "END" #f #t
                      'PrefixExpression 'Punctuation 'Number 'Punctuation
                      'close-at-eof #f #f))
               (make-text-line 'NameExpression 'Number)))
             (source (line-structure-rowan-source
                      arithmetic-language-grammar structure)))
        (check (and (string-contains source "fields: Some(HeadingFieldsRule") #t)
               => #t)
        (check (equal? (line-structure-parser-digest
                        arithmetic-language-grammar structure)
                       (line-structure-parser-digest
                        arithmetic-language-grammar (fixture-structure)))
               => #f)))
    (test-case "typed key-value body rules are projected and bound to identity"
      (let* ((body (make-key-value-line ":" 'NameExpression
                                        'Number 'Number 'Punctuation))
             (structure
              (make-line-structure
               (make-heading-line "*" " " 'GroupedExpression
                                  'Expression 'Punctuation)
               (list (make-block-line
                      "BEGIN" "END" #f #t
                      'PrefixExpression 'Punctuation 'Number 'Punctuation
                      'recover-as-text #t body))
               (make-text-line 'NameExpression 'Number)))
             (source (line-structure-rowan-source
                      arithmetic-language-grammar structure)))
        (check (and (string-contains source "body_line: Some(KeyValueLineRule") #t)
               => #t)
        (check (equal? (line-structure-parser-digest
                        arithmetic-language-grammar structure)
                       (line-structure-parser-digest
                        arithmetic-language-grammar (fixture-structure)))
               => #f)))
    (test-case "typed block header changes parser identity and emits token kinds"
      (let* ((header (make-block-header 'Number 'Punctuation))
             (structure
              (make-line-structure
               (make-heading-line "*" " " 'GroupedExpression
                                  'Expression 'Punctuation)
               (list (make-block-line
                      "BEGIN" "END" #f #t
                      'PrefixExpression 'Punctuation 'Number 'Punctuation
                      'close-at-eof #f #f header))
               (make-text-line 'NameExpression 'Number)))
             (source (line-structure-rowan-source
                      arithmetic-language-grammar structure)))
        (check (and (string-contains source "header: Some(BlockHeaderRule")
                    (string-contains source "argument_token:") #t)
               => #t)
        (check (equal? (line-structure-parser-digest
                        arithmetic-language-grammar structure)
                       (line-structure-parser-digest
                        arithmetic-language-grammar (fixture-structure)))
               => #f)))
    (test-case "typed inline link is bound to parser identity"
      (let* ((link (make-inline-link "[[" "][" "]]"
                                     'NameExpression 'Number 'Number 'Punctuation))
             (structure
              (make-line-structure
               (make-heading-line "*" " " 'GroupedExpression
                                  'Expression 'Punctuation)
               (list (make-block-line
                      "BEGIN" "END" #f #t
                      'PrefixExpression 'Punctuation 'Number 'Punctuation
                      'close-at-eof #f #f))
               (make-text-line 'NameExpression 'Number link)))
             (source (line-structure-rowan-source
                      arithmetic-language-grammar structure)))
        (check (and (string-contains source "inline_link: Some(InlineLinkRule") #t)
               => #t)
        (check (equal? (line-structure-parser-digest
                        arithmetic-language-grammar structure)
                       (line-structure-parser-digest
                        arithmetic-language-grammar (fixture-structure)))
               => #f)))
    (test-case "paragraph grouping is a POO-declared node and parser identity"
      (let* ((structure
              (make-line-structure
               (make-heading-line "*" " " 'GroupedExpression
                                  'Expression 'Punctuation)
               (list (make-block-line
                      "BEGIN" "END" #f #t
                      'PrefixExpression 'Punctuation 'Number 'Punctuation
                      'close-at-eof #f #f))
               (make-text-line 'NameExpression 'Number #f 'NameExpression)))
             (source (line-structure-rowan-source
                      arithmetic-language-grammar structure)))
        (check (and (string-contains source "paragraph_node: Some(") #t)
               => #t)
        (check (equal? (line-structure-parser-digest
                        arithmetic-language-grammar structure)
                       (line-structure-parser-digest
                        arithmetic-language-grammar (fixture-structure)))
               => #f)))
    (test-case "table rules project through the typed Rust syntax macro"
      (let* ((table (make-table-line "|" 'GroupedExpression
                                     'NameExpression 'Expression 'NameExpression
                                     'Punctuation 'Number 'Punctuation 'Punctuation))
             (structure
              (make-line-structure
               (make-heading-line "*" " " 'GroupedExpression
                                  'Expression 'Punctuation)
               '()
               (make-text-line 'NameExpression 'Number)
               table))
             (syntax (line-structure-rowan-syntax
                      arithmetic-language-grammar structure))
             (value (rust-static-form-value (rust-module-form-item syntax))))
        (check (rust-struct-form? value) => #t)
        (check (map rust-field-name (rust-struct-form-fields value))
               => '(grammar_digest parser_digest heading blocks
                        paragraph_node table text_node text_token inline_link))
        (check (equal? (line-structure-parser-digest
                        arithmetic-language-grammar structure)
                       (line-structure-parser-digest
                        arithmetic-language-grammar (fixture-structure)))
               => #f)))
    (test-case "wrong-category kind is rejected at the AOT boundary"
      (check (with-catch
              (lambda (error) #t)
              (lambda ()
                (line-structure-rowan-source
                 arithmetic-language-grammar
                 (fixture-structure 'Number))
                #f))
             => #t))
    (test-case "untyped block values cannot enter the POO contract"
      (check (with-catch
              (lambda (error) #t)
              (lambda ()
                (make-line-structure
                 (make-heading-line "*" " "
                                    'GroupedExpression 'Expression 'Punctuation)
                 (list 'not-a-block)
                 (make-text-line 'NameExpression 'Number))
                #f))
             => #t))
    (test-case "untyped key-value body rules cannot enter the POO contract"
      (check (with-catch
              (lambda (error) #t)
              (lambda ()
                (make-block-line "BEGIN" "END" #f #t
                                 'PrefixExpression 'Punctuation
                                 'Number 'Punctuation
                                 'recover-as-text #t 'not-a-body-rule)
                #f))
             => #t))
    (test-case "an undeclared EOF recovery mode is rejected"
      (check (with-catch
              (lambda (error) #t)
              (lambda ()
                (make-block-line "BEGIN" "END" #f #t
                                 'PrefixExpression 'Punctuation
                                 'Number 'Punctuation 'guess #f #f)
                #f))
             => #t))))
