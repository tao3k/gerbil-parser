;;; -*- Gerbil -*-
;;; POO declaration owns contextual-line AOT admission.

(import (only-in :std/test check check-exception test-case test-suite)
        (only-in :gerbil-parser/languages/arithmetic/v1/grammar
                 arithmetic-language-grammar)
        (only-in :gerbil-parser/src/modules/parser/line-structure-objects
                 line-structure? make-line-structure make-heading-line
                 make-heading-fields
                 make-block-line make-block-header make-key-value-line
                 make-inline-link make-text-line make-table-line make-list-line
                 make-key-line)
        (only-in :gerbil-parser/src/compiler/line-structure-rowan
                 line-structure-parser-digest line-structure-rowan-source
                 line-structure-rowan-syntax)
        (only-in ./line-structure-assertions check-structural-aot))
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
             (syntax (line-structure-rowan-syntax
                      arithmetic-language-grammar structure)))
        (check (line-structure? structure) => #t)
        (check-structural-aot syntax (identity) (block-opening "BEGIN"))))
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
             (syntax (line-structure-rowan-syntax
                      arithmetic-language-grammar recursive)))
        (check-structural-aot syntax
                              (block-contents "BlockContents::Elements"))
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
    (test-case "list strategy is a POO value projected into the Rust table"
      (let* ((list-rule (make-list-line "-+*" #t 'GroupedExpression
                                       'Expression 'Punctuation 'Punctuation))
             (narrow-tab-rule (make-list-line "-+*" #t 'GroupedExpression
                                             'Expression 'Punctuation 'Punctuation 4))
             (structure
              (make-line-structure
               (make-heading-line "*" " " 'GroupedExpression
                                  'Expression 'Punctuation)
               '()
               (make-text-line 'NameExpression 'Number)
               #f list-rule))
             (syntax (line-structure-rowan-syntax
                      arithmetic-language-grammar structure)))
        (check-structural-aot syntax (list-markers "-+*" 8))
        (check (equal? (line-structure-parser-digest
                        arithmetic-language-grammar structure)
                       (line-structure-parser-digest
                        arithmetic-language-grammar
                        (make-line-structure
                         (make-heading-line "*" " " 'GroupedExpression
                                            'Expression 'Punctuation)
                         '() (make-text-line 'NameExpression 'Number)
                         #f narrow-tab-rule)))
               => #f)
        (check (equal? (line-structure-parser-digest
                        arithmetic-language-grammar structure)
                       (line-structure-parser-digest
                        arithmetic-language-grammar (fixture-structure)))
               => #f)))
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
             (syntax (line-structure-rowan-syntax
                      arithmetic-language-grammar structure)))
        (check-structural-aot syntax (heading-fields))
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
             (syntax (line-structure-rowan-syntax
                      arithmetic-language-grammar structure)))
        (check-structural-aot syntax (body-line))
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
             (syntax (line-structure-rowan-syntax
                      arithmetic-language-grammar structure)))
        (check-structural-aot syntax (block-header))
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
             (syntax (line-structure-rowan-syntax
                      arithmetic-language-grammar structure)))
        (check-structural-aot syntax (inline-link))
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
             (syntax (line-structure-rowan-syntax
                      arithmetic-language-grammar structure)))
        (check-structural-aot syntax (paragraph-node))
        (check (equal? (line-structure-parser-digest
                        arithmetic-language-grammar structure)
                       (line-structure-parser-digest
                        arithmetic-language-grammar (fixture-structure)))
               => #f)))
    (test-case "bounded key-line strategy changes the digest and Rust table"
      (let* ((rule (make-key-line "#+" '() ":" #t #t #f #f
                                  'Expression 'Punctuation 'Number 'Punctuation))
             (structure
              (make-line-structure
               (make-heading-line "*" " " 'GroupedExpression
                                  'Expression 'Punctuation)
               '()
               (make-text-line 'NameExpression 'Number)
               #f #f (list rule)))
             (syntax (line-structure-rowan-syntax
                      arithmetic-language-grammar structure)))
        (check-structural-aot syntax
                              (key-lines '(("#+" () "KeyLineContext::Anywhere"
                                           "KeyLineMode::Single"))))
        (check (equal? (line-structure-parser-digest
                        arithmetic-language-grammar structure)
                       (line-structure-parser-digest
                        arithmetic-language-grammar (fixture-structure)))
               => #f)))
    (test-case "unbounded key-line declarations are rejected"
      (check-exception
       (make-key-line "" '() ":" #f #f #f #f
                      'Expression 'Punctuation 'Number 'Punctuation)
       true)
      (check-exception
       (make-key-line "#+" '() "x" #f #f #f #f
                      'Expression 'Punctuation 'Number 'Punctuation)
       true))
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
                      arithmetic-language-grammar structure)))
        (check-structural-aot
         syntax
         (fields '(grammar_digest parser_digest heading blocks
                   paragraph_node table list key_lines text_node text_token inline_link)))
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
