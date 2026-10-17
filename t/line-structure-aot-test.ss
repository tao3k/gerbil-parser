(import :gerbil-parser/language-test-support)
;;; -*- Gerbil -*-
;;; POO declaration owns contextual-line AOT admission.

(import (only-in :std/test check check-exception test-case test-suite)
        (only-in :gerbil-parser/languages/arithmetic/parser
                 arithmetic-language-grammar)
        (only-in :gerbil-parser/src/modules/parser/line-structure-objects
                 line-structure? make-line-structure make-heading-line
                 make-heading-fields
                 make-block-line make-block-header make-key-value-line
                 make-inline-link make-text-line make-table-line make-list-line
                 make-key-line)
        (only-in :gerbil-parser/src/compiler/line-structure-runtime
                 line-structure-parser-digest line-structure-runtime-source
                 line-structure-runtime-syntax)
        (only-in ./line-structure-assertions check-structural-aot))


(def (fixture-structure (section 'GroupedExpression))
  (make-line-structure
   (make-heading-line "*" " " section 'Expression 'Punctuation)
   (list (make-block-line
          "BEGIN" "END" #f #t
          'PrefixExpression 'Punctuation 'Number 'Punctuation
          'close-at-eof #f #f))
   (make-text-line 'NameExpression 'Number)))

(deflanguage-parser-tests line-structure-aot-test "POO structural-line AOT"
  (engine)
    (scenario "one checked POO declaration resolves canonical parser kinds"
      (sequential ((structure (fixture-structure))
             (syntax (line-structure-runtime-syntax
                      arithmetic-language-grammar structure)))
        (equal (line-structure? structure)  #t)
        (verify (check-structural-aot syntax (identity) (block-opening "BEGIN")))))
    (property "parser identity changes when the POO strategy changes"
      (bindings (original (fixture-structure)) (changed
              (make-line-structure
               (make-heading-line "*" " " 'GroupedExpression
                                  'Expression 'Punctuation)
               (list (make-block-line
                      "BEGIN" "END" #f #t
                      'PrefixExpression 'Punctuation 'Number 'Punctuation
                      'recover-as-text #f #f))
               (make-text-line 'NameExpression 'Number))))
      (equal (line-structure-parser-digest
                arithmetic-language-grammar original)
                (line-structure-parser-digest
                   arithmetic-language-grammar original))
      (equal (equal? (line-structure-parser-digest
                        arithmetic-language-grammar original)
                       (line-structure-parser-digest
                        arithmetic-language-grammar changed))
                #f))
    (property "parser identity includes the heading boundary policy"
      (bindings (original (fixture-structure)) (bounded
              (make-line-structure
               (make-heading-line "*" " " 'GroupedExpression
                                  'Expression 'Punctuation)
               (list (make-block-line
                      "BEGIN" "END" #f #t
                      'PrefixExpression 'Punctuation 'Number 'Punctuation
                      'close-at-eof #t #f))
               (make-text-line 'NameExpression 'Number))))
      (equal (equal? (line-structure-parser-digest
                        arithmetic-language-grammar original)
                       (line-structure-parser-digest
                        arithmetic-language-grammar bounded))
                #f))
    (scenario "recursive block contents is Scheme-owned and digest-bound"
      (sequential ((recursive
              (make-line-structure
               (make-heading-line "*" " " 'GroupedExpression
                                  'Expression 'Punctuation)
               (list (make-block-line
                      "BEGIN" "END" #f #t
                      'PrefixExpression 'Punctuation 'Number 'Punctuation
                      'close-at-eof #f #f #f 'elements))
               (make-text-line 'NameExpression 'Number)))
             (syntax (line-structure-runtime-syntax
                      arithmetic-language-grammar recursive)))
        (verify (check-structural-aot syntax
                              (block-contents "BlockContents::Elements")))
        (equal (equal? (line-structure-parser-digest
                        arithmetic-language-grammar recursive)
                       (line-structure-parser-digest
                        arithmetic-language-grammar (fixture-structure)))
                #f)))
    (scenario "recursive block contents rejects key-value body semantics"
      (raises
       (make-block-line
        "BEGIN" "END" #f #t
        'PrefixExpression 'Punctuation 'Number 'Punctuation
        'recover-as-text #f
        (make-key-value-line ":" 'NameExpression
                             'Number 'Number 'Punctuation)
        #f 'elements)
       true))
    (scenario "list strategy is a POO value projected into the Rust table"
      (sequential ((list-rule (make-list-line "-+*" #t 'GroupedExpression
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
             (syntax (line-structure-runtime-syntax
                      arithmetic-language-grammar structure)))
        (verify (check-structural-aot syntax (list-markers "-+*" 8)))
        (equal (equal? (line-structure-parser-digest
                        arithmetic-language-grammar structure)
                       (line-structure-parser-digest
                        arithmetic-language-grammar
                        (make-line-structure
                         (make-heading-line "*" " " 'GroupedExpression
                                            'Expression 'Punctuation)
                         '() (make-text-line 'NameExpression 'Number)
                         #f narrow-tab-rule)))
                #f)
        (equal (equal? (line-structure-parser-digest
                        arithmetic-language-grammar structure)
                       (line-structure-parser-digest
                        arithmetic-language-grammar (fixture-structure)))
                #f)))
    (scenario "typed heading fields are projected and bound to identity"
      (sequential ((structure
              (make-line-structure
               (make-heading-line "*" " " 'GroupedExpression
                                  'Expression 'Punctuation
                                  (make-heading-fields 'Number 'Punctuation))
               (list (make-block-line
                      "BEGIN" "END" #f #t
                      'PrefixExpression 'Punctuation 'Number 'Punctuation
                      'close-at-eof #f #f))
               (make-text-line 'NameExpression 'Number)))
             (syntax (line-structure-runtime-syntax
                      arithmetic-language-grammar structure)))
        (verify (check-structural-aot syntax (heading-fields)))
        (equal (equal? (line-structure-parser-digest
                        arithmetic-language-grammar structure)
                       (line-structure-parser-digest
                        arithmetic-language-grammar (fixture-structure)))
                #f)))
    (scenario "typed key-value body rules are projected and bound to identity"
      (sequential ((body (make-key-value-line ":" 'NameExpression
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
             (syntax (line-structure-runtime-syntax
                      arithmetic-language-grammar structure)))
        (verify (check-structural-aot syntax (body-line)))
        (equal (equal? (line-structure-parser-digest
                        arithmetic-language-grammar structure)
                       (line-structure-parser-digest
                        arithmetic-language-grammar (fixture-structure)))
                #f)))
    (scenario "typed block header changes parser identity and emits token kinds"
      (sequential ((header (make-block-header 'Number 'Punctuation))
             (structure
              (make-line-structure
               (make-heading-line "*" " " 'GroupedExpression
                                  'Expression 'Punctuation)
               (list (make-block-line
                      "BEGIN" "END" #f #t
                      'PrefixExpression 'Punctuation 'Number 'Punctuation
                      'close-at-eof #f #f header))
               (make-text-line 'NameExpression 'Number)))
             (syntax (line-structure-runtime-syntax
                      arithmetic-language-grammar structure)))
        (verify (check-structural-aot syntax (block-header)))
        (equal (equal? (line-structure-parser-digest
                        arithmetic-language-grammar structure)
                       (line-structure-parser-digest
                        arithmetic-language-grammar (fixture-structure)))
                #f)))
    (scenario "typed inline link is bound to parser identity"
      (sequential ((link (make-inline-link "[[" "][" "]]"
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
             (syntax (line-structure-runtime-syntax
                      arithmetic-language-grammar structure)))
        (verify (check-structural-aot syntax (inline-link)))
        (equal (equal? (line-structure-parser-digest
                        arithmetic-language-grammar structure)
                       (line-structure-parser-digest
                        arithmetic-language-grammar (fixture-structure)))
                #f)))
    (scenario "paragraph grouping is a POO-declared node and parser identity"
      (sequential ((structure
              (make-line-structure
               (make-heading-line "*" " " 'GroupedExpression
                                  'Expression 'Punctuation)
               (list (make-block-line
                      "BEGIN" "END" #f #t
                      'PrefixExpression 'Punctuation 'Number 'Punctuation
                      'close-at-eof #f #f))
               (make-text-line 'NameExpression 'Number #f 'NameExpression)))
             (syntax (line-structure-runtime-syntax
                      arithmetic-language-grammar structure)))
        (verify (check-structural-aot syntax (paragraph-node)))
        (equal (equal? (line-structure-parser-digest
                        arithmetic-language-grammar structure)
                       (line-structure-parser-digest
                        arithmetic-language-grammar (fixture-structure)))
                #f)))
    (scenario "bounded key-line strategy changes the digest and Rust table"
      (sequential ((rule (make-key-line "#+" '() ":" #t #t #f #f
                                  'Expression 'Punctuation 'Number 'Punctuation))
             (structure
              (make-line-structure
               (make-heading-line "*" " " 'GroupedExpression
                                  'Expression 'Punctuation)
               '()
               (make-text-line 'NameExpression 'Number)
               #f #f (list rule)))
             (syntax (line-structure-runtime-syntax
                      arithmetic-language-grammar structure)))
        (verify (check-structural-aot syntax
                              (key-lines '(("#+" () "KeyLineContext::Anywhere"
                                           "KeyLineMode::Single")))))
        (equal (equal? (line-structure-parser-digest
                        arithmetic-language-grammar structure)
                       (line-structure-parser-digest
                        arithmetic-language-grammar (fixture-structure)))
                #f)))
    (scenario "unbounded key-line declarations are rejected"
      (raises
       (make-key-line "" '() ":" #f #f #f #f
                      'Expression 'Punctuation 'Number 'Punctuation)
       true)
      (raises
       (make-key-line "#+" '() "x" #f #f #f #f
                      'Expression 'Punctuation 'Number 'Punctuation)
       true))
    (scenario "table rules project through the typed Rust syntax macro"
      (sequential ((table (make-table-line "|" 'GroupedExpression
                                     'NameExpression 'Expression 'NameExpression
                                     'Punctuation 'Number 'Punctuation 'Punctuation))
             (structure
              (make-line-structure
               (make-heading-line "*" " " 'GroupedExpression
                                  'Expression 'Punctuation)
               '()
               (make-text-line 'NameExpression 'Number)
               table))
             (syntax (line-structure-runtime-syntax
                      arithmetic-language-grammar structure)))
        (verify (check-structural-aot
         syntax
         (fields '(grammar_digest parser_digest heading blocks
                   paragraph_node table list key_lines text_node text_token inline_link))))
        (equal (equal? (line-structure-parser-digest
                        arithmetic-language-grammar structure)
                       (line-structure-parser-digest
                        arithmetic-language-grammar (fixture-structure)))
                #f)))
    (property "wrong-category kind is rejected at the AOT boundary"
      (bindings)
      (equal (with-catch
              (lambda (error) #t)
              (lambda ()
                (line-structure-runtime-source
                 arithmetic-language-grammar
                 (fixture-structure 'Number))
                #f))
              #t))
    (property "untyped block values cannot enter the POO contract"
      (bindings)
      (equal (with-catch
              (lambda (error) #t)
              (lambda ()
                (make-line-structure
                 (make-heading-line "*" " "
                                    'GroupedExpression 'Expression 'Punctuation)
                 (list 'not-a-block)
                 (make-text-line 'NameExpression 'Number))
                #f))
              #t))
    (property "untyped key-value body rules cannot enter the POO contract"
      (bindings)
      (equal (with-catch
              (lambda (error) #t)
              (lambda ()
                (make-block-line "BEGIN" "END" #f #t
                                 'PrefixExpression 'Punctuation
                                 'Number 'Punctuation
                                 'recover-as-text #t 'not-a-body-rule)
                #f))
              #t))
    (property "an undeclared EOF recovery mode is rejected"
      (bindings)
      (equal (with-catch
              (lambda (error) #t)
              (lambda ()
                (make-block-line "BEGIN" "END" #f #t
                                 'PrefixExpression 'Punctuation
                                 'Number 'Punctuation 'guess #f #f)
                #f))
              #t)))
