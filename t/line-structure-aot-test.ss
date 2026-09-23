;;; -*- Gerbil -*-
;;; POO declaration owns contextual-line AOT admission.

(import (only-in :std/test check test-case test-suite)
        (only-in :gerbil-parser/languages/arithmetic/v1/grammar
                 arithmetic-language-grammar)
        (only-in :gerbil-parser/src/modules/parser/line-structure-objects
                 line-structure? make-line-structure make-heading-line
                 make-block-line make-text-line)
        (only-in :gerbil-parser/src/compiler/line-structure-rowan
                 line-structure-parser-digest line-structure-rowan-source))
(export line-structure-aot-test)

(def (fixture-structure (section 'GroupedExpression))
  (make-line-structure
   (make-heading-line "*" " " section 'Expression 'Punctuation)
   (list (make-block-line
          "BEGIN" "END" #f #t
          'PrefixExpression 'Punctuation 'Number 'Punctuation
          'close-at-eof))
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
                      'recover-as-text))
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
    (test-case "an undeclared EOF recovery mode is rejected"
      (check (with-catch
              (lambda (error) #t)
              (lambda ()
                (make-block-line "BEGIN" "END" #f #t
                                 'PrefixExpression 'Punctuation
                                 'Number 'Punctuation 'guess)
                #f))
             => #t))))
