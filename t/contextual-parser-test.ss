;;; -*- Gerbil -*-
;;; The generic contextual scanner supplies an existing LR ParserMachine.

(import (only-in :std/test check test-case test-suite)
        (only-in :gerbil-parser/languages/hcl/v2-24/grammar
                 hcl-v2-24-parser)
        (only-in :gerbil-parser/src/compiler/machine
                 parser-machine-grammar-digest parser-machine-ir)
        (only-in :gerbil-parser/src/modules/parser/contextual-objects
                 make-contextual-method make-contextual-role
                 make-contextual-scan-rule)
        (only-in :gerbil-parser/src/compiler/contextual-parser-ir
                 compile-contextual-parser)
        (only-in :gerbil-parser/src/runtime/parser
                 parse-source/contextual)
        (only-in :gerbil-parser/src/runtime/identity sha256-text)
        (only-in :gerbil-parser/src/runtime/artifact
                 parse-artifact-success? parse-artifact-events
                 parse-artifact-status parse-artifact-ref
                 parse-artifact-valid? parse-artifact-roundtrip
                 token-event? token-event-token-kind token-event-lexeme))
(export contextual-parser-test)

(def (method name form result)
  (make-contextual-method name 'any 'any form result))

(def (rule name form matcher rank)
  (make-contextual-scan-rule name 'normal form matcher rank 'keep))

(def (contextual-product machine grammar-digest
                         (positions '(line))
                         (clauses '((line () ())))
                         (number-literal "1"))
  (let (role (make-contextual-role
              'hcl-parser-fixture
              (list (method 'identifier 'identifier 'identifier)
                    (method 'number 'number 'number)
                    (method 'punctuation 'punctuation 'punctuation)
                    (method 'space 'space 'horizontal-whitespace)
                    (method 'newline 'newline 'newline))))
    (compile-contextual-parser
     (parser-machine-ir machine) grammar-digest (list role)
     '(normal) positions
     '(identifier number punctuation space newline)
     (list (rule 'identifier 'identifier '(identifier) 0)
           (rule 'number 'number (list 'literal number-literal) 0)
           (rule 'punctuation 'punctuation '(literal "=") 0)
           (rule 'space 'space '(horizontal-whitespace+) 0)
           (rule 'newline 'newline '(newline-one) 0))
     'normal clauses)))

(def contextual-parser-test
  (test-suite "contextual scanner LR integration"
    (test-case "HCL grammar accepts tokens from the generic scanner"
      (let* ((source "x = 1\n")
             (machine hcl-v2-24-parser)
             (product
              (contextual-product machine
                                  (parser-machine-grammar-digest machine)))
             (artifact
              (parse-source/contextual machine product source))
             (tokens (filter token-event? (parse-artifact-events artifact))))
        (check (parse-artifact-success? artifact) => #t)
        (check (parse-artifact-ref artifact 'grammarDigest)
               => (cdr (assq 'digest product)))
        (check (map token-event-token-kind tokens)
               => '(identifier horizontal-whitespace punctuation
                     horizontal-whitespace number newline))
        (check (apply string-append
                      (map token-event-lexeme tokens))
               => source)))
    (test-case "LR rejection remains a rejected ParseArtifact"
      (let* ((machine hcl-v2-24-parser)
             (artifact
              (parse-source/contextual
               machine
               (contextual-product
                machine (parser-machine-grammar-digest machine))
               "x = ")))
        (check (parse-artifact-status artifact) => 'rejected)
        (check (parse-artifact-valid? artifact) => #t)
        (check (parse-artifact-roundtrip artifact) => "x = ")))
    (test-case "rejected lookahead and unscanned suffix retain UTF8 coverage"
      (let* ((machine hcl-v2-24-parser)
             (product (contextual-product
                       machine (parser-machine-grammar-digest machine))))
        (for-each
         (lambda (source)
           (let (artifact (parse-source/contextual machine product source))
             (check (parse-artifact-status artifact) => 'rejected)
             (check (parse-artifact-valid? artifact) => #t)
             (check (parse-artifact-roundtrip artifact) => source)))
         '("x x = 1\n" "x = @你好\n"))))
    (test-case "different grammar identity is rejected before scanning"
      (check
       (with-catch
        (lambda (condition) (error-message condition))
        (lambda ()
          (parse-source/contextual
           hcl-v2-24-parser
           (contextual-product hcl-v2-24-parser "other") "x = 1\n")
          #f))
       => "contextual parser product does not match parser machine"))
    (test-case "scanner change alters the published grammar identity"
      (let* ((machine hcl-v2-24-parser)
             (base (parser-machine-grammar-digest machine))
             (first (contextual-product machine base))
             (second (contextual-product
                      machine base '(line) '((line () ())) "2")))
        (check (equal? (cdr (assq 'digest first))
                       (cdr (assq 'digest second))) => #f)))
    (test-case "token requirements select a specific LR position"
      (let* ((machine hcl-v2-24-parser)
             (product
              (contextual-product
               machine (parser-machine-grammar-digest machine)
               '(line name) '((line () ()) (name ((token identifier)) ()))))
             (positions (cdr (assq 'state-positions product))))
        (check (cdr (assq 0 positions)) => 'name)
        (check (any (lambda (row) (eq? (cdr row) 'line)) positions) => #t)
        (check (parse-artifact-success?
                (parse-source/contextual machine product "x = 1\n"))
               => #t)))
    (test-case "literal requirements select grammar-owned LR positions"
      (let* ((machine hcl-v2-24-parser)
             (product
              (contextual-product
               machine (parser-machine-grammar-digest machine)
               '(line assignment)
               '((line () ()) (assignment ((literal "=")) ()))))
             (positions (cdr (assq 'state-positions product))))
        (check (any (lambda (row) (eq? (cdr row) 'assignment)) positions) => #t)
        (check (parse-artifact-success?
                (parse-source/contextual machine product "x = 1\n"))
               => #t)))
    (test-case "EOF requirements select an empty-input parser position"
      (let* ((machine hcl-v2-24-parser)
             (product
              (contextual-product
               machine (parser-machine-grammar-digest machine)
               '(line finish) '((line () ()) (finish ((eof)) ()))))
             (positions (cdr (assq 'state-positions product))))
        (check (any (lambda (row) (eq? (cdr row) 'finish)) positions) => #t)
        (check (parse-artifact-success?
                (parse-source/contextual machine product "")) => #t)))
    (test-case "duplicate LR state positions fail runtime preparation"
      (let* ((machine hcl-v2-24-parser)
             (product (contextual-product
                       machine (parser-machine-grammar-digest machine)))
             (rows (cdr (assq 'state-positions product)))
             (body
              (map (lambda (row)
                     (if (eq? (car row) 'state-positions)
                       (cons 'state-positions
                             (cons (car rows) (cons (car rows) (cddr rows))))
                       row))
                   (filter (lambda (row) (not (eq? (car row) 'digest))) product)))
             (altered
              (append body
                      (list (cons 'digest
                                  (sha256-text
                                   (call-with-output-string
                                    (lambda (port) (write body port)))))))))
        (check
         (with-catch
          (lambda (condition) (error-message condition))
          (lambda () (parse-source/contextual machine altered "x = 1\n") #f))
         => "invalid contextual LR position table")))
    (test-case "unknown lookaheads fail declaration admission"
      (let ((machine hcl-v2-24-parser))
        (check
         (with-catch
          (lambda (condition) (error-message condition))
          (lambda ()
            (contextual-product
             machine (parser-machine-grammar-digest machine)
             '(line unknown)
             '((line () ()) (unknown ((literal "@@")) ())))
            #f))
         => "invalid contextual LR position declaration")))
    (test-case "incomparable LR position declarations are rejected"
      (let* ((machine hcl-v2-24-parser)
             (base (parser-machine-grammar-digest machine)))
        (check
         (with-catch
          (lambda (condition) (error-message condition))
          (lambda ()
            (contextual-product
             machine base '(line other)
             '((line () ()) (other () ())))
            #f))
         => "unresolved contextual LR position")))))
