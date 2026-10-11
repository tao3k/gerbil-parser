;;; -*- Gerbil -*-
;;; The generic contextual scanner supplies an existing LR ParserMachine.

(import (only-in :std/test check test-case test-suite)
        (only-in :gerbil-parser/languages/hcl/grammar
                 hcl-parser)
        (only-in :gerbil-parser/src/compiler/machine
                 parser-machine-grammar-digest parser-machine-contextual-ir parser-machine-ir)
        (only-in :gerbil-parser/t/benchmarks/contextual-scanner/parser-fixture
                 contextual-product)
        (only-in :gerbil-parser/src/runtime/parser
                 parse-source/contextual prepare-contextual-parser
                 parse-source/contextual/prepared)
        (only-in :gerbil-parser/src/runtime/identity sha256-text)
        (only-in :gerbil-parser/src/runtime/contextual-ir contextual-parser-ir-valid? contextual-ir-ref)
        (only-in :gerbil-parser/src/runtime/artifact
                 parse-artifact-success? parse-artifact-events
                 parse-artifact-status parse-artifact-ref
                 parse-artifact-valid? parse-artifact-roundtrip
                 token-event? token-event-token-kind token-event-lexeme))
(export contextual-parser-test)

(def contextual-parser-test
  (test-suite "contextual scanner LR integration"
    (test-case "ordinary LR machine owns the common admitted instruction product"
      (let (ir (parser-machine-contextual-ir hcl-parser))
        (check (contextual-parser-ir-valid? ir) => #t)
        (check (contextual-ir-ref (contextual-ir-ref ir 'recognition) 'program)
               => (parser-machine-ir hcl-parser))
        (set-cdr! (assq 'root-kind ir) 'Foreign)
        (check (contextual-parser-ir-valid? (parser-machine-contextual-ir hcl-parser)) => #t)))
    (test-case "raw and prepared parsers own product and published artifact identities"
      (let* ((machine hcl-parser)
             (literal (string-copy "1"))
             (product (contextual-product machine (parser-machine-grammar-digest machine)
                                          '(line) '((line () ())) literal))
             (plan (prepare-contextual-parser machine product))
             (sources '("x = 1\n" "名字 = 1\n" "x = " "x = @你好\n"))
             (expected
              (map (lambda (source)
                     (parse-source/contextual machine product source)) sources)))
        (string-set! literal 0 #\2)
        (set-cdr! (car (cdr (assq 'state-positions (cdr (assq 'context product))))) 'foreign)
        (string-set! (cdr (assq 'digest product)) 0 #\x)
        (let (artifact (parse-source/contextual/prepared plan (car sources)))
          (string-set! (parse-artifact-ref artifact 'grammarDigest) 0 #\x))
        (for-each
         (lambda (source reference)
           (let (artifact (parse-source/contextual/prepared plan source))
             (check artifact => reference)
             (check (parse-artifact-valid? artifact) => #t)
             (check (parse-artifact-roundtrip artifact) => source)))
         sources expected)
        (check (with-catch (lambda (condition) (error-message condition))
                 (lambda () (parse-source/contextual machine product "x = 1\n") #f))
               => "contextual parser product does not match parser machine")))
    (test-case "prepared parser rejects mismatched products and raw plan inputs"
      (check (with-catch (lambda (condition) (error-message condition))
               (lambda () (prepare-contextual-parser hcl-parser
                              (contextual-product hcl-parser "other")) #f))
             => "contextual parser product does not match parser machine")
      (check (with-catch (lambda (condition) (error-message condition))
               (lambda () (parse-source/contextual/prepared '() "") #f))
             => "contextual parser requires prepared plan and source"))
    (test-case "self-consistent non-symbol positions fail both runtime admissions"
      (def (resign datum)
        (let (body (filter (lambda (row) (not (eq? (car row) 'digest))) datum))
          (append body (list (cons 'digest (sha256-text
                           (call-with-output-string (lambda (port) (write body port)))))))))
      (let* ((machine hcl-parser)
             (product (contextual-product machine (parser-machine-grammar-digest machine)))
             (position (string-copy "line"))
             (scanner (resign
                       (map (lambda (row) (if (eq? (car row) 'positions)
                                            (cons 'positions (list position)) row))
                            (cdr (assq 'scanner product)))))
             (altered (resign
                       (map (lambda (row)
                              (case (car row)
                                ((scanner) (cons 'scanner scanner))
                                ((context)
                                 (cons 'context
                                   (list (cons 'state-positions
                                     (map (lambda (entry) (cons (car entry) position))
                                          (cdr (assq 'state-positions (cdr row))))))))
                                (else row))) product))))
        (for-each
         (lambda (admit)
           (check (with-catch (lambda (condition) (error-message condition))
                    (lambda () (admit) #f))
                  => "invalid contextual LR position table"))
         (list (lambda () (prepare-contextual-parser machine altered))
               (lambda () (parse-source/contextual machine altered "x = 1\n"))))))
    (test-case "HCL grammar accepts tokens from the generic scanner"
      (let* ((source "x = 1\n")
             (machine hcl-parser)
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
      (let* ((machine hcl-parser)
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
      (let* ((machine hcl-parser)
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
           hcl-parser
           (contextual-product hcl-parser "other") "x = 1\n")
          #f))
       => "contextual parser product does not match parser machine"))
    (test-case "scanner change alters the published grammar identity"
      (let* ((machine hcl-parser)
             (base (parser-machine-grammar-digest machine))
             (first (contextual-product machine base))
             (second (contextual-product
                      machine base '(line) '((line () ())) "2")))
        (check (equal? (cdr (assq 'digest first))
                       (cdr (assq 'digest second))) => #f)))
    (test-case "token requirements select a specific LR position"
      (let* ((machine hcl-parser)
             (product
              (contextual-product
               machine (parser-machine-grammar-digest machine)
               '(line name) '((line () ()) (name ((token identifier)) ()))))
             (positions (cdr (assq 'state-positions (cdr (assq 'context product))))))
        (check (cdr (assq 0 positions)) => 'name)
        (check (any (lambda (row) (eq? (cdr row) 'line)) positions) => #t)
        (check (parse-artifact-success?
                (parse-source/contextual machine product "x = 1\n"))
               => #t)))
    (test-case "literal requirements select grammar-owned LR positions"
      (let* ((machine hcl-parser)
             (product
              (contextual-product
               machine (parser-machine-grammar-digest machine)
               '(line assignment)
               '((line () ()) (assignment ((literal "=")) ()))))
             (positions (cdr (assq 'state-positions (cdr (assq 'context product))))))
        (check (any (lambda (row) (eq? (cdr row) 'assignment)) positions) => #t)
        (check (parse-artifact-success?
                (parse-source/contextual machine product "x = 1\n"))
               => #t)))
    (test-case "EOF requirements select an empty-input parser position"
      (let* ((machine hcl-parser)
             (product
              (contextual-product
               machine (parser-machine-grammar-digest machine)
               '(line finish) '((line () ()) (finish ((eof)) ()))))
             (positions (cdr (assq 'state-positions (cdr (assq 'context product))))))
        (check (any (lambda (row) (eq? (cdr row) 'finish)) positions) => #t)
        (check (parse-artifact-success?
                (parse-source/contextual machine product "")) => #t)))
    (test-case "duplicate LR state positions fail runtime preparation"
      (let* ((machine hcl-parser)
             (product (contextual-product
                       machine (parser-machine-grammar-digest machine)))
             (rows (cdr (assq 'state-positions (cdr (assq 'context product)))))
             (body
              (map (lambda (row)
                     (if (eq? (car row) 'context)
                       (cons 'context (list (cons 'state-positions
                             (cons (car rows) (cons (car rows) (cddr rows))))))
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
         => "invalid contextual LR position table")
        (check
         (with-catch
          (lambda (condition) (error-message condition))
          (lambda () (prepare-contextual-parser machine altered) #f))
         => "invalid contextual LR position table")))
    (test-case "unknown lookaheads fail declaration admission"
      (let ((machine hcl-parser))
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
      (let* ((machine hcl-parser)
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
