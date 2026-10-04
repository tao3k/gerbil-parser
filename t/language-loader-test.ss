;;; POO language loaders bind effective inherited slots before engine dispatch.
(import :std/test
        (for-syntax (only-in :gerbil/expander core-expand))
        (only-in :clan/poo/object .o .ref .cc object?)
        (only-in :clan/poo/mop define-type element? validate)
        (only-in :core/types PooFlowContract. poo-flow-classification-evidence)
        (only-in :gerbil-parser/src/language/entry
                 deflanguage-parser-loader LanguageLoader. LanguageLoaderContract
                 language-parser-entry-ref check-language-loader-fixtures!)
        (only-in :gerbil-parser/languages/arithmetic/v1/grammar
                 arithmetic-language-grammar)
        (only-in :gerbil-parser/languages/arithmetic/v1/fixtures arithmetic-v1-basic-fixture)
        (only-in :gerbil-parser/languages/bash/v5-3/scanner make-bash-scanner)
        (only-in :gerbil-parser/src/runtime/source-scanner source-scanner-tokens)
        (only-in :gerbil-parser/src/runtime/token token-lexeme)
        (only-in :gerbil-parser/src/language/source declare-source-language)
        (only-in :gerbil-parser/languages/bash/v5-3/grammar bash-v5-3-source-language)
        (only-in :gerbil-parser/src/runtime/artifact
                 parse-artifact-success? parse-artifact-roundtrip))

(def Dialect.
  (.o (:: @ LanguageLoader.)
      dialect: 'arithmetic
      ;; Identity and engine dispatch from parents must not take control.
      language: "incorrect-parent"
      (.parse (lambda (_) (error "parent parse must never run")))))

(def dialect-admissions 0)

(define-type (DialectContract @ PooFlowContract.)
  identity: 'loader-test/dialect
  .classify: (lambda (candidate context)
               (set! dialect-admissions (+ dialect-admissions 1))
               (let (ok? (eq? (.ref candidate 'dialect) 'arithmetic))
                 (poo-flow-classification-evidence
                  'loader-test/dialect candidate ok?
                  (if ok? '() '((invalid dialect))) context))))

(deflanguage-parser-loader (arithmetic-loader :: self Dialect.)
  (descriptor arithmetic-language-grammar)
  (parse parse-loaded-arithmetic)
  (slots dialect-name: "Arithmetic"
         (summary (string-append (.ref self 'dialect-name) " dialect")))
  (contracts DialectContract))

(def (rejects? thunk)
  (with-catch (lambda (_) #t) (lambda () (thunk) #f)))

(defsyntax (engine-slot-rejection-message stx)
  (def (message form)
    (with-catch (lambda (condition) (error-message condition))
      (lambda () (core-expand form))))
  (datum->syntax
   #'engine-slot-rejection-message
   (string-append
    (message
     #'(deflanguage-parser-loader (invalid-loader @ LanguageLoader.)
         (descriptor #f) (parse invalid-parse)
         (slots (.parse (lambda (_) #f))) (contracts)))
    " | "
    (message
     #'(deflanguage-parser-loader (invalid-keyword-loader @ LanguageLoader.)
         (descriptor #f) (parse invalid-keyword-parse)
         (slots schema: "invalid") (contracts))))))

(defsyntax (invalid-declaration-messages stx)
  (def (message form)
    (with-catch (lambda (condition) (error-message condition))
      (lambda () (core-expand form))))
  (datum->syntax
   #'invalid-declaration-messages
   (cons 'quote (list (list
    (message #'(deflanguage-parser-loader bad (grammar #f) (parse parse-bad)
                 (slots) (slots)))
    (message #'(deflanguage-parser-loader bad (grammar #f) (parse parse-bad)
                 (contracts) (slots)))
    (message #'(deflanguage-parser-loader bad (grammar #f) (parse parse-bad)
                 (options)))
    (message #'(deflanguage-parser-loader (bad marker self LanguageLoader.)
                 (descriptor #f) (parse parse-bad) (slots) (contracts)))
    (message #'(deflanguage-parser-loader bad (grammar #f) (parse "bad"))))))))

(def language-loader-test
  (test-suite "POO language loader admission"
    (test-case "inherits extensions and binds engine identity and dispatch"
      (check (object? arithmetic-loader) => #t)
      (check (element? LanguageLoaderContract arithmetic-loader) => #t)
      (check (.ref arithmetic-loader 'dialect) => 'arithmetic)
      (check (.ref arithmetic-loader 'dialect-name) => "Arithmetic")
      (check (.ref arithmetic-loader 'summary) => "Arithmetic dialect")
      (check (language-parser-entry-ref arithmetic-loader 'language) => "arithmetic")
      (check (.ref arithmetic-loader 'capabilities) => '(grammar-ir parser-ir scheme))
      (let* ((source "a + 2 * b") (artifact (parse-loaded-arithmetic source)))
        (check (parse-artifact-success? artifact) => #t)
        (check (parse-artifact-roundtrip artifact) => source)))
    (test-case "typed declarations compose with POO extension forms and optional sections"
      (deflanguage-parser-loader (typed-loader :: self Dialect.)
        (grammar arithmetic-language-grammar) (parse parse-typed)
        (slots label: "typed" (summary (.ref self 'label))))
      (deflanguage-parser-loader (contract-loader @ Dialect.)
        (grammar arithmetic-language-grammar) (parse parse-contract)
        (contracts DialectContract))
      (deflanguage-parser-loader (self-loader :: self Dialect.)
        (grammar arithmetic-language-grammar) (parse parse-self))
      (deflanguage-parser-loader (self-contract-loader :: self Dialect.)
        (grammar arithmetic-language-grammar) (parse parse-self-contract)
        (contracts DialectContract))
      (check (.ref typed-loader 'summary) => "typed")
      (for-each (lambda (parse) (check (parse-artifact-success? (parse "a+1")) => #t))
                (list parse-typed parse-contract parse-self parse-self-contract)))
    (test-case "source declarations use the same extension normalization and lossless dispatch"
      (deflanguage-parser-loader (source-loader :: self LanguageLoader.)
        (source bash-v5-3-source-language) (parse parse-source-loaded)
        (slots label: "Bash" (summary (.ref self 'label))))
      (let* ((source "echo α\n") (artifact (parse-source-loaded source)))
        (check (.ref source-loader 'capabilities) => '(scheme-source))
        (check (.ref source-loader 'summary) => "Bash")
        (check (parse-artifact-success? artifact) => #t)
        (check (parse-artifact-roundtrip artifact) => source)))
    (test-case "POO pack metadata and named services inherit without eager execution"
      (def calls 0)
      (def Pack.
        (.o (:: self Dialect.)
            metadata: (.o edition: "v1")
            fixtures: (lambda () (set! calls (+ calls 1)) (list arithmetic-v1-basic-fixture))
            tests: (list (cons 'fixtures check-language-loader-fixtures!))
            scan-workers: (list (cons 'bash-command make-bash-scanner))))
      (deflanguage-parser-loader (pack-loader :: self Pack.)
        (grammar arithmetic-language-grammar) (parse parse-pack))
      (check calls => 0)
      (check (.ref (.ref pack-loader 'metadata) 'edition) => "v1")
      (check (.ref pack-loader 'grammars) => (list arithmetic-language-grammar))
      (let (artifacts ((cdr (assq 'fixtures (.ref pack-loader 'tests))) pack-loader))
        (check (length artifacts) => 1)
        (check (parse-artifact-success? (car artifacts)) => #t))
      (check calls => 1)
      (let* ((source "echo α\n")
             (worker ((cdr (assq 'bash-command (.ref pack-loader 'scan-workers))) source)))
        (check (apply string-append (map token-lexeme (source-scanner-tokens worker 'command))) => source)))
    (test-case "metadata admission rejects invalid descriptors and service registries"
      (for-each
       (lambda (row)
         (check (element? LanguageLoaderContract (.cc arithmetic-loader (car row) (cdr row))) => #f))
       (list (cons 'grammars '()) (cons 'grammars (list #f))
             (cons 'metadata '()) (cons 'fixtures '())
             (cons 'tests (list (cons 'bad #f)))
             (cons 'tests (list (cons 'same void) (cons 'same void)))
             (cons 'scan-workers (list (cons 'bad #f)))))
      (check (rejects? (lambda ()
                        (check-language-loader-fixtures!
                         (.cc arithmetic-loader 'fixtures (lambda () '(invalid)))))) => #t))
    (test-case "parsing reuses the loaded entry without extension admission"
      (let (before dialect-admissions)
        (for-each
         (lambda (source)
           (check (parse-artifact-success? (parse-loaded-arithmetic source)) => #t))
         '("1" "a + b" "a * (b + 2)"))
        (check dialect-admissions => before)))
    (test-case "rejects a missing descriptor and inconsistent identity"
      (check (element? LanguageLoaderContract LanguageLoader.) => #f)
      (check (rejects? (lambda () (validate LanguageLoaderContract LanguageLoader.))) => #t)
      (check (element? LanguageLoaderContract (.cc arithmetic-loader 'language "bash")) => #f)
      (check (element? LanguageLoaderContract (.cc arithmetic-loader 'descriptor #f)) => #f))
    (test-case "rejects engine slot overrides during macro expansion"
      (check (engine-slot-rejection-message)
             => "language loader extension overrides an engine slot | language loader extension overrides an engine slot"))
    (test-case "normalization rejects malformed declarations before publishing a loader"
      (check (invalid-declaration-messages)
             => '("duplicate or out-of-order language loader slots"
                  "duplicate or out-of-order language loader slots"
                  "unknown language loader section"
                  "invalid language loader binding"
                  "language loader parse binding must be an identifier")))
    (test-case "grammar and source declarations enforce their descriptor kind"
      (let (source-descriptor
            (declare-source-language "source-test" "v1" "source-test.v1"
                                     (lambda _ #f) (lambda _ #f)))
        (check
         (rejects? (lambda ()
                     (deflanguage-parser-loader wrong-grammar
                       (grammar source-descriptor) (parse parse-wrong-grammar))
                     wrong-grammar)) => #t)
        (check
         (rejects? (lambda ()
                     (deflanguage-parser-loader wrong-source
                       (source arithmetic-language-grammar) (parse parse-wrong-source))
                     wrong-source)) => #t)))
    (test-case "extension contracts validate the effective slot object"
      (check
       (rejects?
        (lambda ()
          (deflanguage-parser-loader (invalid-dialect @ Dialect.)
            (descriptor arithmetic-language-grammar)
            (parse parse-invalid-dialect)
            (slots (dialect 'other))
            (contracts DialectContract))
          invalid-dialect)) => #t)
      (check (element? DialectContract arithmetic-loader) => #t)
      (check (rejects? (lambda () (validate DialectContract (.cc arithmetic-loader 'dialect 'other)))) => #t))))

(export language-loader-test)
