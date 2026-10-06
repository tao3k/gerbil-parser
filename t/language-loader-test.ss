;;; POO language loaders bind effective inherited slots before engine dispatch.
(import :std/test
        (for-syntax (only-in :gerbil/expander core-expand))
        (only-in :clan/poo/object .o .ref .cc object?)
        (only-in :clan/poo/mop define-type element? validate)
        (only-in :core/types PooFlowContract. poo-flow-classification-evidence)
        (only-in :gerbil-parser/src/language/entry
                 deflanguage-parser-loader LanguageLoader. LanguageLoaderContract
                 language-parser-entry-ref check-language-loader-fixtures!
                 declare-language-source-scan-worker make-language-scan-worker
                 declare-language-fixture-test run-language-test)
        (only-in :gerbil-parser/languages/arithmetic/grammar
                 arithmetic-language-grammar)
        (only-in :gerbil-parser/language-support/fixture
                 defsyntax-fixture defsyntax-corpus syntax-fixture-source syntax-fixture-source-digest)
        (only-in :gerbil-parser/src/runtime/identity sha256-text)
        (only-in :gerbil-parser/languages/arithmetic/parser arithmetic-basic-fixture)
        (only-in :gerbil-parser/src/language/source source-language-scanner-factory)
        (only-in "fixtures/source-strategies.ss" test-source-strategy)
        (only-in :gerbil-parser/src/runtime/source-scanner source-scanner-tokens)
        (only-in :gerbil-parser/src/runtime/token token-lexeme)
        (only-in :gerbil-parser/language-source-support declare-source-language LineSourceStrategy.)
        (only-in :gerbil-parser/languages/bash/parser bash-source-language)
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

(defsyntax (inline-fixture-rejections stx)
  (def (rejects form)
    (with-catch (lambda (_) #t) (lambda () (core-expand form) #f)))
  (datum->syntax
   #'inline-fixture-rejections
   (list 'quote
         (list
          (rejects #'(defsyntax-fixture invalid-fixture
                       (identity "bad" "lang" "v1" "contract")
                       (text 17) (expect accepted Root ())))
          (rejects #'(defsyntax-fixture invalid-fixture
                       (identity "bad" "lang" "v1" "contract")
                       (text "x") (expect unknown Root ())))
          (rejects #'(defsyntax-corpus invalid-corpus
                       (identity "lang" "v1" "contract")
                       (accepted ("bad" invalid-fixture (text 17) Root ()))
                       (rejected)))))))

(defsyntax-corpus inline-arithmetic-fixtures
  (identity "arithmetic" "v1" "arithmetic-expression.v1")
  (accepted ("loader/inline/unicode" inline-unicode (text "α + 2") SourceFile (Expression)))
  (rejected ("loader/inline/incomplete" inline-incomplete (text "1 +") )))

(defsyntax-fixture foreign-language
  (identity "foreign/language" "foreign" "v1" "arithmetic-expression.v1")
  (text "1") (expect accepted SourceFile ()))
(defsyntax-fixture foreign-version
  (identity "foreign/version" "arithmetic" "v2" "arithmetic-expression.v1")
  (text "1") (expect accepted SourceFile ()))
(defsyntax-fixture foreign-contract
  (identity "foreign/contract" "arithmetic" "v1" "foreign.v1")
  (text "1") (expect accepted SourceFile ()))

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
        (source bash-source-language) (parse parse-source-loaded)
        (slots label: "Bash" (summary (.ref self 'label))))
      (let* ((source "echo α\n") (artifact (parse-source-loaded source)))
        (check (.ref source-loader 'capabilities) => '(scheme-source))
        (check (.ref source-loader 'summary) => "Bash")
        (check (parse-artifact-success? artifact) => #t)
        (check (parse-artifact-roundtrip artifact) => source)))
    (test-case "POO fixture values inherit and override without parser execution"
      (def Pack.
        (.o (:: self Dialect.)
            metadata: (.o edition: "v1" version: "wrong" digest: "wrong")
            fixtures: (list arithmetic-basic-fixture)
            scan-workers: '()))
      (deflanguage-parser-loader (pack-loader :: self Pack.)
        (grammar arithmetic-language-grammar) (parse parse-pack))
      (check (.ref pack-loader 'fixtures) => (list arithmetic-basic-fixture))
      (check (rejects? (lambda () (run-language-test pack-loader 'missing))) => #t)
      (check (.ref (.ref pack-loader 'metadata) 'edition) => "v1")
      (check (.ref (.ref pack-loader 'metadata) 'version) => (.ref pack-loader 'version))
      (check (equal? (.ref (.ref pack-loader 'metadata) 'digest) "wrong") => #f)
      (check (.ref pack-loader 'grammars) => (list arithmetic-language-grammar))
      (let (artifacts (run-language-test pack-loader 'fixtures))
        (check (length artifacts) => 1)
        (check (parse-artifact-success? (car artifacts)) => #t))
      (let (overridden (.cc pack-loader 'fixtures inline-arithmetic-fixtures))
        (check (map parse-artifact-success? (run-language-test overridden 'fixtures)) => '(#t #f)))
      (let (extended (.cc pack-loader 'fixtures
                         (append (.ref pack-loader 'fixtures) (list inline-incomplete))))
        (check (map parse-artifact-success? (run-language-test extended 'fixtures)) => '(#t #f)))
      (deflanguage-parser-loader (worker-loader :: self LanguageLoader.)
        (source bash-source-language) (parse parse-worker-source)
        (slots scan-workers: (list (cons 'bash-command
                                  (declare-language-source-scan-worker
                                   bash-source-language)))))
      (let* ((source "echo α\n")
             (worker (make-language-scan-worker worker-loader 'bash-command source)))
        (check (apply string-append (map token-lexeme (source-scanner-tokens worker 'command))) => source)))
    (test-case "typed fixture services extend POO slots while engine checks outcomes"
      (check (rejects? (lambda () (declare-language-fixture-test #f))) => #t)
      (deflanguage-parser-loader (extended-tests :: self LanguageLoader.)
        (grammar arithmetic-language-grammar) (parse parse-extended-tests)
        (slots fixtures: (list arithmetic-basic-fixture)
               tests: (list (cons 'conformance (declare-language-fixture-test arithmetic-language-grammar)))))
      (check (length (run-language-test extended-tests 'conformance)) => 1)
      (let (fake (.cc extended-tests '.parse (lambda (_) #t)))
        (check (rejects? (lambda () (run-language-test fake 'conformance))) => #t)))
    (test-case "declared source workers reject foreign products and undeclared names"
      (check (rejects? (lambda ()
                        (declare-language-source-scan-worker arithmetic-language-grammar))) => #t)
      (for-each
       (lambda (factory)
         (def bad-source (declare-source-language "bad-worker" "v1" "bad-worker.test"
                          (test-source-strategy factory (lambda (_) '()) (lambda _ #f))))
         (deflanguage-parser-loader (bad-worker-loader :: self LanguageLoader.)
           (source bad-source) (parse parse-bad-worker)
           (slots scan-workers: (list (cons 'command
                                     (declare-language-source-scan-worker bad-source)))))
         (check (rejects? (lambda () (make-language-scan-worker bad-worker-loader 'command "echo α\n"))) => #t)
         (check (rejects? (lambda () (make-language-scan-worker bad-worker-loader 'missing "echo α\n"))) => #t))
       (list (lambda (_) #f) (lambda (_) ((source-language-scanner-factory bash-source-language) "different source")))))
    (test-case "metadata admission rejects invalid descriptors and service registries"
      (for-each
       (lambda (row)
         (check (element? LanguageLoaderContract (.cc arithmetic-loader (car row) (cdr row))) => #f))
       (list (cons 'grammars '()) (cons 'grammars (list #f))
             (cons 'metadata '())
             (cons 'metadata (.cc (.ref arithmetic-loader 'metadata) 'version "wrong"))
             (cons 'metadata (.cc (.ref arithmetic-loader 'metadata) 'digest "wrong")) (cons 'fixtures #f)
             (cons 'fixtures (lambda () '()))
             (cons 'fixtures '(invalid))
             (cons 'tests (list (cons 'bad #f)))
             (cons 'tests (list (cons 'fixtures (lambda (_) #t))))
             (cons 'tests (list (cons 'arbitrary (lambda (_) '(accepted)))))
             (cons 'tests (list (cons 'renamed check-language-loader-fixtures!)))
             (cons 'tests (list (cons 'fixtures check-language-loader-fixtures!)))
             (cons 'tests (list (cons 'foreign (declare-language-fixture-test bash-source-language))))
             (cons 'tests (list (cons 'same (declare-language-fixture-test arithmetic-language-grammar))
                                     (cons 'same (declare-language-fixture-test arithmetic-language-grammar))))
             (cons 'scan-workers (list (cons 'bad #f)))
             (cons 'scan-workers (list (cons 'undeclared (source-language-scanner-factory bash-source-language))))
             (cons 'scan-workers (list (cons 'foreign (declare-language-source-scan-worker
                                                      bash-source-language))))))
      (check (rejects? (lambda ()
                        (check-language-loader-fixtures!
                         (.cc arithmetic-loader 'fixtures '(invalid))))) => #t))
    (test-case "inline corpora preserve source bytes, digest and positive/negative conformance"
      (check (inline-fixture-rejections) => '(#t #t #t))
      (let* ((loader (.cc arithmetic-loader 'fixtures inline-arithmetic-fixtures))
             (artifacts (check-language-loader-fixtures! loader)))
        (check (map parse-artifact-success? artifacts) => '(#t #f))
        (check (syntax-fixture-source inline-unicode) => "α + 2")
        (check (syntax-fixture-source-digest inline-unicode) => (sha256-text "α + 2"))))
    (test-case "fixture admission rejects foreign identities and duplicate IDs before parsing"
      (def calls 0)
      (for-each
       (lambda (fixtures)
         (let (loader (.cc (.cc arithmetic-loader 'fixtures fixtures)
                          '.parse (lambda (_) (set! calls (+ calls 1)) #f)))
           (check (rejects? (lambda () (check-language-loader-fixtures! loader))) => #t)))
       (list (list foreign-language) (list foreign-version) (list foreign-contract)
             (list arithmetic-basic-fixture arithmetic-basic-fixture)
             ;; Admission covers the whole corpus before parsing its valid prefix.
             (list arithmetic-basic-fixture foreign-contract)))
      (check calls => 0))
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
                                     LineSourceStrategy.))
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
