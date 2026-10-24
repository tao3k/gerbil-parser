(import :gerbil-parser/language-test-support)
;;; POO language loaders bind effective inherited slots before engine dispatch.
(import :std/test
        (only-in :gerbil-parser/language-support/entry deflanguage-parser-loader language-parser-source-receipt)
        (for-syntax (only-in :gerbil/expander core-expand))
        (only-in :clan/poo/object .o .ref .cc object?)
        (only-in :clan/poo/mop define-type element? validate)
        (only-in :yuanxu/object-types/contracts YuanxuContract. yuanxu-classification-evidence)
        (only-in :gerbil-parser/language-support/development
                 deflanguage-development-loader LanguageDevelopmentLoader. LanguageDevelopmentLoaderContract
                 language-parser-entry-ref check-language-loader-fixtures!
                 declare-language-source-scan-worker make-language-scan-worker
                 declare-language-fixture-test run-language-test)
        (only-in :gerbil-parser/languages/arithmetic/parser
                 arithmetic-language-grammar)
        (only-in :gerbil-parser/language-support/fixture
                 defsyntax-fixture defsyntax-corpus syntax-fixture-source syntax-fixture-source-digest)
        (only-in :gerbil-parser/src/runtime/identity sha256-text)
        (only-in :gerbil-parser/languages/arithmetic/parser-test arithmetic-basic-fixture)
        (only-in :gerbil-parser/src/language/source source-language-scanner-factory)
        (only-in "fixtures/source-strategies.ss" test-source-strategy)
        (only-in :gerbil-parser/src/runtime/source-scanner source-scanner-tokens)
        (only-in :gerbil-parser/src/runtime/token token-lexeme)
        (only-in :gerbil-parser/src/language/source declare-source-language)
        (only-in :gerbil-parser/src/runtime/source-engines LineSourceStrategy.)
        (only-in :gerbil-parser/languages/bash/parser bash-source-language)
        (only-in :gerbil-parser/src/runtime/artifact
                 parse-artifact-success? parse-artifact-roundtrip))

(def Dialect.
  (.o (:: @ LanguageDevelopmentLoader.)
      dialect: 'arithmetic
      ;; Identity and engine dispatch from parents must not take control.
      language: "incorrect-parent"
      (.parse (lambda (_) (error "parent parse must never run")))))

(def dialect-admissions 0)

(define-type (DialectContract @ YuanxuContract.)
  identity: 'loader-test/dialect
  .classify: (lambda (candidate context)
               (set! dialect-admissions (+ dialect-admissions 1))
               (let (ok? (eq? (.ref candidate 'dialect) 'arithmetic))
                 (yuanxu-classification-evidence
                  'loader-test/dialect candidate ok?
                  (if ok? '() '((invalid dialect))) context))))

(deflanguage-development-loader (arithmetic-loader :: self Dialect.)
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
     #'(deflanguage-development-loader (invalid-loader @ LanguageDevelopmentLoader.)
         (descriptor #f) (parse invalid-parse)
         (slots (.parse (lambda (_) #f))) (contracts)))
    " | "
    (message
     #'(deflanguage-development-loader (invalid-keyword-loader @ LanguageDevelopmentLoader.)
         (descriptor #f) (parse invalid-keyword-parse)
         (slots schema: "invalid") (contracts))))))

(defsyntax (invalid-declaration-messages stx)
  (def (message form)
    (with-catch (lambda (condition) (error-message condition))
      (lambda () (core-expand form))))
  (datum->syntax
   #'invalid-declaration-messages
   (cons 'quote (list (list
    (message #'(deflanguage-development-loader bad (grammar #f) (parse parse-bad)
                 (slots) (slots)))
    (message #'(deflanguage-development-loader bad (grammar #f) (parse parse-bad)
                 (contracts) (slots)))
    (message #'(deflanguage-development-loader bad (grammar #f) (parse parse-bad)
                 (options)))
    (message #'(deflanguage-development-loader (bad marker self LanguageDevelopmentLoader.)
                 (descriptor #f) (parse parse-bad) (slots) (contracts)))
    (message #'(deflanguage-development-loader bad (grammar #f) (parse "bad"))))))))

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

(deflanguage-parser-tests language-loader-test "POO language loader admission"
  (engine)
    (scenario "source schema slots admit ordinary values and native inherited POO"
(locals ((template '((schema . "test.source.v1") (namespace . test)
                     (sourceVersion . "v1") (upstreamCommit . "first")))
(parent (.o schema: "test.source.v1" namespace: 'test
                      sourceVersion: "v1" upstreamCommit: "first")))

      (act (deflanguage-parser-loader value-entry (descriptor arithmetic-language-grammar)
        (parse value-parse) (slots source-schema: template)))

      (act (deflanguage-parser-loader inherited-entry (descriptor arithmetic-language-grammar)
        (parse inherited-parse)
        (slots source-schema: (.o (:: @ parent) upstreamCommit: "second"))))
      (equal (language-parser-source-receipt value-entry '(normalization factoring))
              '((schema . "test.source.v1") (namespace . test) (name . factoring)
                  (kind . normalization) (sourceVersion . "v1") (upstreamCommit . "first")))
      (equal (language-parser-source-receipt inherited-entry 'replacement)
              '((schema . "test.source.v1") (namespace . test) (name . replacement)
                  (sourceVersion . "v1") (upstreamCommit . "second")))
      (equal (language-parser-source-receipt value-entry 'replacement)
              '((schema . "test.source.v1") (namespace . test) (name . replacement)
                  (sourceVersion . "v1") (upstreamCommit . "first")))
      (equal (parse-artifact-success? (value-parse "1 + 2"))  #t)
      (equal (parse-artifact-success? (inherited-parse "1 + 2"))  #t)
      (equal (rejects? (lambda ()
                       (language-parser-source-receipt value-entry '(unknown factoring))))  #t)))
    (scenario "malformed source schemas fail at production entry admission"
      (each (configuration) ('(((schema . "test.source.v1") (namespace . test) (sourceVersion . "v1"))
         ((schema . "test.source.v1") (namespace . test) (sourceVersion . "v1") (sourceVersion . "v2"))
         ((schema . "test.source.v1") (namespace . "wrong") (sourceVersion . "v1") (upstreamCommit . "first"))
         ((schema . "test.source.v1") (namespace . test) (sourceVersion . "v1") (upstreamCommit . ""))
         ((schema . "test.source.v1") (namespace . test) (sourceVersion . "v1") (upstreamCommit . "first") (typo . 1))))
         (equal (rejects? (lambda ()
                  (deflanguage-parser-loader invalid-entry (descriptor arithmetic-language-grammar)
                    (parse invalid-parse) (slots source-schema: configuration))
                  invalid-entry))  #t)))

    (scenario "inherits extensions and binds engine identity and dispatch"
      (equal (object? arithmetic-loader)  #t)
      (equal (element? LanguageDevelopmentLoaderContract arithmetic-loader)  #t)
      (equal (.ref arithmetic-loader 'dialect)  'arithmetic)
      (equal (.ref arithmetic-loader 'dialect-name)  "Arithmetic")
      (equal (.ref arithmetic-loader 'summary)  "Arithmetic dialect")
      (equal (language-parser-entry-ref arithmetic-loader 'language)  "arithmetic")
      (equal (.ref arithmetic-loader 'capabilities)  '(grammar-ir parser-ir scheme))
      (sequential ((source "a + 2 * b") (artifact (parse-loaded-arithmetic source)))
        (equal (parse-artifact-success? artifact)  #t)
        (equal (parse-artifact-roundtrip artifact)  source)))
    (scenario "typed declarations compose with POO extension forms and optional sections"
      (act (deflanguage-development-loader (typed-loader :: self Dialect.)
        (grammar arithmetic-language-grammar) (parse parse-typed)
        (slots label: "typed" (summary (.ref self 'label)))))
      (act (deflanguage-development-loader (contract-loader @ Dialect.)
        (grammar arithmetic-language-grammar) (parse parse-contract)
        (contracts DialectContract)))
      (act (deflanguage-development-loader (self-loader :: self Dialect.)
        (grammar arithmetic-language-grammar) (parse parse-self)))
      (act (deflanguage-development-loader (self-contract-loader :: self Dialect.)
        (grammar arithmetic-language-grammar) (parse parse-self-contract)
        (contracts DialectContract)))
      (equal (.ref typed-loader 'summary)  "typed")
      (each (parse) ((list parse-typed parse-contract parse-self parse-self-contract)) (equal (parse-artifact-success? (parse "a+1"))  #t)))
    (scenario "source declarations use the same extension normalization and lossless dispatch"
      (act (deflanguage-development-loader (source-loader :: self LanguageDevelopmentLoader.)
        (source bash-source-language) (parse parse-source-loaded)
        (slots label: "Bash" (summary (.ref self 'label)))))
      (sequential ((source "echo α\n") (artifact (parse-source-loaded source)))
        (equal (.ref source-loader 'capabilities)  '(scheme-source))
        (equal (.ref source-loader 'summary)  "Bash")
        (equal (parse-artifact-success? artifact)  #t)
        (equal (parse-artifact-roundtrip artifact)  source)))
    (scenario "POO fixture values inherit and override without parser execution"
(locals ((Pack. (.o (:: self Dialect.)
            metadata: (.o edition: "v1" version: "wrong" digest: "wrong")
            fixtures: (list arithmetic-basic-fixture)
            scan-workers: '())))

      (act (deflanguage-development-loader (pack-loader :: self Pack.)
        (grammar arithmetic-language-grammar) (parse parse-pack)))
      (equal (.ref pack-loader 'fixtures)  (list arithmetic-basic-fixture))
      (equal (rejects? (lambda () (run-language-test pack-loader 'missing)))  #t)
      (equal (.ref (.ref pack-loader 'metadata) 'edition)  "v1")
      (equal (.ref (.ref pack-loader 'metadata) 'version)  (.ref pack-loader 'version))
      (equal (equal? (.ref (.ref pack-loader 'metadata) 'digest) "wrong")  #f)
      (equal (.ref pack-loader 'grammars)  (list arithmetic-language-grammar))
      (scope ((artifacts (run-language-test pack-loader 'fixtures)))
        (equal (length artifacts)  1)
        (equal (parse-artifact-success? (car artifacts))  #t))
      (scope ((overridden (.cc pack-loader 'fixtures inline-arithmetic-fixtures)))
        (equal (map parse-artifact-success? (run-language-test overridden 'fixtures))  '(#t #f)))
      (scope ((extended (.cc pack-loader 'fixtures
                         (append (.ref pack-loader 'fixtures) (list inline-incomplete)))))
        (equal (map parse-artifact-success? (run-language-test extended 'fixtures))  '(#t #f)))
      (act (deflanguage-development-loader (worker-loader :: self LanguageDevelopmentLoader.)
        (source bash-source-language) (parse parse-worker-source)
        (slots scan-workers: (list (cons 'bash-command
                                  (declare-language-source-scan-worker
                                   bash-source-language))))))
      (sequential ((source "echo α\n")
             (worker (make-language-scan-worker worker-loader 'bash-command source)))
        (equal (apply string-append (map token-lexeme (source-scanner-tokens worker 'command)))  source))))
    (scenario "typed fixture services extend POO slots while engine checks outcomes"
      (equal (rejects? (lambda () (declare-language-fixture-test #f)))  #t)
      (act (deflanguage-development-loader (extended-tests :: self LanguageDevelopmentLoader.)
        (grammar arithmetic-language-grammar) (parse parse-extended-tests)
        (slots fixtures: (list arithmetic-basic-fixture)
               tests: (list (cons 'conformance (declare-language-fixture-test arithmetic-language-grammar))))))
      (equal (length (run-language-test extended-tests 'conformance))  1)
      (scope ((fake (.cc extended-tests '.parse (lambda (_) #t))))
        (equal (rejects? (lambda () (run-language-test fake 'conformance)))  #t)))
    (scenario "declared source workers reject foreign products and undeclared names"
      (equal (rejects? (lambda ()
                        (declare-language-source-scan-worker arithmetic-language-grammar)))  #t)
      (each (factory) ((list (lambda (_) #f) (lambda (_) ((source-language-scanner-factory bash-source-language) "different source"))))
(locals ((bad-source (declare-source-language "bad-worker" "v1" "bad-worker.test"
                          (test-source-strategy factory (lambda (_) '()) (lambda _ #f)))))

         (act (deflanguage-development-loader (bad-worker-loader :: self LanguageDevelopmentLoader.)
           (source bad-source) (parse parse-bad-worker)
           (slots scan-workers: (list (cons 'command
                                     (declare-language-source-scan-worker bad-source))))))
         (equal (rejects? (lambda () (make-language-scan-worker bad-worker-loader 'command "echo α\n")))  #t)
         (equal (rejects? (lambda () (make-language-scan-worker bad-worker-loader 'missing "echo α\n")))  #t))))
    (scenario "metadata admission rejects invalid descriptors and service registries"
      (each (row) ((list (cons 'grammars '()) (cons 'grammars (list #f))
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
         (equal (element? LanguageDevelopmentLoaderContract (.cc arithmetic-loader (car row) (cdr row)))  #f))
      (equal (rejects? (lambda ()
                        (check-language-loader-fixtures!
                         (.cc arithmetic-loader 'fixtures '(invalid)))))  #t))
    (scenario "inline corpora preserve source bytes, digest and positive/negative conformance"
      (equal (inline-fixture-rejections)  '(#t #t #t))
      (sequential ((loader (.cc arithmetic-loader 'fixtures inline-arithmetic-fixtures))
             (artifacts (check-language-loader-fixtures! loader)))
        (equal (map parse-artifact-success? artifacts)  '(#t #f))
        (equal (syntax-fixture-source inline-unicode)  "α + 2")
        (equal (syntax-fixture-source-digest inline-unicode)  (sha256-text "α + 2"))))
    (scenario "fixture admission rejects foreign identities and duplicate IDs before parsing"
(locals ((calls 0))

      (each (fixtures) ((list (list foreign-language) (list foreign-version) (list foreign-contract)
             (list arithmetic-basic-fixture arithmetic-basic-fixture)
             ;; Admission covers the whole corpus before parsing its valid prefix.
             (list arithmetic-basic-fixture foreign-contract)))
         (scope ((loader (.cc (.cc arithmetic-loader 'fixtures fixtures)
                          '.parse (lambda (_) (set! calls (+ calls 1)) #f))))
           (equal (rejects? (lambda () (check-language-loader-fixtures! loader)))  #t)))
      (equal calls  0)))
    (scenario "parsing reuses the loaded entry without extension admission"
      (scope ((before dialect-admissions))
        (each (source) ('("1" "a + b" "a * (b + 2)"))
           (equal (parse-artifact-success? (parse-loaded-arithmetic source))  #t))
        (equal dialect-admissions  before)))
    (property "rejects a missing descriptor and inconsistent identity"
      (bindings)
      (equal (element? LanguageDevelopmentLoaderContract LanguageDevelopmentLoader.)  #f)
      (equal (rejects? (lambda () (validate LanguageDevelopmentLoaderContract LanguageDevelopmentLoader.)))  #t)
      (equal (element? LanguageDevelopmentLoaderContract (.cc arithmetic-loader 'language "bash"))  #f)
      (equal (element? LanguageDevelopmentLoaderContract (.cc arithmetic-loader 'descriptor #f))  #f))
    (property "rejects engine slot overrides during macro expansion"
      (bindings)
      (equal (engine-slot-rejection-message)
              "language loader extension overrides an engine slot | language loader extension overrides an engine slot"))
    (property "normalization rejects malformed declarations before publishing a loader"
      (bindings)
      (equal (invalid-declaration-messages)
              '("duplicate or out-of-order language loader slots"
                  "duplicate or out-of-order language loader slots"
                  "unknown language loader section"
                  "invalid language loader binding"
                  "language loader parse binding must be an identifier")))
    (property "grammar and source declarations enforce their descriptor kind"
      (bindings (source-descriptor
            (declare-source-language "source-test" "v1" "source-test.v1"
                                     LineSourceStrategy.)))
      (equal
         (rejects? (lambda ()
                     (deflanguage-development-loader wrong-grammar
                       (grammar source-descriptor) (parse parse-wrong-grammar))
                     wrong-grammar))  #t)
      (equal
         (rejects? (lambda ()
                     (deflanguage-development-loader wrong-source
                       (source arithmetic-language-grammar) (parse parse-wrong-source))
                     wrong-source))  #t))
    (property "extension contracts validate the effective slot object"
      (bindings)
      (equal
       (rejects?
        (lambda ()
          (deflanguage-development-loader (invalid-dialect @ Dialect.)
            (descriptor arithmetic-language-grammar)
            (parse parse-invalid-dialect)
            (slots (dialect 'other))
            (contracts DialectContract))
          invalid-dialect))  #t)
      (equal (element? DialectContract arithmetic-loader)  #t)
      (equal (rejects? (lambda () (validate DialectContract (.cc arithmetic-loader 'dialect 'other))))  #t)))
