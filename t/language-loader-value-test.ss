;;; Stable loader identity and ordinary POO value slots use the unreleased v2 contract.
(import :std/test
        (only-in :clan/poo/object .o .ref .cc)
        (only-in :clan/poo/mop element?)
        (only-in :gerbil-parser/language-support/development
                 deflanguage-development-loader LanguageDevelopmentLoader. LanguageDevelopmentLoaderContract
                 +language-parser-entry-schema+ run-language-test
                 language-loader-fixtures language-loader-fixture-count language-loader-fixture)
        (only-in :gerbil-parser/src/language/source declare-source-language)
        (only-in :gerbil-parser/src/runtime/source-engines LineSourceStrategy.)
        (only-in :gerbil-parser/language-support/fixture defsyntax-fixture syntax-fixture-copy syntax-fixture-id syntax-fixture-source
                 syntax-fixture-source-digest syntax-fixture-expected-status))
(export language-loader-value-test)

(def descriptor
  (declare-source-language "loader-value" "0.12.2" "loader-value.test"
                           LineSourceStrategy.))
(def Pack.
  (.o (:: self LanguageDevelopmentLoader.)
      metadata: (.o version: "spoofed" digest: "spoofed" upstream-commit: "pinned")
      fixtures: '()
      (edition-summary (.ref (.ref self 'metadata) 'version))))
(deflanguage-development-loader (loader :: self Pack.)
  (source descriptor) (parse parse-loader))
(defsyntax-fixture foreign
  (identity "foreign" "another-language" "0.12.2" "loader-value.test")
  (text "x") (expect rejected #f ()))
(def (rejects? thunk)
  (with-catch (lambda (_) #t) (lambda () (thunk) #f)))

(def fixture-descriptor
  (declare-source-language "fixture-index" "0.12.2" "fixture-index.test"
    (.o (:: self LineSourceStrategy.) root-kind: 'RecordFile token-kind: 'Record required-prefix: "ok:")))
(defsyntax-fixture fixture-a
  (identity "fixture-index/0000" "fixture-index" "0.12.2" "fixture-index.test")
  (text "ok:α") (expect accepted RecordFile ()))
(defsyntax-fixture fixture-b
  (identity "fixture-index/rejected" "fixture-index" "0.12.2" "fixture-index.test")
  (text "bad") (expect rejected #f ()))
(defsyntax-fixture fixture-c
  (identity "fixture-index/extended" "fixture-index" "0.12.2" "fixture-index.test")
  (text "ok:中") (expect accepted RecordFile ()))
(def FixturePack.
  (.o (:: self LanguageDevelopmentLoader.) fixtures: (list fixture-a fixture-b)
      fixture-catalog: 'forged-parent
      (accepted-count (language-loader-fixture-count self 'accepted))))
(deflanguage-development-loader (fixture-loader :: self FixturePack.)
  (source fixture-descriptor) (parse parse-fixture-index))
(def language-loader-value-test
  (test-suite "language loader value slots"
    (test-case "engine fixture metadata partitions once and runs complete semantic conformance"
      (check (language-loader-fixture-count fixture-loader) => 2)
      (check (language-loader-fixture-count fixture-loader 'accepted) => 1)
      (check (language-loader-fixture-count fixture-loader 'rejected) => 1)
      (check (map syntax-fixture-id (language-loader-fixtures fixture-loader))
             => '("fixture-index/0000" "fixture-index/rejected"))
      (check (map syntax-fixture-id (language-loader-fixtures fixture-loader 'accepted)) => '("fixture-index/0000"))
      (check (map syntax-fixture-id (language-loader-fixtures fixture-loader 'rejected)) => '("fixture-index/rejected"))
      (check (.ref fixture-loader 'accepted-count) => 1)
      (check (eq? (.ref fixture-loader 'fixture-catalog) (.ref fixture-loader 'fixture-catalog)) => #t)
      (check (length (run-language-test fixture-loader 'fixtures)) => 2)
      (check (syntax-fixture-source (language-loader-fixture fixture-loader (string-copy "fixture-index/0000"))) => "ok:α")
      (check (language-loader-fixture fixture-loader "missing") => #f))
    (test-case "POO replacement and extension recompute the effective fixture index"
      (let* ((replacement (.cc fixture-loader 'fixtures (list fixture-c)))
             (extension (.cc fixture-loader 'fixtures (append (.ref fixture-loader 'fixtures) (list fixture-c)))))
        (check (element? LanguageDevelopmentLoaderContract replacement) => #t)
        (check (element? LanguageDevelopmentLoaderContract extension) => #t)
        (check (.ref replacement 'accepted-count) => 1)
        (check (.ref extension 'accepted-count) => 2)
        (check (language-loader-fixture-count replacement 'rejected) => 0)
        (check (map syntax-fixture-id (language-loader-fixtures extension 'accepted))
               => '("fixture-index/0000" "fixture-index/extended"))
        (check (eq? (.ref replacement 'fixture-catalog) (.ref fixture-loader 'fixture-catalog)) => #f)
        (check (length (run-language-test extension 'fixtures)) => 3)))
    (test-case "input and published fixture mutations cannot alter the admitted snapshot"
      (let* ((input (syntax-fixture-copy fixture-a)) (owned-loader (.cc fixture-loader 'fixtures (list input)))
             (snapshot (.ref owned-loader 'fixture-catalog))
             (published (language-loader-fixture owned-loader "fixture-index/0000")))
        (string-set! (syntax-fixture-source input) 0 #\x)
        (string-set! (syntax-fixture-id published) 0 #\x)
        (string-set! (syntax-fixture-source published) 0 #\x)
        (set-car! (language-loader-fixtures owned-loader) fixture-b)
        (check (syntax-fixture-source (language-loader-fixture owned-loader "fixture-index/0000")) => "ok:α")
        (check (language-loader-fixture-count owned-loader 'accepted) => 1)
        (check (length (run-language-test owned-loader 'fixtures)) => 1)))
    (test-case "foreign identities, duplicate ids, invalid digests and forged derived slots reject at admission"
      (let (corrupted (syntax-fixture-copy fixture-a))
        (string-set! (syntax-fixture-source corrupted) 0 #\x)
        (for-each (lambda (candidate) (check (element? LanguageDevelopmentLoaderContract candidate) => #f))
          (list (.cc fixture-loader 'fixtures (list fixture-a (syntax-fixture-copy fixture-a)))
                (.cc fixture-loader 'fixtures (list foreign))
                (.cc fixture-loader 'fixtures (list corrupted))
                (.cc fixture-loader 'fixtures (lambda () '()))
                (.cc fixture-loader 'fixture-catalog 'forged)
                (.cc fixture-loader 'fixture-catalog (.ref loader 'fixture-catalog)))))
      (check (rejects? (lambda () (language-loader-fixtures fixture-loader 'unknown))) => #t)
      (check (rejects? (lambda () (language-loader-fixture-count fixture-loader 'unknown))) => #t)
      (check (rejects? (lambda () (language-loader-fixture fixture-loader 'not-a-string))) => #t))
    (test-case "metadata seals identity and preserves extension values without a schema bump"
      (check +language-parser-entry-schema+ => "gerbil-parser.language-entry.v2")
      (check (.ref (.ref loader 'metadata) 'language) => "loader-value")
      (check (.ref (.ref loader 'metadata) 'version) => "0.12.2")
      (check (.ref (.ref loader 'metadata) 'contract) => "loader-value.test")
      (check (.ref (.ref loader 'metadata) 'digest-kind) => 'source-identity)
      (check (equal? (.ref (.ref loader 'metadata) 'digest) "spoofed") => #f)
      (check (.ref (.ref loader 'metadata) 'upstream-commit) => "pinned")
      (check (.ref loader 'edition-summary) => "0.12.2")
      (check (element? LanguageDevelopmentLoaderContract loader) => #t)
      (check (element? LanguageDevelopmentLoaderContract
                       (.cc loader 'metadata (.cc (.ref loader 'metadata) 'version "spoofed"))) => #f))
    (test-case "fixture values inherit and replace without callback execution"
      (check (.ref loader 'fixtures) => '())
      (check (run-language-test loader 'fixtures) => '())
      (check (element? LanguageDevelopmentLoaderContract (.cc loader 'fixtures (lambda () '()))) => #f)
      (check (element? LanguageDevelopmentLoaderContract (.cc loader 'fixtures '(invalid))) => #f)
      (check (rejects? (lambda () (run-language-test (.cc loader 'fixtures (list foreign)) 'fixtures))) => #t))
    (test-case "new descriptor revisions change metadata under the same public loader contract"
      (def next-descriptor
        (declare-source-language "loader-value" "0.12.3" "loader-value.test"
                                 LineSourceStrategy.))
      (deflanguage-development-loader (next-loader :: self Pack.)
        (source next-descriptor) (parse parse-next-loader))
      (check (.ref (.ref next-loader 'metadata) 'version) => "0.12.3")
      (check (equal? (.ref (.ref next-loader 'metadata) 'digest)
                     (.ref (.ref loader 'metadata) 'digest)) => #f)
      (check (.ref next-loader 'edition-summary) => "0.12.3")
      (check (.ref next-loader 'schema) => (.ref loader 'schema)))))
