;;; Stable loader identity and ordinary POO value slots use the unreleased v2 contract.
(import :std/test
        (only-in :clan/poo/object .o .ref .cc)
        (only-in :clan/poo/mop element?)
        (only-in :gerbil-parser/src/language/entry
                 deflanguage-parser-loader LanguageLoader. LanguageLoaderContract
                 +language-parser-entry-schema+ run-language-test)
        (only-in :gerbil-parser/src/language/source declare-source-language)
        (only-in :gerbil-parser/language-support/fixture defsyntax-fixture))
(export language-loader-value-test)

(def descriptor
  (declare-source-language "loader-value" "0.12.2" "loader-value.test"
                           (lambda (_) (error "scanner must not run"))
                           (lambda _ (error "parser must not run"))))
(def Pack.
  (.o (:: self LanguageLoader.)
      metadata: (.o version: "spoofed" digest: "spoofed" upstream-commit: "pinned")
      fixtures: '()
      (edition-summary (.ref (.ref self 'metadata) 'version))))
(deflanguage-parser-loader (loader :: self Pack.)
  (source descriptor) (parse parse-loader))
(defsyntax-fixture foreign
  (identity "foreign" "another-language" "0.12.2" "loader-value.test")
  (text "x") (expect rejected #f ()))
(def (rejects? thunk)
  (with-catch (lambda (_) #t) (lambda () (thunk) #f)))

(def language-loader-value-test
  (test-suite "language loader value slots"
    (test-case "metadata seals identity and preserves extension values without a schema bump"
      (check +language-parser-entry-schema+ => "gerbil-parser.language-entry.v2")
      (check (.ref (.ref loader 'metadata) 'language) => "loader-value")
      (check (.ref (.ref loader 'metadata) 'version) => "0.12.2")
      (check (.ref (.ref loader 'metadata) 'contract) => "loader-value.test")
      (check (.ref (.ref loader 'metadata) 'digest-kind) => 'source-identity)
      (check (equal? (.ref (.ref loader 'metadata) 'digest) "spoofed") => #f)
      (check (.ref (.ref loader 'metadata) 'upstream-commit) => "pinned")
      (check (.ref loader 'edition-summary) => "0.12.2")
      (check (element? LanguageLoaderContract loader) => #t)
      (check (element? LanguageLoaderContract
                       (.cc loader 'metadata (.cc (.ref loader 'metadata) 'version "spoofed"))) => #f))
    (test-case "fixture values inherit and replace without callback execution"
      (check (.ref loader 'fixtures) => '())
      (check (run-language-test loader 'fixtures) => '())
      (check (element? LanguageLoaderContract (.cc loader 'fixtures (lambda () '()))) => #f)
      (check (element? LanguageLoaderContract (.cc loader 'fixtures '(invalid))) => #f)
      (check (rejects? (lambda () (run-language-test (.cc loader 'fixtures (list foreign)) 'fixtures))) => #t))
    (test-case "new descriptor revisions change metadata under the same public loader contract"
      (def next-descriptor
        (declare-source-language "loader-value" "0.12.3" "loader-value.test"
                                 (lambda (_) #f) (lambda _ #f)))
      (deflanguage-parser-loader (next-loader :: self Pack.)
        (source next-descriptor) (parse parse-next-loader))
      (check (.ref (.ref next-loader 'metadata) 'version) => "0.12.3")
      (check (equal? (.ref (.ref next-loader 'metadata) 'digest)
                     (.ref (.ref loader 'metadata) 'digest)) => #f)
      (check (.ref next-loader 'edition-summary) => "0.12.3")
      (check (.ref next-loader 'schema) => (.ref loader 'schema)))))
