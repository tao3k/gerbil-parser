;;; Product harness controls exercise data quoting and temporary ownership.
(import :std/test
        :gerbil-parser/language-test-support
        (only-in :gerbil-parser/languages/tla-plus/parser tla-plus-core-language
                 qualify-tla-plus-core-model tla-plus-model-receipt?)
        (rename-in (only-in :gerbil-parser/languages/tla-plus/qualification
                            qualify-tla-plus-core-model tla-plus-model-receipt?)
                   (qualify-tla-plus-core-model legacy-qualify)
                   (tla-plus-model-receipt? legacy-receipt?))
        (only-in :gerbil-parser/src/testing/language-product language-model-test-receipt)
        (only-in :gerbil-parser/src/runtime/identity sha256-text))
(export language-product-test)
(def source "---- MODULE Quote ----\nVARIABLE enabled\nInit == enabled = FALSE\nNext == enabled' = TRUE\n====\n")
(def config "INIT Init\nNEXT Next\n")
(def output "TLC2 Version fixture'$(printf corrupted)\n")
(def language-product-test
  (test-suite "closed model tool fixtures"
    (test-case "legacy qualification import resolves the canonical engine API"
      (check (eq? legacy-qualify qualify-tla-plus-core-model) => #t)
      (check (eq? legacy-receipt? tla-plus-model-receipt?) => #t))
    (test-case "stdout is inert data and all temporary files are removed"
      (let* ((receipt (language-model-test-receipt tla-plus-core-language "Quote" source config (list 'stdout output 0) 1))
             (ref (lambda (key) (cdr (assq key receipt))))
             (directory (path-directory (ref 'tool-path))))
        (check (ref 'output-digest) => (sha256-text output))
        (check (ref 'tlc-version) => "fixture'$(printf corrupted)")
        (check (ref 'source-digest) => (sha256-text source))
        (check (ref 'config-digest) => (sha256-text config))
        (check (ref 'admitted) => #f)
        (check (file-exists? directory) => #f)))))
