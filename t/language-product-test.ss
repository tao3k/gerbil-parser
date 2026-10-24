;;; Product harness controls exercise data quoting and temporary ownership.
(import :std/test
        :gerbil-parser/language-test-support
        (only-in :gerbil-parser/languages/tla-plus/parser tla-plus-core-language
                 qualify-tla-plus-core-model tla-plus-model-receipt?)
        (only-in :gerbil-parser/src/testing/language-product language-model-test-receipt)
        (only-in :gerbil-parser/src/runtime/identity sha256-text))

(def source "---- MODULE Quote ----\nVARIABLE enabled\nInit == enabled = FALSE\nNext == enabled' = TRUE\n====\n")
(def config "INIT Init\nNEXT Next\n")
(def output "TLC2 Version fixture'$(printf corrupted)\n")
(deflanguage-parser-tests language-product-test "closed model tool fixtures"
  (engine)
    (property "stdout is inert data and all temporary files are removed"
      (bindings (receipt (language-model-test-receipt tla-plus-core-language "Quote" source config (list 'stdout output 0) 1)) (ref (lambda (key) (cdr (assq key receipt)))) (directory (path-directory (ref 'tool-path))))
      (equal (ref 'output-digest)  (sha256-text output))
      (equal (ref 'tlc-version)  "fixture'$(printf corrupted)")
      (equal (ref 'source-digest)  (sha256-text source))
      (equal (ref 'config-digest)  (sha256-text config))
      (equal (ref 'admitted)  #f)
      (equal (file-exists? directory)  #f)))
