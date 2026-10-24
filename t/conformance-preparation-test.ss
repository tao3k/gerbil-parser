;;; Exercise the real script in its own SDK top context, as the CLI does.
(import :gerbil-parser/language-test-support
        (only-in :gerbil/expander current-expander-context make-top-context eval-syntax))
(def (source-conformance-compiler-identity)
  (parameterize ((current-expander-context (make-top-context)))
    (eval-syntax '(import :gerbil/core))
    (for-each eval-syntax (read-syntax-from-file "scripts/preparation-cache.ss"))
    (eval-syntax '(conformance-compiler-identity))))

(deflanguage-parser-tests conformance-preparation-test "conformance source preparation"
  (engine (exclusive #t))
  (property "compiler identity resolves its explicit digest dependency"
    (bindings (identity (source-conformance-compiler-identity)))
    (equal (length identity) 2)
    (equal (file-exists? (car identity)) #t)
    (equal (and (u8vector? (cadr identity)) (= (u8vector-length (cadr identity)) 32)) #t)))
