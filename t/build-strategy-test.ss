(import :gerbil-parser/language-test-support)
;;; Two real output targets share one engine admission and Loader dispatch.
(import :std/test
        (only-in :clan/poo/object .o .cc .ref .slot?)
        (only-in :clan/poo/mop validate)
        (only-in :gerbil-parser/language-build-support
                 BuildStrategy. BuildStrategyContract RustRuntimeStrategy. FusedReductionStrategy.
                 make-bound-build-strategy make-fused-reduction-strategy make-rust-runtime-strategy
                 make-recursive-source-strategy
                 declare-language-build-strategy emit-build-strategy emit-language-build-strategy)
        (only-in :gerbil-parser/language-support/development LanguageDevelopmentLoaderContract deflanguage-development-loader)
        (only-in :gerbil-parser/src/runtime/artifact parse-artifact-valid?)
        (only-in :gerbil-parser/src/compiler/build-strategy declare-build-strategy-provider)
        (only-in :gerbil-parser/src/compiler/rust-runtime language-rust-runtime-module-source)
        (only-in :gerbil-parser/src/language/descriptor language-grammar-with-parser-policy)
        (only-in :gerbil-parser/languages/hcl/parser  hcl-language-grammar)
        (only-in :gerbil-parser/languages/arithmetic/parser  arithmetic-language-grammar))


;;; This owner exercises engine admission; language corpus suites own fixtures.
(deflanguage-development-loader hcl-test-language
  (grammar hcl-language-grammar)
  (parse parse-hcl-strategy-test)
  (slots build-strategies:
         (list (declare-language-build-strategy 'fused-reductions
                 (make-fused-reduction-strategy hcl-language-grammar))
               (declare-language-build-strategy 'rust-runtime
                 (make-rust-runtime-strategy hcl-language-grammar)))))
(deflanguage-development-loader arithmetic-test-language
  (grammar arithmetic-language-grammar)
  (parse parse-arithmetic-strategy-test)
  (slots build-strategies:
         (list (declare-language-build-strategy 'fused-reductions
                 (make-fused-reduction-strategy arithmetic-language-grammar))
               (declare-language-build-strategy 'rust-runtime
                 (make-rust-runtime-strategy arithmetic-language-grammar)))))

(def (loader-output loader name)
  (call-with-output-string (lambda (port) (emit-language-build-strategy loader name port))))
(def (strategy-output strategy)
  (call-with-output-string (lambda (port) (emit-build-strategy strategy port))))

(deflanguage-parser-tests build-strategy-test "common POO build capability protocol"
  (engine)
    (scenario "both languages declare two different admitted providers"
      (each (loader) ((list hcl-test-language arithmetic-test-language))
         (sequential ((rows (.ref loader 'build-strategies))
                (scheme (cdr (assq 'fused-reductions rows)))
                (rust (cdr (assq 'rust-runtime rows))))
           (equal (length rows)  2)
           (act (validate BuildStrategyContract scheme))
           (act (validate BuildStrategyContract rust))
           (equal (.ref scheme 'kind)  'fused-reductions)
           (equal (.ref scheme 'format)  'scheme)
           (equal (.ref rust 'kind)  'rust-runtime)
           (equal (.ref rust 'format)  'rust))))
    (scenario "the same Loader dispatcher preserves canonical Rust output in both languages"
      (each (loader) ((list hcl-test-language arithmetic-test-language))
         (equal (loader-output loader 'rust-runtime)
                 (language-rust-runtime-module-source (.ref loader 'descriptor)))))
    (scenario "generic declaration supports downstream names and inherited POO metadata"
      (sequential ((prototype (.o (:: self RustRuntimeStrategy.) metadata: (.o consumer: 'downstream)))
             (strategy (make-rust-runtime-strategy arithmetic-language-grammar prototype))
             (loader (.cc arithmetic-test-language 'build-strategies
                          (list (declare-language-build-strategy 'my-rust-output strategy)))))
        (act (validate LanguageDevelopmentLoaderContract loader))
        (equal (.ref (.ref strategy 'metadata) 'consumer)  'downstream)
        (equal (loader-output loader 'my-rust-output)  (strategy-output strategy))))
    (property "a recipe method override cannot replace the registered engine writer"
      (bindings (strategy (.cc (make-rust-runtime-strategy arithmetic-language-grammar)
                           '.emit (lambda (_) (error "unchecked writer must not execute")))) (loader (.cc arithmetic-test-language 'build-strategies
                          (list (declare-language-build-strategy 'rust-output strategy)))))
      (equal (loader-output loader 'rust-output)
                (language-rust-runtime-module-source arithmetic-language-grammar)))
    (scenario "unregistered providers and forged target identity reject before writing"
      (scope ((strategy (make-rust-runtime-strategy arithmetic-language-grammar))
            (port (open-output-string)))
        (each (invalid) ((list BuildStrategy.
               (.cc strategy 'provider (.o kind: 'rust-runtime .emit: (lambda (_) (error "unchecked"))))
               (.cc strategy 'kind 'fused-reductions)
               (.cc strategy 'format 'scheme)
               (.cc strategy 'digest "sha256:stale")))
           (raises (validate BuildStrategyContract invalid) true)
           (raises (emit-build-strategy invalid port) true))
        (equal (get-output-string port)  "")
        (raises (make-rust-runtime-strategy arithmetic-language-grammar FusedReductionStrategy.) true)))
    (scenario "an engine extension adds a constrained recipe and format without dispatcher edits"
      (sequential ((admissions 0) (emissions 0)
             (engine-provider
              (declare-build-strategy-provider
               'test-receipt 'test-format
               (lambda (candidate)
                 (set! admissions (+ admissions 1))
                 (and (.slot? candidate 'receipt) (eq? (.ref candidate 'receipt) 'qualified)))
               (parser-test-datum-writer
                 (lambda (candidate)
                   (set! emissions (+ emissions 1))
                   (list 'engine-receipt (.ref candidate 'receipt))))))
             (prototype (.o (:: self BuildStrategy.) provider: engine-provider receipt: 'qualified))
             (strategy (make-bound-build-strategy arithmetic-language-grammar prototype))
             (loader (.cc arithmetic-test-language 'build-strategies
                          (list (declare-language-build-strategy 'custom-receipt strategy)))))
        (equal (.ref strategy 'format)  'test-format)
        (equal (loader-output loader 'custom-receipt)  "(engine-receipt qualified)")
        (scope ((before-admissions admissions) (before-emissions emissions))
          (each (_) ((iota 8)) (equal (parse-artifact-valid? ((.ref loader '.parse) "1 + 2"))  #t))
          (equal admissions  before-admissions)
          (equal emissions  before-emissions))
        (raises (make-bound-build-strategy arithmetic-language-grammar
                          (.cc prototype 'receipt 'unknown)) true)))
    (scenario "the common registry rejects foreign descriptors across provider kinds"
      (raises
       (validate LanguageDevelopmentLoaderContract
        (.cc hcl-test-language 'build-strategies
             (list (declare-language-build-strategy 'rust-output
                     (make-rust-runtime-strategy arithmetic-language-grammar))))) true))
    (scenario "recursive emission admits only its reviewed grammar before writing"
      (sequential ((strategy (make-recursive-source-strategy hcl-language-grammar))
             (port (open-output-string)))
        (act (validate BuildStrategyContract strategy))
        (raises (make-recursive-source-strategy arithmetic-language-grammar) true)
        (raises
         (emit-build-strategy (.cc strategy 'descriptor arithmetic-language-grammar) port) true)
        (equal (get-output-string port)  "")))
    (scenario "Rust policy admission remains distinct from Scheme reduction admission"
      (scope ((descriptor (language-grammar-with-parser-policy
                        arithmetic-language-grammar "test-policy" 16 (lambda (_) #f))))
        (raises (make-rust-runtime-strategy descriptor) true)
        (act (validate BuildStrategyContract (make-fused-reduction-strategy descriptor))))))
