;;; Two real output targets share one engine admission and Loader dispatch.
(import :std/test
        (only-in :clan/poo/object .o .cc .ref .slot?)
        (only-in :clan/poo/mop validate)
        (only-in :gerbil-parser/language-build-support
                 BuildStrategy. BuildStrategyContract RustRowanStrategy. FusedReductionStrategy.
                 make-bound-build-strategy make-fused-reduction-strategy make-rust-rowan-strategy
                 declare-language-build-strategy emit-build-strategy emit-language-build-strategy)
        (only-in :gerbil-parser/language-support/development LanguageDevelopmentLoaderContract)
        (only-in :gerbil-parser/src/runtime/artifact parse-artifact-valid?)
        (only-in :gerbil-parser/src/compiler/build-strategy declare-build-strategy-provider)
        (only-in :gerbil-parser/src/compiler/rust-rowan language-rust-rowan-module-source)
        (only-in :gerbil-parser/src/language/descriptor language-grammar-with-parser-policy)
        (only-in :gerbil-parser/languages/hcl/parser hcl-language hcl-language-grammar)
        (only-in :gerbil-parser/languages/arithmetic/parser arithmetic-language arithmetic-language-grammar))
(export build-strategy-test)
(def (loader-output loader name)
  (call-with-output-string (lambda (port) (emit-language-build-strategy loader name port))))
(def (strategy-output strategy)
  (call-with-output-string (lambda (port) (emit-build-strategy strategy port))))

(def build-strategy-test
  (test-suite "common POO build capability protocol"
    (test-case "both languages declare two different admitted providers"
      (for-each
       (lambda (loader)
         (let* ((rows (.ref loader 'build-strategies))
                (scheme (cdr (assq 'fused-reductions rows)))
                (rust (cdr (assq 'rust-rowan rows))))
           (check (length rows) => 2)
           (validate BuildStrategyContract scheme)
           (validate BuildStrategyContract rust)
           (check (.ref scheme 'kind) => 'fused-reductions)
           (check (.ref scheme 'format) => 'scheme)
           (check (.ref rust 'kind) => 'rust-rowan)
           (check (.ref rust 'format) => 'rust)))
       (list hcl-language arithmetic-language)))
    (test-case "the same Loader dispatcher preserves canonical Rust output in both languages"
      (for-each
       (lambda (loader)
         (check (loader-output loader 'rust-rowan)
                => (language-rust-rowan-module-source (.ref loader 'descriptor))))
       (list hcl-language arithmetic-language)))
    (test-case "generic declaration supports downstream names and inherited POO metadata"
      (let* ((prototype (.o (:: self RustRowanStrategy.) metadata: (.o consumer: 'downstream)))
             (strategy (make-rust-rowan-strategy arithmetic-language-grammar prototype))
             (loader (.cc arithmetic-language 'build-strategies
                          (list (declare-language-build-strategy 'my-rust-output strategy)))))
        (validate LanguageDevelopmentLoaderContract loader)
        (check (.ref (.ref strategy 'metadata) 'consumer) => 'downstream)
        (check (loader-output loader 'my-rust-output) => (strategy-output strategy))))
    (test-case "a recipe method override cannot replace the registered engine writer"
      (let* ((strategy (.cc (make-rust-rowan-strategy arithmetic-language-grammar)
                           '.emit (lambda (_) (error "unchecked writer must not execute"))))
             (loader (.cc arithmetic-language 'build-strategies
                          (list (declare-language-build-strategy 'rust-output strategy)))))
        (check (loader-output loader 'rust-output)
               => (language-rust-rowan-module-source arithmetic-language-grammar))))
    (test-case "unregistered providers and forged target identity reject before writing"
      (let ((strategy (make-rust-rowan-strategy arithmetic-language-grammar))
            (port (open-output-string)))
        (for-each
         (lambda (invalid)
           (check-exception (validate BuildStrategyContract invalid) true)
           (check-exception (emit-build-strategy invalid port) true))
         (list BuildStrategy.
               (.cc strategy 'provider (.o kind: 'rust-rowan .emit: (lambda (_) (error "unchecked"))))
               (.cc strategy 'kind 'fused-reductions)
               (.cc strategy 'format 'scheme)
               (.cc strategy 'digest "sha256:stale")))
        (check (get-output-string port) => "")
        (check-exception (make-rust-rowan-strategy arithmetic-language-grammar FusedReductionStrategy.) true)))
    (test-case "an engine extension adds a constrained recipe and format without dispatcher edits"
      (let* ((admissions 0) (emissions 0)
             (engine-provider
              (declare-build-strategy-provider
               'test-receipt 'test-format
               (lambda (candidate)
                 (set! admissions (+ admissions 1))
                 (and (.slot? candidate 'receipt) (eq? (.ref candidate 'receipt) 'qualified)))
               (lambda (candidate port)
                 (set! emissions (+ emissions 1))
                 (let (form (list 'engine-receipt (.ref candidate 'receipt)))
                   (write form port)))))
             (prototype (.o (:: self BuildStrategy.) provider: engine-provider receipt: 'qualified))
             (strategy (make-bound-build-strategy arithmetic-language-grammar prototype))
             (loader (.cc arithmetic-language 'build-strategies
                          (list (declare-language-build-strategy 'custom-receipt strategy)))))
        (check (.ref strategy 'format) => 'test-format)
        (check (loader-output loader 'custom-receipt) => "(engine-receipt qualified)")
        (let ((before-admissions admissions) (before-emissions emissions))
          (for-each (lambda (_) (check (parse-artifact-valid? ((.ref loader '.parse) "1 + 2")) => #t)) (iota 8))
          (check admissions => before-admissions)
          (check emissions => before-emissions))
        (check-exception (make-bound-build-strategy arithmetic-language-grammar
                          (.cc prototype 'receipt 'unknown)) true)))
    (test-case "the common registry rejects foreign descriptors across provider kinds"
      (check-exception
       (validate LanguageDevelopmentLoaderContract
        (.cc hcl-language 'build-strategies
             (list (declare-language-build-strategy 'rust-output
                     (make-rust-rowan-strategy arithmetic-language-grammar))))) true))
    (test-case "Rust policy admission remains distinct from Scheme reduction admission"
      (let (descriptor (language-grammar-with-parser-policy
                        arithmetic-language-grammar "test-policy" 16 (lambda (_) #f)))
        (check-exception (make-rust-rowan-strategy descriptor) true)
        (validate BuildStrategyContract (make-fused-reduction-strategy descriptor))))))
