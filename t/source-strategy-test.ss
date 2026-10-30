(import :gerbil-parser/language-test-support)
;;; A second source family shares admission, identity, workers and publication.
(import :std/test
        (only-in :clan/poo/object .o .cc)
        (only-in :gerbil-parser/src/language/source-strategy SourceStrategy. bind-source-strategy declare-source-strategy-provider make-source-engine source-engine-scanner source-engine-factory source-engine-parse source-engine-receipt source-engine-results source-engine-program)
        (only-in :gerbil-parser/src/runtime/source-engines LineSourceStrategy.)
        (only-in :gerbil-parser/src/compiler/contextual-program contextual-program-ir)
        (only-in :gerbil-parser/src/runtime/contextual-ir contextual-parser-ir-valid? contextual-ir-ref)
        (only-in :gerbil-parser/src/compiler/rust-scanner command-source-rust-module-source)
        (only-in :gerbil-parser/t/fixtures/bash-products bash-results bash-parts bash-word-regions bash-commands bash-command-scanner)
        (only-in :gerbil-parser/src/runtime/source-engines ShellSourceStrategy.)
        (only-in :gerbil-parser/src/language/source declare-source-language parse-source-language source-language-digest source-language-contextual-ir
                 parse-source-language/session source-language-session-artifact
                 source-language-session-scanned-token-count source-language-session-reused-token-count)
        (only-in :gerbil-parser/src/runtime/artifact parse-artifact-valid? parse-artifact-success? parse-artifact-roundtrip)
        (only-in :gerbil-parser/language-support/development deflanguage-development-loader LanguageDevelopmentLoader. declare-language-source-scan-worker make-language-scan-worker)
        (only-in :gerbil-parser/src/runtime/source-scanner source-scanner-tokens)
        (only-in :gerbil-parser/src/runtime/token token-lexeme))

(def prefix (string-copy "#"))
(def notes-source
 (declare-source-language "notes" "v1" "notes.v1"
  (.o (:: self LineSourceStrategy.) root-kind: 'Notes required-prefix: prefix)))
(deflanguage-development-loader (notes-language :: self LanguageDevelopmentLoader.)
 (source notes-source) (parse parse-notes)
 (slots scan-workers: (list (cons 'lines (declare-language-source-scan-worker notes-source)))))
(def (rejects? thunk) (with-catch (lambda (_) #t) (lambda () (thunk) #f)))
(deflanguage-parser-tests source-strategy-test "source recipe reuse and ownership"
  (engine)
  (property "ordinary Source sessions preserve the declared scanner and do not admit reuse"
      (bindings (old (parse-source-language/session notes-source "#old\n")) (next (parse-source-language/session notes-source "#new\n" old)))
      (equal (source-language-session-artifact next)  (parse-source-language notes-source "#new\n"))
      (equal (source-language-session-scanned-token-count next)  #f)
      (equal (source-language-session-reused-token-count next)  0)
      (equal (rejects? (lambda ()
                       (parse-source-language/session
                        (declare-source-language "foreign" "v1" "foreign.v1" LineSourceStrategy.)
                        "#new\n" old)))  #t))
  (scenario "one compilation supplies both identity and executable source product"
   (scope ((classifications 0) (compilations 0))
    (scope-values (((recipe engine) (bind-source-strategy LineSourceStrategy.)))
     (sequential ((counted-provider (declare-source-strategy-provider (quote counted)
                       (lambda (_) (set! classifications (+ classifications 1)) #t)
                       (lambda (_) (set! compilations (+ compilations 1))
                         (values (cadr recipe) engine))))
            (strategy (.o (:: self SourceStrategy.) provider: counted-provider)))
      (scope-values (((compiled-recipe compiled-engine) (bind-source-strategy strategy)))
       (equal classifications  1)
       (equal compilations  1)
       (equal compiled-recipe  (list (quote counted) (cadr recipe)))
       (equal (eq? compiled-engine engine)  #t))))))
  (scenario "Rust lowering consumes admitted products without reading declaration slots"
   (sequential ((strategy (.o (:: self ShellSourceStrategy.) regions: bash-word-regions
                        results: bash-results parts: bash-parts commands: bash-commands
                        scanner: bash-command-scanner))
          (reference (command-source-rust-module-source strategy))
          (compilations 0))
    (scope-values (((recipe engine) (bind-source-strategy strategy)))
     ;; Only the provider remains: rereading any declaration slot must fail.
     (sequential ((admitted-provider (declare-source-strategy-provider 'shell (lambda (_) #t)
                       (lambda (_) (set! compilations (+ compilations 1))
                         (values (cadr recipe) engine))))
            (bound (.o (:: self SourceStrategy.) provider: admitted-provider)))
       (equal (command-source-rust-module-source bound)  reference)
       (equal compilations  1)))))
  (scenario "common contextual product rejects malformed and ambiguous envelopes"
   (scope-values (((recipe engine) (bind-source-strategy LineSourceStrategy.)))
    (scope ((ir (contextual-program-ir (source-engine-program engine))))
     (equal (contextual-parser-ir-valid? ir)  #t)
     (equal (contextual-ir-ref (contextual-ir-ref ir 'recognition) 'dialect)  'lines)
     (each (bad) ((list '(invalid) (cons '(root-kind . SourceFile) ir)
             (cons '(foreign . 1) ir)
             (map (lambda (row) (if (eq? (car row) 'root-kind)
                                  '(root-kind . Missing) row)) ir)
             (map (lambda (row) (if (eq? (car row) 'recognition)
                                  (cons 'recognition (list (cons 'dialect 'lines)
                                                          (cons 'program (lambda () #t)))) row)) ir))) (equal (contextual-parser-ir-valid? bad)  #f))))
   (scope-values (((recipe engine) (bind-source-strategy LineSourceStrategy.)))
    (sequential ((mismatched-provider (declare-source-strategy-provider 'mismatch (lambda (_) #t)
                      (lambda (_) (values (cons '(foreign . 1) (cadr recipe)) engine))))
           (strategy (.o (:: self SourceStrategy.) provider: mismatched-provider)))
      (equal (rejects? (lambda () (bind-source-strategy strategy)))  #t))))
  (scenario "source roots must belong to the compiled result catalog"
   (scope-values (((recipe engine) (bind-source-strategy LineSourceStrategy.)))
    (sequential ((bad (make-source-engine (source-engine-scanner engine)
                 (source-engine-factory engine) (source-engine-parse engine)
                 (source-engine-receipt engine) (source-engine-results engine) 'UndeclaredRoot (source-engine-program engine)))
           (bad-root-provider (declare-source-strategy-provider 'bad-root (lambda (_) #t)
                       (lambda (_) (values (cadr recipe) bad))))
           (strategy (.o (:: self SourceStrategy.) provider: bad-root-provider)))
     (equal (rejects? (lambda () (bind-source-strategy strategy)))  #t))))
  (scenario "line recipe publishes lossless UTF-8 artifacts and owned workers"
   (sequential ((source "#α\nsecond\n") (artifact (parse-notes source))
          (worker (make-language-scan-worker notes-language 'lines source)))
    (equal (parse-artifact-valid? artifact)  #t)
    (equal (parse-artifact-success? artifact)  #t)
    (equal (parse-artifact-roundtrip artifact)  source)
    (equal (apply string-append (map token-lexeme (source-scanner-tokens worker 'lines)))  source))
   (equal (parse-artifact-success? (parse-notes "different"))  #f))
  (scenario "source descriptors retain an owned canonical product"
   (scope ((ir (source-language-contextual-ir notes-source)))
     (equal (contextual-parser-ir-valid? ir)  #t)
     (act (set-cdr! (assq 'root-kind ir) 'Foreign))
     (equal (contextual-ir-ref (source-language-contextual-ir notes-source) 'root-kind)  'Notes)
     (equal (parse-artifact-success? (parse-notes "#kept"))  #t)))
  (scenario "Scheme source authority admits the deep mixed continuation control losslessly"
   (sequential ((strategy (.o (:: self ShellSourceStrategy.) regions: bash-word-regions
                        results: bash-results parts: bash-parts commands: bash-commands
                        scanner: bash-command-scanner))
          (descriptor (declare-source-language "source-control" "v1" "source-control.v1" strategy))
          (source (string-append (apply string-append (make-list 512 "{ ( "))
                                 "echo α; " (apply string-append (make-list 512 "); } ; "))))
          (artifact (parse-source-language descriptor source)))
     (equal (parse-artifact-success? artifact)  #t)
     (equal (parse-artifact-valid? artifact)  #t)
     (equal (parse-artifact-roundtrip artifact)  source)))
  (property "source identity includes the closed engine recipe"
      (bindings (other (declare-source-language "notes" "v1" "notes.v1"
                (.o (:: self LineSourceStrategy.) root-kind: 'Different required-prefix: "#"))))
      (equal (equal? (source-language-digest other) (source-language-digest notes-source))  #f))
  (scenario "binding snapshots values and rejects invalid provider overrides"
   (act (string-set! prefix 0 #\!))
   (equal (parse-artifact-success? (parse-notes "#kept"))  #t)
   (equal (rejects? (lambda () (declare-source-language "bad" "v1" "bad.v1" (.cc LineSourceStrategy. 'root-kind "wrong"))))  #t)
   (equal (rejects? (lambda () (declare-source-language "bad" "v1" "bad.v1" (.cc LineSourceStrategy. 'provider (lambda (_) #t)))))  #t)))
