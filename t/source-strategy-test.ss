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
(export source-strategy-test)
(def prefix (string-copy "#"))
(def notes-source
 (declare-source-language "notes" "v1" "notes.v1"
  (.o (:: self LineSourceStrategy.) root-kind: 'Notes required-prefix: prefix)))
(deflanguage-development-loader (notes-language :: self LanguageDevelopmentLoader.)
 (source notes-source) (parse parse-notes)
 (slots scan-workers: (list (cons 'lines (declare-language-source-scan-worker notes-source)))))
(def (rejects? thunk) (with-catch (lambda (_) #t) (lambda () (thunk) #f)))
(def source-strategy-test
 (test-suite "source recipe reuse and ownership"
  (test-case "ordinary Source sessions preserve the declared scanner and do not admit reuse"
   (let* ((old (parse-source-language/session notes-source "#old\n"))
          (next (parse-source-language/session notes-source "#new\n" old)))
     (check (source-language-session-artifact next) => (parse-source-language notes-source "#new\n"))
     (check (source-language-session-scanned-token-count next) => #f)
     (check (source-language-session-reused-token-count next) => 0)
     (check (rejects? (lambda ()
                       (parse-source-language/session
                        (declare-source-language "foreign" "v1" "foreign.v1" LineSourceStrategy.)
                        "#new\n" old))) => #t)))
  (test-case "one compilation supplies both identity and executable source product"
   (let ((classifications 0) (compilations 0))
    (let-values (((recipe engine) (bind-source-strategy LineSourceStrategy.)))
     (let* ((counted-provider (declare-source-strategy-provider (quote counted)
                       (lambda (_) (set! classifications (+ classifications 1)) #t)
                       (lambda (_) (set! compilations (+ compilations 1))
                         (values (cadr recipe) engine))))
            (strategy (.o (:: self SourceStrategy.) provider: counted-provider)))
      (let-values (((compiled-recipe compiled-engine) (bind-source-strategy strategy)))
       (check classifications => 1)
       (check compilations => 1)
       (check compiled-recipe => (list (quote counted) (cadr recipe)))
       (check (eq? compiled-engine engine) => #t))))))
  (test-case "Rust lowering consumes admitted products without reading declaration slots"
   (let* ((strategy (.o (:: self ShellSourceStrategy.) regions: bash-word-regions
                        results: bash-results parts: bash-parts commands: bash-commands
                        scanner: bash-command-scanner))
          (reference (command-source-rust-module-source strategy))
          (compilations 0))
    (let-values (((recipe engine) (bind-source-strategy strategy)))
     ;; Only the provider remains: rereading any declaration slot must fail.
     (let* ((admitted-provider (declare-source-strategy-provider 'shell (lambda (_) #t)
                       (lambda (_) (set! compilations (+ compilations 1))
                         (values (cadr recipe) engine))))
            (bound (.o (:: self SourceStrategy.) provider: admitted-provider)))
       (check (command-source-rust-module-source bound) => reference)
       (check compilations => 1)))))
  (test-case "common contextual product rejects malformed and ambiguous envelopes"
   (let-values (((recipe engine) (bind-source-strategy LineSourceStrategy.)))
    (let (ir (contextual-program-ir (source-engine-program engine)))
     (check (contextual-parser-ir-valid? ir) => #t)
     (check (contextual-ir-ref (contextual-ir-ref ir 'recognition) 'dialect) => 'lines)
     (for-each (lambda (bad) (check (contextual-parser-ir-valid? bad) => #f))
       (list '(invalid) (cons '(root-kind . SourceFile) ir)
             (cons '(foreign . 1) ir)
             (map (lambda (row) (if (eq? (car row) 'root-kind)
                                  '(root-kind . Missing) row)) ir)
             (map (lambda (row) (if (eq? (car row) 'recognition)
                                  (cons 'recognition (list (cons 'dialect 'lines)
                                                          (cons 'program (lambda () #t)))) row)) ir)))))
   (let-values (((recipe engine) (bind-source-strategy LineSourceStrategy.)))
    (let* ((mismatched-provider (declare-source-strategy-provider 'mismatch (lambda (_) #t)
                      (lambda (_) (values (cons '(foreign . 1) (cadr recipe)) engine))))
           (strategy (.o (:: self SourceStrategy.) provider: mismatched-provider)))
      (check (rejects? (lambda () (bind-source-strategy strategy))) => #t))))
  (test-case "source roots must belong to the compiled result catalog"
   (let-values (((recipe engine) (bind-source-strategy LineSourceStrategy.)))
    (let* ((bad (make-source-engine (source-engine-scanner engine)
                 (source-engine-factory engine) (source-engine-parse engine)
                 (source-engine-receipt engine) (source-engine-results engine) 'UndeclaredRoot (source-engine-program engine)))
           (bad-root-provider (declare-source-strategy-provider 'bad-root (lambda (_) #t)
                       (lambda (_) (values (cadr recipe) bad))))
           (strategy (.o (:: self SourceStrategy.) provider: bad-root-provider)))
     (check (rejects? (lambda () (bind-source-strategy strategy))) => #t))))
  (test-case "line recipe publishes lossless UTF-8 artifacts and owned workers"
   (let* ((source "#α\nsecond\n") (artifact (parse-notes source))
          (worker (make-language-scan-worker notes-language 'lines source)))
    (check (parse-artifact-valid? artifact) => #t)
    (check (parse-artifact-success? artifact) => #t)
    (check (parse-artifact-roundtrip artifact) => source)
    (check (apply string-append (map token-lexeme (source-scanner-tokens worker 'lines))) => source))
   (check (parse-artifact-success? (parse-notes "different")) => #f))
  (test-case "source descriptors retain an owned canonical product"
   (let (ir (source-language-contextual-ir notes-source))
     (check (contextual-parser-ir-valid? ir) => #t)
     (set-cdr! (assq 'root-kind ir) 'Foreign)
     (check (contextual-ir-ref (source-language-contextual-ir notes-source) 'root-kind) => 'Notes)
     (check (parse-artifact-success? (parse-notes "#kept")) => #t)))
  (test-case "Scheme source authority admits the deep mixed continuation control losslessly"
   (let* ((strategy (.o (:: self ShellSourceStrategy.) regions: bash-word-regions
                        results: bash-results parts: bash-parts commands: bash-commands
                        scanner: bash-command-scanner))
          (descriptor (declare-source-language "source-control" "v1" "source-control.v1" strategy))
          (source (string-append (apply string-append (make-list 512 "{ ( "))
                                 "echo α; " (apply string-append (make-list 512 "); } ; "))))
          (artifact (parse-source-language descriptor source)))
     (check (parse-artifact-success? artifact) => #t)
     (check (parse-artifact-valid? artifact) => #t)
     (check (parse-artifact-roundtrip artifact) => source)))
  (test-case "source identity includes the closed engine recipe"
   (let (other (declare-source-language "notes" "v1" "notes.v1"
                (.o (:: self LineSourceStrategy.) root-kind: 'Different required-prefix: "#")))
    (check (equal? (source-language-digest other) (source-language-digest notes-source)) => #f)))
  (test-case "binding snapshots values and rejects invalid provider overrides"
   (string-set! prefix 0 #\!)
   (check (parse-artifact-success? (parse-notes "#kept")) => #t)
   (check (rejects? (lambda () (declare-source-language "bad" "v1" "bad.v1" (.cc LineSourceStrategy. 'root-kind "wrong")))) => #t)
   (check (rejects? (lambda () (declare-source-language "bad" "v1" "bad.v1" (.cc LineSourceStrategy. 'provider (lambda (_) #t))))) => #t))))
