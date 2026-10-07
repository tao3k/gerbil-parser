;;; A second source family shares admission, identity, workers and publication.
(import :std/test
        (only-in :clan/poo/object .o .cc)
        (only-in :gerbil-parser/src/language/source-strategy SourceStrategy. bind-source-strategy declare-source-strategy-provider)
        (only-in :gerbil-parser/src/runtime/source-engines LineSourceStrategy.)
        (only-in :gerbil-parser/src/language/source declare-source-language parse-source-language source-language-digest)
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
  (test-case "line recipe publishes lossless UTF-8 artifacts and owned workers"
   (let* ((source "#α\nsecond\n") (artifact (parse-notes source))
          (worker (make-language-scan-worker notes-language 'lines source)))
    (check (parse-artifact-valid? artifact) => #t)
    (check (parse-artifact-success? artifact) => #t)
    (check (parse-artifact-roundtrip artifact) => source)
    (check (apply string-append (map token-lexeme (source-scanner-tokens worker 'lines))) => source))
   (check (parse-artifact-success? (parse-notes "different")) => #f))
  (test-case "source identity includes the closed engine recipe"
   (let (other (declare-source-language "notes" "v1" "notes.v1"
                (.o (:: self LineSourceStrategy.) root-kind: 'Different required-prefix: "#")))
    (check (equal? (source-language-digest other) (source-language-digest notes-source)) => #f)))
  (test-case "binding snapshots values and rejects invalid provider overrides"
   (string-set! prefix 0 #\!)
   (check (parse-artifact-success? (parse-notes "#kept")) => #t)
   (check (rejects? (lambda () (declare-source-language "bad" "v1" "bad.v1" (.cc LineSourceStrategy. 'root-kind "wrong")))) => #t)
   (check (rejects? (lambda () (declare-source-language "bad" "v1" "bad.v1" (.cc LineSourceStrategy. 'provider (lambda (_) #t))))) => #t))))
