;;; Reporting never replaces SDK lookup, and failed imports remain retryable.
(import :gerbil-parser/language-test-support :gerbil/expander)
(include "fixtures/tla-sany-differential/preload.ss")
(def (interface-report-count output prefix)
  (length (filter (lambda (line) (string-prefix? prefix line))
                  (string-split output #\newline))))
(def (interface-retry-observation)
  (let* ((path (string-append "/parser-interface-" (symbol->string (gensym))))
         (calls 0) (failure (list 'expected)) (value (list 'context))
         (identity? #f) (cleared? #f) (restored? #f))
    (def (importer _path _reload?)
      (set! calls (+ calls 1))
      (if (= calls 3) (raise failure) value))
    (let (output
      (call-with-output-string
        (lambda (port)
          (parameterize ((current-output-port port)
                         (gx#current-expander-module-import importer))
            (call-with-compiled-interface-trace
              (lambda ()
                ((gx#current-expander-module-import) path #f)
                ((gx#current-expander-module-import) path #f)
                (set! identity?
                  (with-catch (lambda (exception) (eq? exception failure))
                    (lambda () ((gx#current-expander-module-import) path #f) #f)))
                (set! cleared? (not (table-ref +reported-interface-imports+ path #f)))
                ((gx#current-expander-module-import) path #f)
                ((gx#current-expander-module-import) path #t)))
            (set! restored? (eq? (gx#current-expander-module-import) importer))))))
      (list calls identity? cleared? restored?
            (interface-report-count output "INTERFACE-IMPORT ")
            (interface-report-count output "INTERFACE-IMPORT-FAILED ")))))
(def (relative-interface-observation path)
  (let ((calls 0))
    (let (output
      (call-with-output-string
        (lambda (port)
          (parameterize ((current-output-port port)
                         (gx#current-expander-module-import
                           (lambda (_path _reload?) (set! calls (+ calls 1)) 'context)))
            (call-with-compiled-interface-trace
              (lambda ()
                ((gx#current-expander-module-import) path #f)
                ((gx#current-expander-module-import) path #f)))))))
      (list calls (interface-report-count output "INTERFACE-IMPORT ")))))

(deflanguage-parser-tests interface-observation-test "SDK interface observation"
  (engine (exclusive #t))
  (property "warm lookups, failure identity, retries and reloads preserve SDK dispatch"
    (bindings)
    (equal (interface-retry-observation) '(5 #t #t #t 3 1)))
  (matrix "relative requests stay visible across caller contexts"
    (setup) ((path '("relative.ss" ./parser (local Helper)))) (bindings)
    (equal (relative-interface-observation path) '(2 2))))
