;;; Reporting never replaces SDK lookup, and failed imports remain retryable.
(import :gerbil-parser/language-test-support :gerbil/expander)
(include "fixtures/module-loading/observations.ss")
(def (interface-report-count output prefix)
  (length (filter (lambda (line) (string-prefix? prefix line))
                  (string-split output #\newline))))
(def (interface-retry-observation)
  (let* ((path (string-append "/parser-interface-" (symbol->string (gensym))))
         (calls 0) (failure (list 'expected)) (value (list 'context))
         (identity? #f) (restored? #f))
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
                ((gx#current-expander-module-import) path #f)
                ((gx#current-expander-module-import) path #t)))
            (set! restored? (eq? (gx#current-expander-module-import) importer))))))
      (list calls identity? restored?
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

(def (runtime-resolution-observation)
  (let ((original ##get-module)
        (importer (gx#current-expander-module-import))
        (evaluator (gx#current-expander-module-eval)) (calls 0) (failure (list 'expected))
        (result (list 'module)) (preserved? #f) (restored? #f))
    (def (resolver _id)
      (set! calls (+ calls 1))
      (if (= calls 2) (raise failure) result))
    (dynamic-wind
      (lambda () (##get-module-set! resolver))
      (lambda ()
        (let (output
          (call-with-output-string
            (lambda (port)
              (parameterize ((current-output-port port))
                (set! preserved?
                  (eq? result (call-with-module-evaluation-trace
                                (lambda () (##get-module 'first)))))
                (set! preserved?
                  (and preserved?
                    (with-catch (lambda (condition) (eq? condition failure))
                      (lambda ()
                        (call-with-module-evaluation-trace
                          (lambda () (##get-module 'failed))) #f))))
                (set! restored?
                  (and (eq? resolver ##get-module)
                       (eq? importer (gx#current-expander-module-import))
                       (eq? evaluator (gx#current-expander-module-eval))))))))
          (list calls preserved? restored?
                (interface-report-count output "MODULE-RESOLVE "))))
      (lambda () (##get-module-set! original)))))

(deflanguage-parser-tests interface-observation-test "SDK interface observation"
  (engine)
  (property "warm lookups, failure identity, retries and reloads preserve SDK dispatch"
    (bindings)
    (equal (interface-retry-observation) '(5 #t #t 5 1)))
  (property "composed observers preserve results and restore all hooks after failure"
    (bindings)
    (equal (runtime-resolution-observation) '(2 #t #t 2)))
  (matrix "relative requests stay visible across caller contexts"
    (setup) ((path '("relative.ss" ./parser (local Helper)))) (bindings)
    (equal (relative-interface-observation path) '(2 2))))
