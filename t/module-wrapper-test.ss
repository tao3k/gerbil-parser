;;; Wrapper extraction must not turn Scheme data or deferred code into effects.
(import :gerbil-parser/language-test-support
        (only-in ./fixtures/module-loading/runtime-files with-runtime-file write-program write-datum))
(include "fixtures/tla-sany-differential/preload.ss")

(def (parent-failure-observation missing?)
  (with-runtime-file
    (lambda (parent parent-name)
      (with-runtime-file
        (lambda (child child-name)
          (write-program parent write-datum `(load-module ,child-name))
          (if missing? (void)
            (write-program child write-datum '(error "closed child failed")))
          (let (failure (with-catch values (lambda () (preload-runtime-module parent-name) #f)))
            (write-program child write-datum '(define parser-wrapper-recovered (list 'ready)))
            (reload-module! child-name)
            (let* ((recovered (eval 'parser-wrapper-recovered))
                   (repeated (with-catch values (lambda () (preload-runtime-module parent-name) #f))))
              (reload-module! parent-name)
              (list (and failure #t) (eq? failure repeated)
                    (eq? recovered (eval 'parser-wrapper-recovered))))))))))

(deflanguage-parser-tests module-wrapper-test "closed SDK loader wrappers"
  (engine)
  (property "closed extraction preserves order and rejects incomplete plans"
    (bindings)
    (equal (compiled-dependencies '(begin (load-module "first") (begin (load-module "second") (begin))))
           '("first" "second"))
    (equal (compiled-dependencies '(load-module "first" extra)) #f)
    (equal (compiled-dependencies '(begin (load-module "first") (quote (load-module "second")))) #f)
    (equal (compiled-dependencies '(declare (invalid-declaration))) #f)
    (equal (compiled-dependencies '(declare (block) (standard-bindings) (extended-bindings))) #f)
    (equal (compiled-dependencies '(declare (block) (standard-bindings) (extended-bindings)) #t) '()))
  (scenario "closed wrapper loads preserve order and SDK initialization count"
    (using (with-runtime-file) (file name)
      (using (with-runtime-file) (first first-name)
        (using (with-runtime-file) (second second-name)
          (act (write-program first write-datum '(define parser-wrapper-order '(first))))
          (act (write-program second write-datum
            '(set! parser-wrapper-order (append parser-wrapper-order '(second)))))
          (act (write-program file write-datum
            `(begin (load-module ,first-name) (begin (load-module ,second-name) (begin)))))
          (act (preload-runtime-module name))
          (equal (eval 'parser-wrapper-order) '(first second))
          (act (preload-runtime-module name))
          (equal (eval 'parser-wrapper-order) '(first second))))))
  (matrix "closed parent failures remain cached after child recovery until SDK reload"
    (setup) ((missing? '(#f #t))) (bindings)
    (equal (parent-failure-observation missing?) '(#t #t #t)))
  (scenario "quoted and deferred loads do not initialize dependencies"
    (using (with-runtime-file) (file name)
      (scope ((missing (string-append name "-missing")))
        (act (write-program file write-datum
          `(begin
             (define parser-wrapper-data '(load-module ,missing))
             (define parser-wrapper-call (lambda () (load-module ,missing))))))
        (act (preload-runtime-module name))
        (equal (eval 'parser-wrapper-data) (list 'load-module missing))
        (matches (eval 'parser-wrapper-call) procedure?)
        (raises ((eval 'parser-wrapper-call)) (lambda (_) #t)))))
  (scenario "opaque code keeps the first SDK execution failure"
    (using (with-runtime-file) (file name)
      (using (with-runtime-file) (dependency dependency-name)
        (act (write-program dependency write-datum '(error "dependency ran too early")))
        (act (write-program file write-datum
          `(begin (error "parent failed first") (load-module ,dependency-name))))
        (raises (preload-runtime-module name)
          (lambda (condition) (equal? (error-message condition) "parent failed first"))))))
  (scenario "SDK source mode requires completed dependencies and restores context"
    (using (with-runtime-file) (file name)
      (scope ((before (__scheme-source)) (missing (string-append name "-not-loaded")))
        (act (write-program file write-datum
          '(begin (declare (block) (standard-bindings) (extended-bindings)) (begin))))
        (equal (sdk-module-ready? missing) #f)
        (raises (load-ready-source-wrapper name file (list missing))
          (lambda (condition) (equal? (error-message condition) "source wrapper dependencies are not settled")))
        (equal (assoc name (list-modules)) #f)
        (equal (__scheme-source) before)
        (act (load-ready-source-wrapper name file '()))
        (matches (sdk-module-ready? name) (lambda (value) value))
        (equal (__scheme-source) before)
        (act (load-ready-source-wrapper name file (list name)))
        (equal (__scheme-source) before))))
  (scenario "SDK failed states remain failures across wrapper requests"
    (using (with-runtime-file) (file name)
      (scope ((before (__scheme-source)))
        (act (write-program file write-datum '(error "wrapper initializer failed")))
        (raises (load-ready-source-wrapper name file '())
          (lambda (condition) (equal? (error-message condition) "wrapper initializer failed")))
        (equal (sdk-module-ready? name) #f)
        (equal (__scheme-source) before)
        (raises (preload-runtime-module name)
          (lambda (condition) (equal? (error-message condition) "wrapper initializer failed")))
        (equal (__scheme-source) before))))
  (scenario "settled replay preserves the first cached failure before a cold suffix"
    (using (with-runtime-file) (parent parent-name)
      (using (with-runtime-file) (child child-name)
        (scope ((before (__scheme-source))
                (missing (string-append parent-name "-unavailable")))
          (act (write-program child write-datum '(error "cached wrapper child")))
          (scope ((failure (with-catch values
                            (lambda () (load-ready-source-wrapper child-name child '()) #f))))
            (equal (settled-source-wrapper? (list child-name missing)) #t)
            (equal (settled-source-wrapper? (list missing child-name)) #f)
            (act (write-program parent write-datum
              `(begin (declare (block) (standard-bindings) (extended-bindings))
                      (load-module ,child-name) (load-module ,missing))))
            (equal (eq? failure (with-catch values
                     (lambda () (load-ready-source-wrapper parent-name parent (list child-name missing)) #f))) #t)
            (equal (eq? failure (with-catch values
                     (lambda () (preload-runtime-module parent-name) #f))) #t)
            (equal (assoc missing (list-modules)) #f)
            (equal (__scheme-source) before))))))
)
