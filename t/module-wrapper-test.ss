;;; Wrapper extraction must not turn Scheme data or deferred code into effects.
(import :gerbil-parser/language-test-support
        (only-in ./fixtures/module-loading/runtime-files with-runtime-file write-program write-datum))
(import (only-in :gerbil/runtime/init __scheme-source))

(def (parent-failure-observation missing?)
  (with-runtime-file
    (lambda (parent parent-name)
      (with-runtime-file
        (lambda (child child-name)
          (write-program parent write-datum `(load-module ,child-name))
          (if missing? (void)
            (write-program child write-datum '(error "closed child failed")))
          (let (failure (with-catch values (lambda () (load-module parent-name) #f)))
            (write-program child write-datum '(define parser-wrapper-recovered (list 'ready)))
            (reload-module! child-name)
            (let* ((recovered (eval 'parser-wrapper-recovered))
                   (repeated (with-catch values (lambda () (load-module parent-name) #f))))
              (reload-module! parent-name)
              (list (and failure #t) (eq? failure repeated)
                    (eq? recovered (eval 'parser-wrapper-recovered))))))))))

(deflanguage-parser-tests module-wrapper-test "native loader order and error ownership"
  (engine)
  (scenario "closed wrapper loads preserve order and SDK initialization count"
    (using (with-runtime-file) (file name)
      (using (with-runtime-file) (first first-name)
        (using (with-runtime-file) (second second-name)
          (act (write-program first write-datum '(define parser-wrapper-order '(first))))
          (act (write-program second write-datum
            '(set! parser-wrapper-order (append parser-wrapper-order '(second)))))
          (act (write-program file write-datum
            `(begin (load-module ,first-name) (begin (load-module ,second-name) (begin)))))
          (act (load-module name))
          (equal (eval 'parser-wrapper-order) '(first second))
          (act (load-module name))
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
        (act (load-module name))
        (equal (eval 'parser-wrapper-data) (list 'load-module missing))
        (matches (eval 'parser-wrapper-call) procedure?)
        (raises ((eval 'parser-wrapper-call)) (lambda (_) #t)))))
  (scenario "opaque code keeps the first SDK execution failure"
    (using (with-runtime-file) (file name)
      (using (with-runtime-file) (dependency dependency-name)
        (act (write-program dependency write-datum '(error "dependency ran too early")))
        (act (write-program file write-datum
          `(begin (error "parent failed first") (load-module ,dependency-name))))
        (raises (load-module name)
          (lambda (condition) (equal? (error-message condition) "parent failed first"))))))
  (scenario "official raw Scheme loading restores context on success and failure"
    (using (with-runtime-file) (file name)
      (scope ((before (__scheme-source)))
        (act (write-program file write-datum
          '(begin (declare (block) (standard-bindings) (extended-bindings)) (begin))))
        (act (load-scheme file))
        (equal (__scheme-source) before)
        (act (write-program file write-datum '(error "native raw Scheme failure")))
        (raises (load-scheme file)
          (lambda (condition) (equal? (error-message condition) "native raw Scheme failure")))
        (equal (__scheme-source) before))))
  (scenario "native failed states remain the same exception until explicit reload"
    (using (with-runtime-file) (file name)
      (act (write-program file write-datum '(error "wrapper initializer failed")))
      (scope ((failure (with-catch values (lambda () (load-module name) #f))))
        (equal (and failure #t) #t)
        (equal (eq? failure (with-catch values (lambda () (load-module name) #f))) #t)
        (act (write-program file write-datum '(begin)))
        (equal (eq? failure (with-catch values (lambda () (load-module name) #f))) #t)
        (act (reload-module! name))
        (matches (cdr (assoc name (list-modules))) string?))))
  (scenario "the first cached child failure prevents a cold suffix from running"
    (using (with-runtime-file) (parent parent-name)
      (using (with-runtime-file) (child child-name)
        (scope ((missing (string-append parent-name "-unavailable")))
          (act (write-program child write-datum '(error "cached wrapper child")))
          (scope ((failure (with-catch values (lambda () (load-module child-name) #f))))
            (act (write-program parent write-datum
              `(begin (load-module ,child-name) (load-module ,missing))))
            (equal (eq? failure (with-catch values (lambda () (load-module parent-name) #f))) #t)
            (equal (eq? failure (with-catch values (lambda () (load-module parent-name) #f))) #t)
            (equal (assoc missing (list-modules)) #f)))))))
