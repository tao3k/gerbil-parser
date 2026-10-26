;;; Module evaluation preserves the SDK boundary while exposing actual work.
(import :gerbil-parser/language-test-support :gerbil/expander
        (only-in :std/io/tempfile make-temporary-file-name)
        (only-in ./fixtures/module-loading/source-evaluation
                 with-source-evaluation source-export-value))
(include "fixtures/tla-sany-differential/preload.ss")

(def (evaluation-observation suffix body-values failure-stage
      (id 'gerbil-parser/src/testing/language-source-assertions))
  (let* ((prelude (gx#make-prelude-context #f))
         (module (gx#make-module-context id prelude 'evaluation-control
                   (if (and (equal? suffix ".ssi")
                            (compiled-file (symbol->string id) ".ssi"))
                     (compiled-file (symbol->string id) ".ssi")
                     (string-append "evaluation-control" suffix))))
         (trace '()) (failure (list 'expected-evaluation-failure)))
    (def (visit stage)
      (set! trace (cons stage trace))
      (when (eq? stage failure-stage) (raise failure)))
    (gx#prelude-context-e-set! prelude
      (delay-atomic (begin (visit 'prelude) (values 'ignored 'prelude-values))))
    (gx#module-context-e-set! module
      (delay-atomic (begin (visit 'body) (apply values body-values))))
    (def (evaluate selected) (gx#core-eval-module selected))
    (def (admit name)
      (unless (equal? name (symbol->string id)) (error "wrong runtime identity" name))
      (visit 'native))
    (let (result #f)
      ;; Framework diagnostics use the caller's owned port, never snapshots.
      (call-with-output-string
        (lambda (port)
          (parameterize ((current-output-port port))
            (set! result
              (with-catch
                (lambda (exception)
                  (if (eq? exception failure) 'failed (list 'unexpected exception)))
                (lambda ()
                  (call-with-values
                    (lambda () (evaluate-observed-module module evaluate admit)) list)))))))
      (list (reverse trace) result))))

(def write-evaluation-program (parser-test-datum-writer values))
(def (with-cold-evaluation procedure)
  (let* ((directory (make-temporary-file-name "parser-evaluation-delay-"))
         (prefix (path-strip-directory directory))
         (root (string-append prefix "-root"))
         (left (string-append prefix "-left"))
         (right (string-append prefix "-right"))
         (prior (load-path)))
    (create-directory directory)
    (def (put name suffix program)
      (call-with-output-file (path-expand (string-append name suffix) directory)
        (lambda (port) (write-evaluation-program program port))))
    (try
      ;; Actual cold SDK wrappers: two genuine three-second initializers.
      ;; No timer emits progress; only completed module boundaries report it.
      (put left ".scm" '(thread-sleep! 3))
      (put right ".scm" '(thread-sleep! 3))
      (put root ".scm" `(begin (load-module ,left) (load-module ,right)))
      (put root ".ssi" '(begin))
      (add-load-path! directory)
      (let* ((module (gx#make-module-context (string->symbol root)
                       (gx#core-module->prelude-context (gx#import-module ':gerbil/core)) root
                       (path-expand (string-append root ".ssi") directory))))
        (gx#module-context-e-set! module (delay-atomic (load-module root)))
        (procedure module root left right))
      (finally (set-load-path! prior) (delete-file-or-directory directory #t)))))

(deflanguage-parser-tests test-module-evaluation-test "selected compiled evaluation"
  (engine)
  (scenario "cold chained initialization reports real work within the idle fence"
    (using (with-cold-evaluation) (module root left right)
      (act (evaluate-observed-module module gx#core-eval-module))
      (equal (and (assoc root (list-modules)) #t) #t)
      (equal (and (assoc left (list-modules)) #t) #t)
      (equal (and (assoc right (list-modules)) #t) #t)))
  (scenario "nested SDK source helpers report actual forces and initialize once"
    (using (with-source-evaluation #f) (context)
      (act (evaluate-observed-module context gx#core-eval-module))
      (scope ((first (source-export-value context 'result)))
        (act (evaluate-observed-module context gx#core-eval-module))
        (equal first '((left) (right)))
        (equal (eq? first (source-export-value context 'result)) #t))))
  (scenario "runtime generated imports observe helpers discovered after root installation"
    (using (with-source-evaluation 'dynamic) (context)
      (act (evaluate-observed-module context gx#core-eval-module))
      (equal (source-export-value context 'result) 'dynamic)))
  (scenario "an opaque source prefix fails before any nested helper initializer"
    (using (with-source-evaluation #t) (context)
      (raises (evaluate-observed-module context gx#core-eval-module)
        (lambda (condition) (equal? (error-message condition) "source parent failed first")))))
  (matrix "compiled and source contexts preserve zero and multiple body values"
    (setup) ((suffix '(".ssi" ".ss")) (results '(() (first second))))
    (bindings (expected (if (equal? suffix ".ssi") '(prelude native body) '(prelude body))))
    (equal (evaluation-observation suffix results #f) (list expected results)))
  (matrix "prelude and admission failures prevent later effects and preserve identity"
    (setup) ((row '((prelude (prelude)) (native (prelude native))
                   (body (prelude native body)))))
    (bindings (stage (car row)) (expected (cadr row)))
    (equal (evaluation-observation ".ssi" '(unused) stage) (list expected 'failed)))
  (scenario "installing a template-only source edge does not initialize its body"
    (using (with-source-evaluation 'dormant) (context)
      (act (evaluate-observed-module context gx#core-eval-module))
      (equal (source-export-value context 'result) 'dormant)))
  (property "an interface without a real native wrapper stays under SDK evaluation"
    (bindings)
    (equal (evaluation-observation ".ssi" '(source-only) #f 'missing-evaluation-control)
           '((prelude body) (source-only)))))
