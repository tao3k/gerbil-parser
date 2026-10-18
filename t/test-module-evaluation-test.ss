;;; Module evaluation preserves the SDK boundary while exposing actual work.
(import :gerbil-parser/language-test-support :gerbil/expander
        (only-in :std/io/tempfile make-temporary-file-name))
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
    (def (evaluate selected)
      (if (eq? selected prelude)
        (begin (visit 'prelude) (values 'ignored 'prelude-values))
        (begin
          (unless (eq? selected module) (error "unexpected evaluation context" selected))
          (visit 'body)
          (apply values body-values))))
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

(def (selected-import-observation phase source-phase)
  (def (native id)
    (gx#make-module-context id #f 'import-control
      (compiled-file (symbol->string id) ".ssi")))
  (let* ((root (gx#make-module-context 'import-control #f 'import-control "import-control.ss"))
         (source (native 'gerbil-parser/src/testing/language-source-assertions))
         (tokens (native 'gerbil-parser/src/runtime/token))
         (syntax (native 'gerbil-parser/src/testing/language-parser))
         (contracts (native 'gerbil-parser/src/testing/contracts))
         (overlay (gx#make-module-context 'gerbil-parser/src/testing/language-source-assertions
                    #f 'import-control "overlay.ss"))
         (foreign (gx#make-module-context 'gerbil-parser/src/testing/language-source-assertions
                    #f 'import-control "foreign.ssi")))
    (gx#module-context-import-set! root
      (reverse
        (list (gx#make-import-set source 0 '())
              (gx#make-import-set tokens phase '())
              (gx#make-import-set overlay 0 '())
              (gx#make-import-set foreign 0 '())
              (gx#make-module-import
                (gx#make-module-export syntax 'check source-phase 'check #f) 'check 0 #f)
              contracts)))
    (module-evaluation-runtime-imports root)))

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
      (let* ((module (gx#make-module-context (string->symbol root) #f root
                       (path-expand (string-append root ".ssi") directory))))
        (gx#module-context-e-set! module (delay-atomic (load-module root)))
        (procedure module root left right))
      (finally (set-load-path! prior) (delete-file-or-directory directory #t)))))

(deflanguage-parser-tests test-module-evaluation-test "selected compiled evaluation"
  (engine (exclusive #t))
  (scenario "cold chained initialization reports real work within the idle fence"
    (using (with-cold-evaluation) (module root left right)
      (act (evaluate-observed-module module gx#core-eval-module))
      (equal (and (assoc root (list-modules)) #t) #t)
      (equal (and (assoc left (list-modules)) #t) #t)
      (equal (and (assoc right (list-modules)) #t) #t)))
  (matrix "compiled and source contexts preserve zero and multiple body values"
    (setup) ((suffix '(".ssi" ".ss")) (results '(() (first second))))
    (bindings (expected (if (equal? suffix ".ssi") '(prelude native body) '(body))))
    (equal (evaluation-observation suffix results #f) (list expected results)))
  (matrix "prelude and admission failures prevent later effects and preserve identity"
    (setup) ((row '((prelude (prelude)) (native (prelude native))
                   (body (prelude native body)))))
    (bindings (stage (car row)) (expected (cadr row)))
    (equal (evaluation-observation ".ssi" '(unused) stage) (list expected 'failed)))
  (matrix "expanded imports retain source order, runtime phases and physical ownership"
    (setup) ((phase '(-1 0 1 2)) (source-phase '(-1 0 1)))
    (bindings (expected
                (append '("gerbil-parser/src/testing/language-source-assertions")
                        (if (zero? phase) '("gerbil-parser/src/runtime/token") '())
                        (if (zero? source-phase) '("gerbil-parser/src/testing/language-parser") '())
                        '("gerbil-parser/src/testing/contracts"))))
    (equal (selected-import-observation phase source-phase) expected))
  (property "an interface without a real native wrapper stays under SDK evaluation"
    (bindings)
    (equal (evaluation-observation ".ssi" '(source-only) #f 'missing-evaluation-control)
           '((body) (source-only)))))
