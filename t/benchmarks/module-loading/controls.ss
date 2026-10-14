;;; File content changes invalidate source admission without per-parse reload.
(load "t/fixtures/tla-sany-differential/preload.ss")
(prefer-compiled-interfaces!)
(preload-module "gerbil-parser/src/language/module-input")
(preload-module "gerbil-parser/src/ffi/rust-runtime-aot")
(preload-module "gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process")
(import :gerbil/expander
        (only-in :std/io/tempfile make-temporary-file-name)
        (only-in :gerbil-parser/src/language/module-input language-module-descriptor)
        (only-in :gerbil-parser/src/language/descriptor language-grammar-language)
        (only-in :gerbil-parser/src/ffi/rust-runtime-aot rust-runtime-source)
        (only-in :gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process test-child-process-exit!))

(def (require! value message)
  (unless value (error message)))
(def (fails? thunk)
  (with-catch (lambda (_) #t) (lambda () (thunk) #f)))
(def (write-text file text)
  (call-with-output-file file (lambda (port) (display text port))))
(def (grammar-source language)
  (string-append
    "(import (only-in :gerbil-parser/src/language/descriptor make-language-grammar +language-grammar-schema+))\n"
    "(export grammar)\n(def grammar (make-language-grammar +language-grammar-schema+ \""
    language "\" \"v1\" \"loader.test.v1\" #f #f #f #f))\n"))

(let (file (string-append (make-temporary-file-name "parser-loader-") ".ss"))
  (dynamic-wind void
    (lambda ()
      (require! (fails? (lambda () (language-module-descriptor file))) "missing module accepted")
      (write-text file (grammar-source "first"))
      (let (first (language-module-descriptor file))
        (require! (eq? first (language-module-descriptor file)) "module snapshot not reused")
        (write-text file (grammar-source "second"))
        (let (second (language-module-descriptor file))
          (require! (equal? (language-grammar-language second) "second") "content invalidation missed source change")
          (require! (not (eq? first second)) "reloaded descriptor retained stale identity")))
      (write-text file "(export grammar)\n(def grammar (error \"initializer failed\"))\n")
      (require! (fails? (lambda () (language-module-descriptor file))) "initializer failure swallowed")
      (require! (fails? (lambda () (language-module-descriptor file))) "failed snapshot admitted")
      (write-text file (grammar-source "recovered"))
      (require! (equal? (language-grammar-language (language-module-descriptor file)) "recovered")
                "explicit reload failed to recover")
      (require! (fails? (lambda () (language-module-descriptor (string-append file "#missing")))) "missing export accepted")
      (displayln "MODULE-SNAPSHOT-CHANGE-FAILURE-OK"))
    (lambda () (when (file-exists? file) (delete-file file)))))

(def (syntax-source spelling)
  (string-append
    "(import (only-in :gerbil-parser/language-support/grammar deflanguage)\n"
    " (only-in :gerbil-parser/src/language/descriptor language-grammar-with-identity))\n"
    "(export grammar)\n"
    "(deflanguage sourceedit (syntax (lexical (root value) (lex (mark Mark (literals \""
    spelling "\"))))) (rules (value (node Value mark))))\n"
    "(def grammar (language-grammar-with-identity sourceedit-syntax \"source-edit\" \"v1\" \"source-edit.v1\"))\n"))
(let (file (string-append (make-temporary-file-name "parser-source-edit-") ".ss"))
  (dynamic-wind void
    (lambda ()
      (write-text file (syntax-source "!"))
      (let (first (rust-runtime-source file))
        (require! (equal? first (rust-runtime-source file)) "unchanged generation differs")
        (write-text file (syntax-source "?"))
        (let (second (rust-runtime-source file))
          (require! (not (equal? first second)) "same-size grammar edit kept stale Rust")
          (require! (equal? second (rust-runtime-source file reload?: #t)) "automatic and explicit generation differ")))
      (displayln "SOURCE-EDIT-RUST-GENERATION-OK"))
    (lambda () (when (file-exists? file) (delete-file file)))))

;; The official runtime deliberately retains an initializer failure until
;; reload-module! is requested. The preloader must propagate it on every call.
(let* ((directory (make-temporary-file-name "parser-runtime-loader-"))
       (name (path-strip-directory directory))
       (file (string-append directory ".scm"))
       (prior-path (load-path)))
  (dynamic-wind
    (lambda () (add-load-path! (path-directory directory)))
    (lambda ()
      (write-text file "(error \"runtime initializer failed\")\n")
      (require! (fails? (lambda () (preload-runtime-module name))) "runtime failure swallowed")
      (require! (fails? (lambda () (preload-runtime-module name))) "retry skipped failed runtime")
      (write-text file "(define parser-loader-recovered (cons 'loaded '()))\n")
      (require! (fails? (lambda () (preload-runtime-module name))) "official failure cache bypassed")
      (reload-module! name)
      (let (loaded (eval 'parser-loader-recovered))
        (preload-runtime-module name)
        (preload-module name)
        (require! (eq? loaded (eval 'parser-loader-recovered)) "runtime initializer repeated"))
      (displayln "RUNTIME-LOAD-FAILURE-RECOVERY-OK"))
    (lambda () (set-load-path! prior-path) (when (file-exists? file) (delete-file file)))))

(let (file (make-temporary-file-name "parser-preload-source-"))
  (dynamic-wind void
    (lambda ()
      (write-text file "(import (\n")
      (require! (fails? (lambda () (preload-test-imports file))) "malformed source accepted")
      (require! (fails? (lambda () (preload-test-imports file))) "source retry skipped failure")
      (write-text file "(import)\n")
      (preload-test-imports file)
      (displayln "SOURCE-PRELOAD-RETRY-OK"))
    (lambda () (when (file-exists? file) (delete-file file)))))
(displayln "MODULE-LOADING-CONTROLS-OK") (force-output)
(test-child-process-exit! 0)
