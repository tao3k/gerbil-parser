;;; Official module promises, source invalidation and failed initializer recovery.
(import :gerbil-parser/language-test-support :gerbil/expander
        (only-in ./fixtures/module-loading/runtime-files
                 with-module-file with-runtime-file write-program write-datum)
        (only-in :gerbil-parser/src/language/module-input language-module-descriptor)
        (only-in :gerbil-parser/src/language/descriptor language-grammar-language)
        (only-in :gerbil-parser/src/ffi/rust-runtime-aot rust-runtime-source)
        (only-in :yuanxu/module-system/source/admission
                 call-with-module-source-read-cache module-source-read))
(include "fixtures/module-loading/observations.ss")

(def write-grammar
  (parser-test-datum-writer
    (lambda (language)
      `(begin
         (import (only-in :gerbil-parser/src/language/descriptor
                         make-language-grammar +language-grammar-schema+))
         (export grammar)
         (def grammar (make-language-grammar +language-grammar-schema+
                       ,language "v1" "loader.test.v1" #f #f #f #f))))))
(def write-syntax
  (parser-test-datum-writer
    (lambda (spelling)
      `(begin
         (import (only-in :gerbil-parser/language-support/grammar deflanguage)
                 (only-in :gerbil-parser/src/language/descriptor language-grammar-with-identity))
         (export grammar)
         (deflanguage sourceedit
           (syntax (lexical (root value) (lex (mark Mark (literals ,spelling)))))
           (rules (value (node Value mark))))
         (def grammar (language-grammar-with-identity
                       sourceedit-syntax "source-edit" "v1" "source-edit.v1"))))))
(def (write-malformed-import file)
  (call-with-output-file file (lambda (port) (write-string "(import (\n" port))))
(def (failed? thunk)
  (with-catch (lambda (_) #t) (lambda () (thunk) #f)))

(deflanguage-parser-tests module-loading-test "official module loading"
  (engine)
  (scenario "source snapshots reject changes and failures and recover the last content"
    (using (with-module-file ".ss") (file)
      (raises (language-module-descriptor file) (lambda (_) #t))
      (act (write-program file write-grammar "first"))
      (scope ((first (language-module-descriptor file)))
        (equal (eq? first (language-module-descriptor file)) #t)
        (act (write-program file write-grammar "second"))
        (scope ((second (language-module-descriptor file)))
          (equal (language-grammar-language second) "second")
          (equal (eq? first second) #f)))
      (act (write-program file write-datum
             '(begin (export grammar) (def grammar (error "initializer failed")))))
      (each (_attempt) ('(first retry))
        (raises (language-module-descriptor file)
                (lambda (condition) (equal? (error-message condition) "initializer failed"))))
      (act (write-program file write-grammar "second"))
      (equal (language-grammar-language (language-module-descriptor file)) "second")
      (act (write-program file write-grammar "recovered"))
      (equal (language-grammar-language (language-module-descriptor file)) "recovered")
      (raises (language-module-descriptor (string-append file "#missing")) (lambda (_) #t))))
  (scenario "Core rejects a descriptor whose initializer rewrites its input"
    (using (with-module-file ".ss") (file)
      (scope ((replacement
        '(begin
           (import (only-in :gerbil-parser/src/language/descriptor
                           make-language-grammar +language-grammar-schema+))
           (export grammar)
           (def grammar (make-language-grammar +language-grammar-schema+
                         "after" "v1" "loader.test.v1" #f #f #f #f)))))
        (act (write-program file write-datum
          `(begin
             (import (only-in :gerbil-parser/src/language/descriptor
                             make-language-grammar +language-grammar-schema+))
             (export grammar)
             (call-with-output-file ,file (lambda (port) (write ',replacement port)))
             (def grammar (make-language-grammar +language-grammar-schema+
                           "before" "v1" "loader.test.v1" #f #f #f #f)))))
        (raises
          (call-with-module-source-read-cache
            (lambda () (module-source-read file) (language-module-descriptor file)))
          (lambda (_) #t))
        (equal (language-grammar-language (language-module-descriptor file)) "after"))))
  (scenario "source initializer failures propagate and changed source recovers"
    (using (with-module-file ".ss") (file)
      (act (write-program file write-datum '(begin (export grammar) (error "one native failure"))))
      ;; Source evaluation follows the native expander, which may retry a failed
      ;; source promise. Runtime load-module error identity is checked separately.
      (each (_attempt) ('(first retry))
        (raises (language-module-descriptor file)
          (lambda (condition) (equal? (error-message condition) "one native failure"))))
      (act (write-program file write-grammar "repaired"))
      (equal (language-grammar-language (language-module-descriptor file)) "repaired")))
  (scenario "same-size DSL edits invalidate Rust generation"
    (using (with-module-file ".ss") (file)
      (act (write-program file write-syntax "!"))
      (scope ((first (rust-runtime-source file)))
        (equal first (rust-runtime-source file))
        (act (write-program file write-syntax "?"))
        (scope ((second (rust-runtime-source file)))
          (equal (equal? first second) #f)
          (equal second (rust-runtime-source file reload?: #t))))))
  (scenario "runtime failures remain cached until official reload and initialize once"
    (using (with-runtime-file) (file name)
      (act (write-program file write-datum '(error "runtime initializer failed")))
      (each (_attempt) ('(first retry))
        (raises (preload-runtime-module name)
                (lambda (condition) (equal? (error-message condition) "runtime initializer failed"))))
      (act (write-program file write-datum '(define parser-loader-recovered (cons 'loaded '()))))
      (raises (preload-runtime-module name)
              (lambda (condition) (equal? (error-message condition) "runtime initializer failed")))
      (act (reload-module! name))
      (scope ((loaded (eval 'parser-loader-recovered)))
        (act (preload-runtime-module name))
        (act (load-module name))
        (equal (eq? loaded (eval 'parser-loader-recovered)) #t))))
  (scenario "source-only namespace imports traverse dependencies and retry failures"
    (using (with-module-file ".ss") (file)
      (scope ((previous (load-path))
              (name (string->symbol
                      (string-append ":" (path-strip-extension (path-strip-directory file))))))
        (protect
          (body
            (group
              (act (add-load-path! (path-directory file)))
              (act (write-malformed-import file))
              (each (_attempt) ('(first retry))
                (raises (import-module name #t #f) (lambda (_) #t)))
              (act (write-program file write-datum '(import)))
              (equal (failed? (lambda () (import-module name #t #f))) #f)))
          (cleanup (act (set-load-path! previous)))))))
  (scenario "native source import recovers with explicit reload"
    (using (with-module-file ".ss") (file)
      (act (write-malformed-import file))
      (each (_attempt) ('(first retry))
        (raises (import-module file #t #f) (lambda (_) #t)))
      (act (write-program file write-datum '(import)))
      (equal (failed? (lambda () (import-module file #t #f))) #f))))
