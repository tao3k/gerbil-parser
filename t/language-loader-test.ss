;;; POO language loaders bind effective inherited slots before engine dispatch.
(import :std/test
        (for-syntax (only-in :gerbil/expander core-expand))
        (only-in :clan/poo/object .o .ref .cc object?)
        (only-in :clan/poo/mop define-type element? validate)
        (only-in :core/types PooFlowContract. poo-flow-classification-evidence)
        (only-in :gerbil-parser/src/language/entry
                 deflanguage-loader LanguageLoader. LanguageLoaderContract
                 language-parser-entry-ref)
        (only-in :gerbil-parser/languages/arithmetic/v1/grammar
                 arithmetic-language-grammar)
        (only-in :gerbil-parser/src/runtime/artifact
                 parse-artifact-success? parse-artifact-roundtrip))

(def Dialect.
  (.o (:: @ LanguageLoader.)
      dialect: 'arithmetic
      ;; Identity and engine dispatch from parents must not take control.
      language: "incorrect-parent"
      (.parse (lambda (_) (error "parent parse must never run")))))

(def dialect-admissions 0)

(define-type (DialectContract @ PooFlowContract.)
  identity: 'loader-test/dialect
  .classify: (lambda (candidate context)
               (set! dialect-admissions (+ dialect-admissions 1))
               (let (ok? (eq? (.ref candidate 'dialect) 'arithmetic))
                 (poo-flow-classification-evidence
                  'loader-test/dialect candidate ok?
                  (if ok? '() '((invalid dialect))) context))))

(deflanguage-loader (arithmetic-loader :: self Dialect.)
  (descriptor arithmetic-language-grammar)
  (parse parse-loaded-arithmetic)
  (slots dialect-name: "Arithmetic"
         (summary (string-append (.ref self 'dialect-name) " dialect")))
  (contracts DialectContract))

(def (rejects? thunk)
  (with-catch (lambda (_) #t) (lambda () (thunk) #f)))

(defsyntax (engine-slot-rejection-message stx)
  (def (message form)
    (with-catch (lambda (condition) (error-message condition))
      (lambda () (core-expand form))))
  (datum->syntax
   #'engine-slot-rejection-message
   (string-append
    (message
     #'(deflanguage-loader (invalid-loader @ LanguageLoader.)
         (descriptor #f) (parse invalid-parse)
         (slots (.parse (lambda (_) #f))) (contracts)))
    " | "
    (message
     #'(deflanguage-loader (invalid-keyword-loader @ LanguageLoader.)
         (descriptor #f) (parse invalid-keyword-parse)
         (slots schema: "invalid") (contracts))))))

(def language-loader-test
  (test-suite "POO language loader admission"
    (test-case "inherits extensions and binds engine identity and dispatch"
      (check (object? arithmetic-loader) => #t)
      (check (element? LanguageLoaderContract arithmetic-loader) => #t)
      (check (.ref arithmetic-loader 'dialect) => 'arithmetic)
      (check (.ref arithmetic-loader 'dialect-name) => "Arithmetic")
      (check (.ref arithmetic-loader 'summary) => "Arithmetic dialect")
      (check (language-parser-entry-ref arithmetic-loader 'language) => "arithmetic")
      (check (.ref arithmetic-loader 'capabilities) => '(grammar-ir parser-ir scheme))
      (let* ((source "a + 2 * b") (artifact (parse-loaded-arithmetic source)))
        (check (parse-artifact-success? artifact) => #t)
        (check (parse-artifact-roundtrip artifact) => source)))
    (test-case "parsing reuses the loaded entry without extension admission"
      (let (before dialect-admissions)
        (for-each
         (lambda (source)
           (check (parse-artifact-success? (parse-loaded-arithmetic source)) => #t))
         '("1" "a + b" "a * (b + 2)"))
        (check dialect-admissions => before)))
    (test-case "rejects a missing descriptor and inconsistent identity"
      (check (element? LanguageLoaderContract LanguageLoader.) => #f)
      (check (rejects? (lambda () (validate LanguageLoaderContract LanguageLoader.))) => #t)
      (check (element? LanguageLoaderContract (.cc arithmetic-loader 'language "bash")) => #f)
      (check (element? LanguageLoaderContract (.cc arithmetic-loader 'descriptor #f)) => #f))
    (test-case "rejects engine slot overrides during macro expansion"
      (check (engine-slot-rejection-message)
             => "language loader extension overrides an engine slot | language loader extension overrides an engine slot"))
    (test-case "extension contracts validate the effective slot object"
      (check
       (rejects?
        (lambda ()
          (deflanguage-loader (invalid-dialect @ Dialect.)
            (descriptor arithmetic-language-grammar)
            (parse parse-invalid-dialect)
            (slots (dialect 'other))
            (contracts DialectContract))
          invalid-dialect)) => #t)
      (check (element? DialectContract arithmetic-loader) => #t)
      (check (rejects? (lambda () (validate DialectContract (.cc arithmetic-loader 'dialect 'other)))) => #t))))

(export language-loader-test)
